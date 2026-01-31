using System;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal class TypeResolver(Compilation compilation)
{
    private readonly Dictionary<Type, ITypeSymbol> _cache = new();
    private readonly NullabilityInfoContext _nullabilityContext = new();
    private readonly Dictionary<MethodBase, PEMethodSymbol> _methodSymbols = new();
    private readonly Dictionary<(PEMethodSymbol method, Type parameter), ITypeParameterSymbol> _methodTypeParameters = new();

    public ITypeSymbol? ResolveType(ParameterInfo parameterInfo)
    {
        var methodContext = parameterInfo.Member as MethodBase;
        var parameterType = parameterInfo.ParameterType;

        var attributes = parameterInfo.GetCustomAttributesData();
        var unionAttribute = attributes
            .FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");

        if (parameterType.IsByRef)
        {
            var elementType = ResolveType(parameterType.GetElementType()!, methodContext);
            if (elementType is null)
                return null;

            if (unionAttribute is not null)
                return CreateTypeUnionSymbol(unionAttribute, elementType);

            var nullInfo = _nullabilityContext.Create(parameterInfo);
            if (nullInfo.ElementType is not null)
                elementType = ApplyNullability(elementType, nullInfo.ElementType);

            return new ByRefTypeSymbol(elementType);
        }

        var declaredType = ResolveType(parameterType, methodContext);

        if (unionAttribute is not null)
        {
            return CreateTypeUnionSymbol(unionAttribute, declaredType);
        }

        var type = declaredType;

        if (type is ITypeParameterSymbol typeParameterSymbol)
            return type;

        var parameterNullInfo = _nullabilityContext.Create(parameterInfo);
        return ApplyNullability(type!, parameterNullInfo);
    }

    public ITypeSymbol? ResolveType(FieldInfo fieldInfo)
    {
        if (TryGetUnion(fieldInfo, out var unionType))
        {
            return unionType;
        }

        var type = ResolveType(fieldInfo.FieldType);

        if (type is ITypeParameterSymbol typeParameterSymbol)
            return type;

        var nullInfo = _nullabilityContext.Create(fieldInfo);
        return ApplyNullability(type!, nullInfo);
    }

    public ITypeSymbol? ResolveType(PropertyInfo propertyInfo)
    {
        if (TryGetUnion(propertyInfo, out var unionType))
        {
            return unionType;
        }

        var type = ResolveType(propertyInfo.PropertyType);

        if (type is ITypeParameterSymbol typeParameterSymbol)
            return type;

        var nullInfo = _nullabilityContext.Create(propertyInfo);
        return ApplyNullability(type!, nullInfo);
    }

    public ITypeSymbol? ResolveType(EventInfo eventInfo)
    {
        var handlerType = eventInfo.EventHandlerType;
        if (handlerType is null)
            return null;

        var type = ResolveType(handlerType);

        if (type is ITypeParameterSymbol typeParameterSymbol)
            return type;

        var nullInfo = _nullabilityContext.Create(eventInfo);
        return ApplyNullability(type!, nullInfo);
    }

    public FieldInfo? ResolveRuntimeField(FieldInfo fieldInfo)
    {
        if (fieldInfo is null)
            throw new ArgumentNullException(nameof(fieldInfo));

        var declaringType = fieldInfo.DeclaringType?.GetTypeInfo();
        if (declaringType is null)
            return null;

        var runtimeType = compilation.ResolveRuntimeType(declaringType);
        if (runtimeType is null)
            return null;

        var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic |
                           (fieldInfo.IsStatic ? BindingFlags.Static : BindingFlags.Instance);

        return runtimeType.GetField(fieldInfo.Name, bindingFlags);
    }

    private bool TryGetUnion(MemberInfo memberInfo, out ITypeUnionSymbol? unionType)
    {
        unionType = null;
        var unionAttribute = memberInfo.GetCustomAttributesData()
            .FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");

        if (unionAttribute is not null)
        {
            ITypeSymbol? declaredType = memberInfo switch
            {
                PropertyInfo propertyInfo => ResolveType(propertyInfo.PropertyType),
                FieldInfo fieldInfo => ResolveType(fieldInfo.FieldType),
                EventInfo eventInfo => ResolveType(eventInfo.EventHandlerType!),
                MethodInfo methodInfo => ResolveType(methodInfo.ReturnType, methodInfo),
                _ => null
            };

            unionType = CreateTypeUnionSymbol(unionAttribute, declaredType);
        }
        return unionType is not null;
    }

    private ITypeUnionSymbol CreateTypeUnionSymbol(CustomAttributeData unionAttribute, ITypeSymbol? declaredUnderlyingType)
    {
        var args = (IEnumerable<CustomAttributeTypedArgument>)unionAttribute
            .ConstructorArguments.First().Value!;

        var types = new List<ITypeSymbol>();
        foreach (var arg in args)
        {
            if (arg.Value is Type t)
            {
                types.Add(ResolveType(t)!);
            }
            else
            {
                var underlying = ResolveType(arg.ArgumentType)!;
                types.Add(new LiteralTypeSymbol(underlying, arg.Value!, compilation));
            }
        }

        return new TypeUnionSymbol(types.ToArray(), null, null, null, [], declaredUnderlyingType);
    }

    public void RegisterMethodSymbol(MethodBase method, PEMethodSymbol symbol)
    {
        _methodSymbols[method] = symbol;
    }

    public ITypeSymbol? ResolveType(Type type)
        => ResolveType(type, null);

    public ITypeSymbol? ResolveType(Type type, MethodBase? methodContext)
    {
        if (_cache.TryGetValue(type, out var cached))
            return cached;

        if (type.Name == "Void")
        {
            var unit = compilation.GetSpecialType(SpecialType.System_Unit);
            _cache[type] = unit;
            return unit;
        }

        if (type.Name == "Null")
            return compilation.NullTypeSymbol;

        var nullable = compilation.GetSpecialType(SpecialType.System_Nullable_T);

        if (type.IsGenericType
            && type.GetGenericTypeDefinition().FullName.Contains("System.Nullable`1"))
        {
            var underlying = ResolveType(type.GetGenericArguments()[0]);
            return underlying!.MakeNullable();
        }

        // TODO: Return immediately if built in type

        if (type.IsPointer)
        {
            var element = ResolveType(type.GetElementType()!, methodContext);
            if (element is null)
                return null;

            var pointer = new PointerTypeSymbol(element);
            _cache[type] = pointer;
            return pointer;
        }

        if (type.IsGenericTypeDefinition)
        {
            return ResolveTypeCore(type);
        }

        if (type.IsGenericType)
        {
            // Important:
            // - For *nested* types, reflection may report generic arguments that include the declaring type's
            //   arguments. The correct way to resolve these is to rebuild the declaring-type chain and
            //   anchor the nested type under the constructed declaring type.
            // - For *non-nested* generic types (e.g. TaskAwaiter<TResult>), we can resolve the generic
            //   definition and construct it directly.

            if (type.IsNested)
                return ResolveNestedTypeChain(type, methodContext);

            var genericTypeDefinition = (INamedTypeSymbol?)ResolveType(type.GetGenericTypeDefinition(), methodContext);
            if (genericTypeDefinition is null)
                return null;

            var args = type.GetGenericArguments().Select(x => ResolveType(x, methodContext)!).ToArray();
            return genericTypeDefinition.Construct(args);
        }

        if (type.IsGenericTypeParameter || type.IsGenericMethodParameter)
        {
            if (ResolveType(type.DeclaringType) is not INamedTypeSymbol declaringNamedType)
                throw new InvalidOperationException($"Could not resolve declaring type for type parameter: {type}");

            if (type.IsGenericMethodParameter)
            {
                var method = methodContext ?? type.DeclaringMethod;
                if (method is null)
                    throw new InvalidOperationException($"Unable to resolve declaring method for type parameter: {type}");

                if (!_methodSymbols.TryGetValue(method, out var methodSymbol))
                    throw new InvalidOperationException($"Method symbol not registered for {method}.");

                return ResolveMethodTypeParameter(type, methodSymbol);
            }

            return new PETypeParameterSymbol(type, declaringNamedType, declaringNamedType, declaringNamedType.ContainingNamespace, [], this);
        }

        if (type.IsArray)
        {
            var elementType = ResolveType(type.GetElementType());
            return new ArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Array), elementType, null, null, null, []);
        }

        var symbol = ResolveTypeCore(type);

        _cache[type] = symbol!;

        return symbol;
    }

    private INamedTypeSymbol ResolveNestedTypeChain(Type t, MethodBase? methodContext)
    {
        // 1) chain outermost..innermost
        var chain = new List<Type>();
        for (var cur = t; cur != null; cur = cur.DeclaringType)
            chain.Add(cur);
        chain.Reverse();

        // 2) declared arity per level
        var arities = chain.Select(GetDeclaredArity).ToArray();

        // 3) flat args for *t* (innermost)
        var flat = t.IsGenericType || t.ContainsGenericParameters
            ? t.GetGenericArguments()
            : Type.EmptyTypes;

        // 4) slice args per level
        var slices = SliceArguments(flat, arities); // returns Type[][]

        // 5) resolve outermost and walk down
        var resolvedOuter = ResolveType(chain[0], methodContext);
        if (resolvedOuter is not INamedTypeSymbol outerNamed)
            return compilation.ErrorTypeSymbol;

        INamedTypeSymbol current = outerNamed;

        if (arities[0] > 0)
            current = (INamedTypeSymbol)current.Construct(slices[0].Select(x => ResolveType(x, methodContext)!).ToArray());

        for (int i = 1; i < chain.Count; i++)
        {
            var levelType = chain[i];

            // Find nested under current containing symbol
            var nestedDef = FindNested(current, levelType);
            if (nestedDef is null)
                return compilation.ErrorTypeSymbol;

            current = nestedDef;

            if (arities[i] > 0)
                current = (INamedTypeSymbol)current.Construct(slices[i].Select(x => ResolveType(x, methodContext)!).ToArray());
        }

        return current;
    }

    private static int GetDeclaredArity(Type level)
    {
        if (!level.IsGenericType && !level.IsGenericTypeDefinition)
            return 0;

        var def = level.IsGenericTypeDefinition ? level : level.GetGenericTypeDefinition();
        return def.GetGenericArguments().Length;
    }

    private static Type[][] SliceArguments(Type[] flat, int[] arities)
    {
        var result = new Type[arities.Length][];
        int pos = 0;
        for (int i = 0; i < arities.Length; i++)
        {
            var n = arities[i];
            if (n == 0) { result[i] = Array.Empty<Type>(); continue; }
            result[i] = flat.Skip(pos).Take(n).ToArray();
            pos += n;
        }
        return result;
    }

    private static INamedTypeSymbol? FindNested(INamedTypeSymbol containing, Type nestedRuntimeType)
    {
        // Use Name+Arity, *not* just Name.
        var (name, arity) = GetRuntimeNestedNameAndArity(nestedRuntimeType);
        var runtimeMetadataName = nestedRuntimeType.Name;

        var candidates = containing.GetMembers().OfType<INamedTypeSymbol>();
        INamedTypeSymbol? nameMatch = null;
        foreach (var candidate in candidates)
        {
            if (string.Equals(candidate.MetadataName, runtimeMetadataName, StringComparison.Ordinal))
                return candidate;

            if (string.Equals(candidate.Name, name, StringComparison.Ordinal) && candidate.Arity == arity)
                nameMatch ??= candidate;
        }

        return nameMatch;
    }

    private static (string name, int arity) GetRuntimeNestedNameAndArity(Type t)
    {
        // Reflection reports nested generic type parameters as the concatenation of:
        //   declaring type parameters + nested type's own parameters.
        //
        // Example (no nested parameters declared in source):
        //   Result<T, E>.Ok  -> runtime nested type may appear as Ok`2 with [T, E]
        //   but the nested type's *own* arity in the source model is 0.
        //
        // Example (nested declares its own params):
        //   Outer<T>.Inner<U> -> runtime Inner may appear with arity 2 total, but nested-own arity is 1.

        var name = t.Name;
        var tick = name.IndexOf('`');
        if (tick >= 0)
            name = name[..tick];

        // Compute nested-own arity by subtracting declaring arity from total generic arguments.
        // For non-generic or non-nested types, this degenerates safely.
        var total = (t.IsGenericType || t.IsGenericTypeDefinition)
            ? (t.IsGenericTypeDefinition ? t.GetGenericArguments().Length : t.GetGenericArguments().Length)
            : 0;

        var declaring = t.DeclaringType;
        var declaringTotal = 0;
        if (declaring is not null && (declaring.IsGenericType || declaring.IsGenericTypeDefinition))
        {
            declaringTotal = declaring.IsGenericTypeDefinition
                ? declaring.GetGenericArguments().Length
                : declaring.GetGenericArguments().Length;
        }

        var nestedOwnArity = Math.Max(0, total - declaringTotal);
        return (name, nestedOwnArity);
    }

    internal ITypeParameterSymbol ResolveMethodTypeParameter(Type type, PEMethodSymbol methodSymbol)
    {
        var key = (methodSymbol, type);

        if (_methodTypeParameters.TryGetValue(key, out var existing))
            return existing;

        var symbol = new PETypeParameterSymbol(type, methodSymbol, methodSymbol.ContainingType, methodSymbol.ContainingNamespace, [new MetadataLocation(methodSymbol.ContainingModule!)], this);
        _methodTypeParameters[key] = symbol;
        return symbol;
    }

    private ITypeSymbol ApplyNullability(ITypeSymbol typeSymbol, System.Reflection.NullabilityInfo nullInfo)
    {
        if (typeSymbol is IArrayTypeSymbol array && nullInfo.ElementType is not null)
        {
            var element = ApplyNullability(array.ElementType, nullInfo.ElementType);
            if (!ReferenceEquals(element, array.ElementType))
                typeSymbol = compilation.CreateArrayTypeSymbol(element);
        }
        else if (typeSymbol is INamedTypeSymbol named && nullInfo.GenericTypeArguments.Length > 0)
        {
            var typeArgs = named.TypeArguments.ToArray();
            var changed = false;
            var len = Math.Min(typeArgs.Length, nullInfo.GenericTypeArguments.Length);
            for (int i = 0; i < len; i++)
            {
                var newArg = ApplyNullability(typeArgs[i], nullInfo.GenericTypeArguments[i]);
                if (!ReferenceEquals(newArg, typeArgs[i]))
                {
                    typeArgs[i] = newArg;
                    changed = true;
                }
            }

            if (changed)
                typeSymbol = named.Construct(typeArgs);
        }

        if (nullInfo.ReadState == NullabilityState.Nullable
            && typeSymbol is not NullableTypeSymbol
            && !typeSymbol.IsValueType
            && typeSymbol is not ITypeParameterSymbol)
        {
            typeSymbol = typeSymbol.MakeNullable();
        }

        return typeSymbol;
    }

    protected ITypeSymbol ResolveTypeCore(Type type)
    {
        var typeInfo = type.GetTypeInfo();
        var metadataName = GetMetadataName(typeInfo);
        if (string.IsNullOrEmpty(metadataName))
            return compilation.ErrorTypeSymbol;

        var assemblyName = type.Assembly.GetName().Name;
        var corLibrary = (PEAssemblySymbol)compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly;
        var assemblySymbol = (PEAssemblySymbol?)compilation.ReferencedAssemblySymbols.FirstOrDefault(x => x.Name == assemblyName)
            ?? corLibrary;

        if (assemblySymbol is null)
            return compilation.GetTypeByMetadataName(metadataName) ?? compilation.ErrorTypeSymbol;

        return (ITypeSymbol?)assemblySymbol.PrimaryModule.ResolveMetadataMember(assemblySymbol.GlobalNamespace, metadataName)
            ?? compilation.GetTypeByMetadataName(metadataName)
            ?? compilation.ErrorTypeSymbol;
    }

    private static string? GetMetadataName(Type type)
    {
        if (type.FullName is { } fullName)
            return fullName;

        var name = type.Name;

        if (type.DeclaringType is { } declaringType)
        {
            var declaringName = GetMetadataName(declaringType);
            return declaringName is null ? null : $"{declaringName}+{name}";
        }

        return string.IsNullOrEmpty(type.Namespace)
            ? name
            : $"{type.Namespace}.{name}";
    }

    public IMethodSymbol? ResolveMethodSymbol(MethodInfo ifaceMethod)
    {
        var type = ResolveType(ifaceMethod.DeclaringType!);

        if (type is null) return null;
        return type.GetMembers()
            .OfType<IMethodSymbol>()
            // TODO: Better condition filtering
            .FirstOrDefault(x => x.Name == ifaceMethod.Name);
    }

    internal IPropertySymbol? ResolvePropertySymbol(PropertyInfo ifaceProp)
    {
        var type = ResolveType(ifaceProp.DeclaringType!);

        if (type is null) return null;
        return type.GetMembers()
            .OfType<IPropertySymbol>()
            // TODO: Better condition for filtering
            .FirstOrDefault(x => x.Name == ifaceProp.Name);
    }

    internal IEventSymbol? ResolveEventSymbol(EventInfo ifaceEvent)
    {
        var type = ResolveType(ifaceEvent.DeclaringType!);

        if (type is null) return null;
        return type.GetMembers()
            .OfType<IEventSymbol>()
            // TODO: Better condition for filtering
            .FirstOrDefault(x => x.Name == ifaceEvent.Name);
    }
}
