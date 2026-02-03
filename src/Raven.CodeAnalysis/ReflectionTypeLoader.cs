using System.Collections.Generic;
using System.Linq;
using System;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal class ReflectionTypeLoader(Compilation compilation)
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

        if (type.IsNullableValueType())
        {
            var underlying = ResolveType(type.GetGenericArguments()[0], methodContext);
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

            if (type.RequiresNestedChainResolution())
                return ResolveNestedTypeChain(type, methodContext);

            var genericTypeDefinition = (INamedTypeSymbol?)ResolveType(type.GetGenericTypeDefinition(), methodContext);
            if (genericTypeDefinition is null)
                return null;

            var args = type.GetGenericArguments().Select(x => ResolveType(x, methodContext)!).ToArray();
            return genericTypeDefinition.Construct(args);
        }

        if (type.IsGenericTypeParameter || type.IsGenericMethodParameter)
        {
            if (ResolveType(type.DeclaringType!) is not INamedTypeSymbol declaringNamedType)
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

            return new PETypeParameterSymbol(type, declaringNamedType, declaringNamedType, declaringNamedType.ContainingNamespace, [], this).AddAsMember2();
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
        var chain = t.DeclaringTypeChain();

        // 2) slice args per level (declared arity per level)
        var slices = t.SliceGenericArgumentsPerLevel(chain);

        // 3) resolve outermost and walk down
        var resolvedOuter = ResolveType(chain[0], methodContext);
        if (resolvedOuter is not INamedTypeSymbol outerNamed)
            return (INamedTypeSymbol)compilation.ErrorTypeSymbol;

        INamedTypeSymbol current = outerNamed;

        if (slices[0].Length > 0)
            current = (INamedTypeSymbol)current.Construct(slices[0].Select(x => ResolveType(x, methodContext)!).ToArray());

        var str = current.ToString();

        for (int i = 1; i < chain.Count; i++)
        {
            var levelType = chain[i];

            // Find nested under current containing symbol
            var nestedDef = current.FindNestedTypeForRuntime(levelType);
            if (nestedDef is null)
                return (INamedTypeSymbol)compilation.ErrorTypeSymbol;

            current = nestedDef;

            if (slices[i].Length > 0)
                current = (INamedTypeSymbol)current.Construct(slices[i].Select(x => ResolveType(x, methodContext)!).ToArray());
        }

        return current;
    }

    internal ITypeParameterSymbol ResolveMethodTypeParameter(Type type, PEMethodSymbol methodSymbol)
    {
        var key = (methodSymbol, type);

        if (_methodTypeParameters.TryGetValue(key, out var existing))
            return existing;

        var symbol = new PETypeParameterSymbol(type, methodSymbol, methodSymbol.ContainingType, methodSymbol.ContainingNamespace, [new MetadataLocation(methodSymbol.ContainingModule!)], this).AddAsMember2();
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

        // When we already have a runtime Type, always intern through the module's Type-based cache
        // to ensure we never create a second instance for the same type via the metadata-name path.
        if (assemblySymbol.PrimaryModule is PEModuleSymbol peModule)
        {
            var r = peModule.GetType(type);
            if (r is not null)
            {
                return r;
            }
        }

        return compilation.GetTypeByMetadataName(metadataName)
            ?? compilation.ErrorTypeSymbol;
    }

    private static string? GetMetadataName(Type type)
    {
        if (type.DeclaringType is { } declaringType)
        {
            var declaringName = GetMetadataName(declaringType);
            if (declaringName is null)
                return null;

            var (nestedName, nestedArity) = type.NestedNameAndDeclaredArity();
            var nestedMetadataName = nestedArity > 0 ? $"{nestedName}`{nestedArity}" : nestedName;
            return $"{declaringName}+{nestedMetadataName}";
        }

        if (type.FullName is { } fullName)
            return fullName;

        var name = type.Name;
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

internal static class ReflectionTypeExtensions
{
    /// <summary>True if this Type represents System.Nullable&lt;T&gt;.</summary>
    public static bool IsNullableValueType(this Type t)
        => t.IsGenericType && t.GetGenericTypeDefinition().Name.Contains("Nullable");

    /// <summary>
    /// True when reflection generic args include inherited outer args and we must rebuild the chain.
    /// </summary>
    public static bool RequiresNestedChainResolution(this Type t)
        => t.IsNested && (
               t.IsGenericType
            || t.ContainsGenericParameters
            || (t.DeclaringType?.IsGenericType ?? false)
           );

    /// <summary>The declaring type chain from outermost -> innermost (includes this type).</summary>
    public static IReadOnlyList<Type> DeclaringTypeChain(this Type t)
    {
        var chain = new List<Type>();
        for (var cur = t; cur != null; cur = cur.DeclaringType)
            chain.Add(cur);
        chain.Reverse();
        return chain;
    }

    /// <summary>Simple runtime name without generic arity tick (e.g. "Ok`2" -> "Ok").</summary>
    public static string SimpleName(this Type t)
    {
        var name = t.Name;
        var tick = name.IndexOf('`');
        return tick >= 0 ? name[..tick] : name;
    }

    /// <summary>Total arity according to reflection (includes inherited outer args for nested types).</summary>
    public static int TotalRuntimeArity(this Type t)
    {
        if (!t.IsGenericType && !t.IsGenericTypeDefinition) return 0;
        var def = t.IsGenericTypeDefinition ? t : t.GetGenericTypeDefinition();
        return def.GetGenericArguments().Length;
    }

    /// <summary>
    /// Declared arity for this level in the source sense:
    /// for nested types, subtract declaring total arity.
    /// </summary>
    public static int DeclaredArity(this Type t)
    {
        if (!t.IsGenericType && !t.IsGenericTypeDefinition) return 0;

        var total = t.TotalRuntimeArity();
        var decl = t.DeclaringType;
        var declTotal = decl is null ? 0 : decl.TotalRuntimeArity();
        return Math.Max(0, total - declTotal);
    }

    /// <summary>Returns (simpleName, declaredArity) for nested matching.</summary>
    public static (string name, int declaredArity) NestedNameAndDeclaredArity(this Type t)
        => (t.SimpleName(), t.DeclaredArity());
}

internal static class ReflectionGenericArgumentExtensions
{
    /// <summary>
    /// Slices the flat generic argument list into per-level slices using each level's declared arity.
    /// Outer-to-inner order.
    /// </summary>
    public static Type[][] SliceGenericArgumentsPerLevel(this Type innermost, IReadOnlyList<Type> chain)
    {
        var arities = chain.Select(x => x.DeclaredArity()).ToArray();

        var flat = (innermost.IsGenericType || innermost.ContainsGenericParameters)
            ? innermost.GetGenericArguments()
            : Type.EmptyTypes;

        var result = new Type[arities.Length][];
        var pos = 0;

        for (int i = 0; i < arities.Length; i++)
        {
            var n = arities[i];
            if (n == 0)
            {
                result[i] = Array.Empty<Type>();
                continue;
            }

            result[i] = flat.Skip(pos).Take(n).ToArray();
            pos += n;
        }

        return result;
    }
}

internal static class SymbolNestedLookupExtensions
{
    /// <summary>
    /// Finds a nested type symbol under <paramref name="containing"/> that corresponds to the runtime nested type.
    /// Matching is based on simple name + declared arity (not total runtime arity).
    /// </summary>
    public static INamedTypeSymbol? FindNestedTypeForRuntime(this INamedTypeSymbol containing, Type runtimeNestedType)
    {
        var (name, declaredArity) = runtimeNestedType.NestedNameAndDeclaredArity();

        INamedTypeSymbol? bestNameOnly = null;

        foreach (var candidate in containing.GetMembers().OfType<INamedTypeSymbol>())
        {
            if (string.Equals(candidate.Name, name, StringComparison.Ordinal) && candidate.Arity == declaredArity)
                return candidate;

            if (bestNameOnly is null && string.Equals(candidate.Name, name, StringComparison.Ordinal))
                bestNameOnly = candidate;
        }

        return bestNameOnly;
    }
}
