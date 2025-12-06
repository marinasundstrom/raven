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
            return new NullableTypeSymbol(underlying!, null, null, null, []);
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
            var genericTypeDefinition = (INamedTypeSymbol?)ResolveType(type.GetGenericTypeDefinition());
            var args = type.GetGenericArguments().Select(x => ResolveType(x)!);
            return genericTypeDefinition.Construct(args.ToArray());
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
            typeSymbol = new NullableTypeSymbol(typeSymbol, null, null, null, []);
        }

        return typeSymbol;
    }

    protected ITypeSymbol? ResolveTypeCore(Type type)
    {
        var typeInfo = type.GetTypeInfo();

        var assemblySymbol = (PEAssemblySymbol)compilation.ReferencedAssemblySymbols.First(x => x.Name == type.Assembly.GetName().Name);
        return (ITypeSymbol?)assemblySymbol.PrimaryModule.ResolveMetadataMember(assemblySymbol.GlobalNamespace, type.FullName);
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
}
