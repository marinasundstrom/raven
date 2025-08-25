using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal class TypeResolver(Compilation compilation)
{
    private readonly Dictionary<Type, ITypeSymbol> _cache = new();
    private readonly NullabilityInfoContext _nullabilityContext = new();

    public ITypeSymbol? ResolveType(ParameterInfo parameterInfo)
    {
        var unionAttribute = parameterInfo.GetCustomAttributesData()
            .FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");

        if (unionAttribute is not null)
        {
            return CreateUnionTypeSymbol(unionAttribute);
        }

        var type = ResolveType(parameterInfo.ParameterType);

        if (type is ITypeParameterSymbol typeParameterSymbol)
            return type;

        var nullInfo = _nullabilityContext.Create(parameterInfo);
        return ApplyNullability(type!, nullInfo);
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

    private bool TryGetUnion(MemberInfo memberInfo, out IUnionTypeSymbol? unionType)
    {
        unionType = null;
        var unionAttribute = memberInfo.GetCustomAttributesData()
            .FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");

        if (unionAttribute is not null)
        {
            unionType = CreateUnionTypeSymbol(unionAttribute);
        }
        return unionType is not null;
    }

    private IUnionTypeSymbol CreateUnionTypeSymbol(CustomAttributeData unionAttribute)
    {
        var types = ((IEnumerable<CustomAttributeTypedArgument>)unionAttribute
            .ConstructorArguments.First().Value).Select(x => (Type)x.Value);

        return new UnionTypeSymbol(types.Select(x => ResolveType(x)!).ToArray(), null, null, null, []);
    }

    public ITypeSymbol? ResolveType(Type type)
    {
        if (_cache.TryGetValue(type, out var cached))
            return cached;

        if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>))
        {
            var underlying = ResolveType(type.GetGenericArguments()[0]);
            return new NullableTypeSymbol(underlying!, null, null, null, []);
        }

        // TODO: Return immediately if built in type

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

            return new PETypeParameterSymbol(type, declaringNamedType, declaringNamedType, declaringNamedType.ContainingNamespace, []);
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

        if (nullInfo.ReadState == NullabilityState.Nullable && typeSymbol is not NullableTypeSymbol && !typeSymbol.IsValueType)
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
}
