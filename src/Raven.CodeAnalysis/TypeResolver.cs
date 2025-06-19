using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal class TypeResolver(Compilation compilation)
{
    private readonly Dictionary<Type, ITypeSymbol> _cache = new();

    public ITypeSymbol? ResolveType(ParameterInfo parameterInfo)
    {
        var unionAttribute = parameterInfo.GetCustomAttributesData().FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");
        if (unionAttribute is not null)
        {
            return CreateUnionTypeSymbol(unionAttribute);
        }

        return ResolveType(parameterInfo.ParameterType);
    }


    public ITypeSymbol? ResolveType(FieldInfo fieldInfo)
    {
        if (TryGetUnion(fieldInfo, out var unionType))
        {
            return unionType;
        }

        return ResolveType(fieldInfo.FieldType);
    }

    public ITypeSymbol? ResolveType(PropertyInfo propertyInfo)
    {
        if (TryGetUnion(propertyInfo, out var unionType))
        {
            return unionType;
        }

        return ResolveType(propertyInfo.PropertyType);
    }

    private bool TryGetUnion(MemberInfo memberInfo, out IUnionTypeSymbol? unionType)
    {
        unionType = null;
        var unionAttribute = memberInfo.GetCustomAttributesData().FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");
        if (unionAttribute is not null)
        {
            unionType = CreateUnionTypeSymbol(unionAttribute);
        }
        return unionType is not null;
    }

    private IUnionTypeSymbol CreateUnionTypeSymbol(CustomAttributeData unionAttribute)
    {
        IUnionTypeSymbol? unionType;
        var types = ((IEnumerable<CustomAttributeTypedArgument>)unionAttribute.ConstructorArguments.First().Value).Select(x => (Type)x.Value);
        unionType = new UnionTypeSymbol(types.Select(x => ResolveType(x)!).ToArray(), null, null, null, []);
        return unionType;
    }

    public ITypeSymbol? ResolveType(Type type)
    {
        if (_cache.TryGetValue(type, out var cached))
            return cached;

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

    protected ITypeSymbol? ResolveTypeCore(Type type)
    {
        var typeInfo = type.GetTypeInfo();

        var assemblySymbol = (PEAssemblySymbol)compilation.ReferencedAssemblySymbols.First(x => x.Name == type.Assembly.GetName().Name);
        return (ITypeSymbol?)assemblySymbol.PrimaryModule.ResolveMetadataMember(assemblySymbol.GlobalNamespace, type.FullName);
    }
}