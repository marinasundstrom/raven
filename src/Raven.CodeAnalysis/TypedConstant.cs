using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public enum TypedConstantKind
{
    Primitive,
    Enum,
    Type,
    Array,
    Null,
    Error
}

public readonly struct TypedConstant
{
    internal TypedConstant(ITypeSymbol? type, object? value, TypedConstantKind kind)
    {
        Type = type;
        Value = value;
        Kind = kind;
    }

    public ITypeSymbol? Type { get; }

    public object? Value { get; }

    public TypedConstantKind Kind { get; }

    public bool IsNull => Kind == TypedConstantKind.Null;

    public ImmutableArray<TypedConstant> Values
        => Kind == TypedConstantKind.Array && Value is ImmutableArray<TypedConstant> array
            ? array
            : ImmutableArray<TypedConstant>.Empty;

    internal static TypedConstant CreatePrimitive(ITypeSymbol? type, object? value)
    {
        if (value is null)
            return CreateNull(type);

        return new TypedConstant(type, value, TypedConstantKind.Primitive);
    }

    internal static TypedConstant CreateNull(ITypeSymbol? type)
        => new(type, null, TypedConstantKind.Null);

    internal static TypedConstant CreateType(ITypeSymbol? type, ITypeSymbol value)
        => new(type, value, TypedConstantKind.Type);

    internal static TypedConstant CreateArray(ITypeSymbol? type, ImmutableArray<TypedConstant> values)
        => new(type, values, TypedConstantKind.Array);

    internal static TypedConstant CreateError(ITypeSymbol? type)
        => new(type, null, TypedConstantKind.Error);

    internal TypedConstant WithType(ITypeSymbol? type)
    {
        if (Kind == TypedConstantKind.Array && Value is ImmutableArray<TypedConstant> array)
            return new TypedConstant(type, array, Kind);

        return new TypedConstant(type, Value, Kind);
    }
}
