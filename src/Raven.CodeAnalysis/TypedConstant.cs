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
    internal TypedConstant(TypeSymbol? type, object? value, TypedConstantKind kind)
    {
        Type = type;
        Value = value;
        Kind = kind;
    }

    public TypeSymbol? Type { get; }

    public object? Value { get; }

    public TypedConstantKind Kind { get; }

    public bool IsNull => Kind == TypedConstantKind.Null;

    public ImmutableArray<TypedConstant> Values
        => Kind == TypedConstantKind.Array && Value is ImmutableArray<TypedConstant> array
            ? array
            : ImmutableArray<TypedConstant>.Empty;

    internal static TypedConstant CreatePrimitive(TypeSymbol? type, object? value)
    {
        if (value is null)
            return CreateNull(type);

        return new TypedConstant(type, value, TypedConstantKind.Primitive);
    }

    internal static TypedConstant CreateEnum(TypeSymbol? type, object? value)
    {
        if (value is null)
            return CreateNull(type);

        return new TypedConstant(type, value, TypedConstantKind.Enum);
    }

    internal static TypedConstant CreateNull(TypeSymbol? type)
        => new(type, null, TypedConstantKind.Null);

    internal static TypedConstant CreateType(TypeSymbol? type, TypeSymbol value)
        => new(type, value, TypedConstantKind.Type);

    internal static TypedConstant CreateArray(TypeSymbol? type, ImmutableArray<TypedConstant> values)
        => new(type, values, TypedConstantKind.Array);

    internal static TypedConstant CreateError(TypeSymbol? type)
        => new(type, null, TypedConstantKind.Error);

    internal TypedConstant WithType(TypeSymbol? type)
    {
        if (Kind == TypedConstantKind.Array && Value is ImmutableArray<TypedConstant> array)
            return new TypedConstant(type, array, Kind);

        return new TypedConstant(type, Value, Kind);
    }
}
