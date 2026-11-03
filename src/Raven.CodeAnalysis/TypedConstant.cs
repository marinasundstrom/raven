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

public readonly record struct TypedConstant
{
    internal TypedConstant(TypeSymbol? type, object? value, TypedConstantKind kind)
    {
        Type = type;
        Value = value;
        Kind = kind;
    }

    public TypeSymbol? Type { get; init; }

    public object? Value { get; init; }

    public TypedConstantKind Kind { get; init; }

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

    public void Deconstruct(out TypeSymbol? type, out object? value, out TypedConstantKind kind)
    {
        type = Type;
        value = Value;
        kind = Kind;
    }

    public override string ToString() => "Raven.CodeAnalysis.TypedConstant";

    internal TypedConstant WithType(TypeSymbol? type)
        => new(type, Value, Kind);
}
