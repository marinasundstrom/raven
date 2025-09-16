namespace Raven.CodeAnalysis;

public struct Conversion
{
    public bool Exists { get; }

    public bool IsImplicit { get; }
    public bool IsExplicit => !IsImplicit;
    public bool IsIdentity { get; }
    public bool IsNumeric { get; }
    public bool IsReference { get; }
    public bool IsBoxing { get; }
    public bool IsUnboxing { get; }

    public bool IsUserDefined { get; }
    public bool IsAlias { get; }
    public IMethodSymbol? MethodSymbol { get; }

    public Conversion(
        bool isImplicit = false,
        bool isIdentity = false,
        bool isNumeric = false,
        bool isReference = false,
        bool isBoxing = false,
        bool isUnboxing = false,
        bool isUserDefined = false,
        bool isAlias = false,
        IMethodSymbol? methodSymbol = null)
    {
        Exists = true;
        IsImplicit = isImplicit;
        IsIdentity = isIdentity;
        IsNumeric = isNumeric;
        IsReference = isReference;
        IsBoxing = isBoxing;
        IsUnboxing = isUnboxing;
        IsUserDefined = isUserDefined;
        IsAlias = isAlias;
        MethodSymbol = methodSymbol;
    }

    public Conversion()
    {
        Exists = false;
    }

    public static Conversion None => new Conversion();

    public override bool Equals(object? obj)
        => obj is Conversion other && Equals(other);

    public bool Equals(Conversion other)
        => Exists == other.Exists &&
           IsImplicit == other.IsImplicit &&
           IsIdentity == other.IsIdentity &&
           IsNumeric == other.IsNumeric &&
           IsReference == other.IsReference &&
           IsBoxing == other.IsBoxing &&
           IsUnboxing == other.IsUnboxing &&
           IsUserDefined == other.IsUserDefined &&
           IsAlias == other.IsAlias &&
           SymbolEqualityComparer.Default.Equals(MethodSymbol, other.MethodSymbol);

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(Exists);
        hash.Add(IsImplicit);
        hash.Add(IsIdentity);
        hash.Add(IsNumeric);
        hash.Add(IsReference);
        hash.Add(IsBoxing);
        hash.Add(IsUnboxing);
        hash.Add(IsUserDefined);
        hash.Add(IsAlias);
        hash.Add(MethodSymbol, SymbolEqualityComparer.Default);
        return hash.ToHashCode();
    }

    public Conversion WithAlias(bool isAlias)
    {
        if (!Exists)
            return this;

        var combined = IsAlias || isAlias;
        if (combined == IsAlias)
            return this;

        return new Conversion(
            isImplicit: IsImplicit,
            isIdentity: IsIdentity,
            isNumeric: IsNumeric,
            isReference: IsReference,
            isBoxing: IsBoxing,
            isUnboxing: IsUnboxing,
            isUserDefined: IsUserDefined,
            isAlias: combined,
            methodSymbol: MethodSymbol);
    }

    public static bool operator ==(Conversion left, Conversion right) => left.Equals(right);
    public static bool operator !=(Conversion left, Conversion right) => !left.Equals(right);
}
