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
    public IMethodSymbol? MethodSymbol { get; }

    public Conversion(bool isImplicit = false, bool isIdentity = false, bool isNumeric = false, bool isReference = false, bool isBoxing = false, bool isUnboxing = false, bool isUserDefined = false)
    {
        Exists = true;
        IsImplicit = isImplicit;
        IsIdentity = isIdentity;
        IsNumeric = isNumeric;
        IsReference = isReference;
        IsBoxing = isBoxing;
        IsUnboxing = isUnboxing;
        IsUserDefined = isUserDefined;
    }

    public Conversion()
    {
        Exists = false;
    }

    public static Conversion None => new Conversion();
}