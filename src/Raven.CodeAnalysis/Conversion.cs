namespace Raven.CodeAnalysis;

public struct Conversion
{
    public bool Exists { get; }

    public bool IsImplicit { get; }
    public bool IsExplicit => !IsImplicit;
    public bool IsIdentity { get; }
    public bool IsReference { get; }
    public bool IsBoxing { get; }
    public bool IsUnboxing { get; }

    public bool IsUserDefined { get; }
    public IMethodSymbol? MethodSymbol { get; }

    public Conversion(bool isImplicit, bool isIdentity = false, bool isReference = false, bool isBoxing = false, bool isUnboxing = false)
    {
        IsImplicit = isImplicit;
        IsIdentity = isIdentity;
        IsReference = isReference;
        IsBoxing = isBoxing;
        IsUnboxing = isUnboxing;
    }

    public static Conversion None => new Conversion(false);
}
