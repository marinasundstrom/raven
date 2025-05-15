namespace Raven.CodeAnalysis;

public sealed class TypeSymbol // : ITypeSymbol
{
    public string Name { get; }

    private TypeSymbol(string name)
    {
        Name = name;
    }

    public static readonly ITypeSymbol Int = null;  //new TypeSymbol("int");
    public static readonly ITypeSymbol String = null;  //new TypeSymbol("string");
    public static readonly ITypeSymbol Bool = null;  //new TypeSymbol("bool");
    public static readonly ITypeSymbol Object = null;  //new TypeSymbol("object");

    public override string ToString() => Name;

    // Optional: helpful for interning comparisons
    public override bool Equals(object obj) => obj is TypeSymbol ts && ts.Name == Name;
    public override int GetHashCode() => Name.GetHashCode();
}