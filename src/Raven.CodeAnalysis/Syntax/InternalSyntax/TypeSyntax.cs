namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class TypeSyntax : ExpressionSyntax
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}


internal abstract partial class NameSyntax : TypeSyntax
{
    protected NameSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}

internal abstract partial class SimpleNameSyntax : NameSyntax
{
    protected SimpleNameSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}

internal partial class IdentifierNameSyntax : SimpleNameSyntax
{
    public IdentifierNameSyntax(SyntaxToken nameToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.IdentifierName, [nameToken], diagnostics)
    {
    }
}

internal partial class GenericNameSyntax : SimpleNameSyntax
{
    public GenericNameSyntax(SyntaxToken nameToken, TypeArgumentListSyntax typeArgumentList,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.GenericName, [nameToken, typeArgumentList], diagnostics)
    {
    }
}

internal partial class QualifiedNameSyntax : NameSyntax
{
    public QualifiedNameSyntax(NameSyntax left, SyntaxToken dotToken, SimpleNameSyntax name,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.QualifiedName, [left, dotToken, name], diagnostics)
    {
    }
}

internal partial class AliasQualifiedNameSyntax : NameSyntax
{
    public AliasQualifiedNameSyntax(IdentifierNameSyntax alias, SyntaxToken colonColonToken, SimpleNameSyntax name,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.AliasQualifiedName, [alias, colonColonToken, name], diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static IdentifierNameSyntax IdentifierName(
        SyntaxToken nameToken,
        IEnumerable<Diagnostic>? diagnostics = null)
      => new(nameToken, diagnostics);

    public static GenericNameSyntax GenericName(
        SyntaxToken nameToken,
        TypeArgumentListSyntax typeArgumentList,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(nameToken, typeArgumentList, diagnostics);

    public static QualifiedNameSyntax QualifiedName(
        NameSyntax left,
        SyntaxToken dotToken,
        SimpleNameSyntax name,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(left, dotToken, name, diagnostics);

    public static AliasQualifiedNameSyntax AliasQualifiedName(
        IdentifierNameSyntax alias,
        SyntaxToken colonColonToken,
        SimpleNameSyntax name,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(alias, colonColonToken, name, diagnostics);
}
