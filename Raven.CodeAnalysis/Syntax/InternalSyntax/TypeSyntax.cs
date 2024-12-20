
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class TypeSyntax : ExpressionSyntax
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}


internal abstract partial class NameSyntax : TypeSyntax
{
    protected NameSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}

internal abstract partial class SimpleNameSyntax : NameSyntax
{
    protected SimpleNameSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}

internal partial class IdentifierNameSyntax : SimpleNameSyntax
{
    public IdentifierNameSyntax(SyntaxToken nameToken)
        : base(SyntaxKind.IdentifierName, [nameToken])
    {
    }
}

internal partial class GenericNameSyntax : SimpleNameSyntax
{
    public GenericNameSyntax(SyntaxToken nameToken, TypeArgumentListSyntax typeArgumentList)
        : base(SyntaxKind.GenericName, [nameToken, typeArgumentList])
    {
    }
}

internal partial class QualifiedNameSyntax : NameSyntax
{
    public QualifiedNameSyntax(NameSyntax left, SyntaxToken dotToken, SimpleNameSyntax name)
        : base(SyntaxKind.QualifiedName, [left, dotToken, name])
    {
    }
}

internal partial class AliasQualifiedNameSyntax : NameSyntax
{
    public AliasQualifiedNameSyntax(IdentifierNameSyntax alias, SyntaxToken colonColonToken, SimpleNameSyntax name)
        : base(SyntaxKind.AliasQualifiedName, [alias, colonColonToken, name])
    {
    }
}