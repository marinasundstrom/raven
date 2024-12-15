
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class TypeSyntax : ExpressionSyntax
{
    protected TypeSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}


public abstract partial class NameSyntax : TypeSyntax
{
    protected NameSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}

public abstract partial class SimpleNameSyntax : NameSyntax
{
    protected SimpleNameSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}

public partial class IdentifierNameSyntax : SimpleNameSyntax
{
    public IdentifierNameSyntax(SyntaxToken nameToken)
        : base(SyntaxKind.IdentifierName, [nameToken])
    {
    }
}

public partial class GenericNameSyntax : SimpleNameSyntax
{
    public GenericNameSyntax(SyntaxToken nameToken)
        : base(SyntaxKind.GenericName, [nameToken])
    {
    }
}

public partial class QualifiedNameSyntax : NameSyntax
{
    public QualifiedNameSyntax(NameSyntax left, SyntaxToken dotToken, SimpleNameSyntax name)
        : base(SyntaxKind.QualifiedName, [left, dotToken, name])
    {
    }
}

public partial class AliasQualifiedNameSyntax : NameSyntax
{
    public AliasQualifiedNameSyntax(IdentifierNameSyntax alias, SyntaxToken colonColonToken, SimpleNameSyntax name)
        : base(SyntaxKind.AliasQualifiedName, [alias, colonColonToken, name])
    {
    }
}