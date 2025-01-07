using System.Reflection.Metadata;

namespace Raven.CodeAnalysis.Syntax;

public abstract partial class TypeSyntax : ExpressionSyntax
{
    internal TypeSyntax(GreenNode greenNode, SyntaxNode parent, int position) : base(greenNode, parent, position)
    {
    }
}

public abstract class NameSyntax : TypeSyntax
{
    public virtual int Arity => 0;

    internal NameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public abstract class SimpleNameSyntax : NameSyntax
{
    public virtual SyntaxToken Identifier { get; }

    internal SimpleNameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public partial class IdentifierNameSyntax : SimpleNameSyntax
{
    public override partial SyntaxToken Identifier { get; }

    internal IdentifierNameSyntax(
        InternalSyntax.IdentifierNameSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public IdentifierNameSyntax(SyntaxToken identifier)
          : this(
                new InternalSyntax.IdentifierNameSyntax(identifier.Green), null)
    {

    }
}

public partial class GenericNameSyntax : SimpleNameSyntax
{
    public override partial SyntaxToken Identifier { get; }

    public partial TypeArgumentListSyntax TypeArgumentList { get; }

    public override int Arity => TypeArgumentList.Count;

    internal GenericNameSyntax(
        InternalSyntax.GenericNameSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public GenericNameSyntax(SyntaxToken identifier, TypeArgumentListSyntax typeArgumentList)
          : this(
                new InternalSyntax.GenericNameSyntax(identifier.Green, (InternalSyntax.TypeArgumentListSyntax)typeArgumentList.Green), null)
    {

    }
}

public partial class QualifiedNameSyntax : NameSyntax
{
    public partial NameSyntax Left { get; }

    public partial SyntaxToken DotToken { get; }

    public partial SimpleNameSyntax Right { get; }

    internal QualifiedNameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public QualifiedNameSyntax(NameSyntax left, SyntaxToken dotToken, SimpleNameSyntax right)
        : base(new InternalSyntax.QualifiedNameSyntax((InternalSyntax.NameSyntax)left.Green, dotToken.Green, (InternalSyntax.SimpleNameSyntax)right.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class AliasQualifiedNameSyntax : NameSyntax
{
    public partial IdentifierNameSyntax Alias { get; }

    public partial SyntaxToken ColonColonToken { get; }

    public partial SimpleNameSyntax Name { get; }


    internal AliasQualifiedNameSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public AliasQualifiedNameSyntax(IdentifierNameSyntax alias, SyntaxToken colonColonToken, SimpleNameSyntax name)
        : base(new InternalSyntax.AliasQualifiedNameSyntax((InternalSyntax.IdentifierNameSyntax)alias.Green, colonColonToken.Green, (InternalSyntax.SimpleNameSyntax)name.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static IdentifierNameSyntax IdentifierName(SyntaxToken identifier) => new IdentifierNameSyntax(identifier);

    public static IdentifierNameSyntax IdentifierName(string text) => new IdentifierNameSyntax(IdentifierToken(text));

    public static TypeSyntax ParseTypeName(string text) => new IdentifierNameSyntax(IdentifierToken(text));

    public static GenericNameSyntax GenericName(SyntaxToken identifier, TypeArgumentListSyntax typeArgumentList) => new GenericNameSyntax(identifier, typeArgumentList);

    public static QualifiedNameSyntax QualifiedName(NameSyntax left, SyntaxToken dotToken, IdentifierNameSyntax right) => new QualifiedNameSyntax(left, dotToken, right);

    public static AliasQualifiedNameSyntax AliasQualifiedName(IdentifierNameSyntax alias, SyntaxToken colonColonToken, IdentifierNameSyntax name) => new AliasQualifiedNameSyntax(alias, colonColonToken, name);

}