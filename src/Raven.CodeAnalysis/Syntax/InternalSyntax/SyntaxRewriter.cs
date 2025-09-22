using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class SyntaxRewriter : SyntaxVisitor<SyntaxNode?>
{
    private int _recursionDepth;

    [return: NotNullIfNotNull(nameof(node))]
    public override SyntaxNode? Visit(SyntaxNode? node)
    {
        if (node is not null)
        {
            _recursionDepth++;

            var result = node.Accept(this);

            _recursionDepth--;

            return result;
        }

        return null;
    }

    public override SyntaxNode? DefaultVisit(SyntaxNode node)
    {
        return node;
    }

    public virtual SyntaxToken VisitToken(SyntaxToken token)
    {
        return token;
    }

    public virtual SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return default;
    }

    public virtual SyntaxNode? VisitStatement(StatementSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitExpression(ExpressionSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitType(TypeSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitName(NameSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitUnqualifiedName(UnqualifiedNameSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitSimpleName(SimpleNameSyntax node)
    {
        return node.Accept(this);
    }

    public virtual ConstructorInitializerSyntax? VisitConstructorInitializer(ConstructorInitializerSyntax? node)
    {
        return (ConstructorInitializerSyntax?)node?.Accept(this);
    }

    public virtual SyntaxList? VisitList(SyntaxList list)
    {
        List<GreenNode> newList = [];

        foreach (var item in list.GetChildren())
        {
            newList.Add(item.Accept(this));
        }
        return SyntaxFactory.List(newList);
    }
}