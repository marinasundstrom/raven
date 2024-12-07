using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static readonly SyntaxToken OpenParenToken = (SyntaxToken)InternalSyntax.SyntaxFactory.OpenParenToken;

    public static readonly SyntaxToken CloseParenToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CloseParenToken;

    public static readonly SyntaxToken OpenBraceToken = (SyntaxToken)InternalSyntax.SyntaxFactory.OpenBraceToken;

    public static readonly SyntaxToken CloseBraceToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CloseBraceToken;

    public static readonly SyntaxToken GreaterThanToken = (SyntaxToken)InternalSyntax.SyntaxFactory.GreaterThanToken;

    public static readonly SyntaxToken IfKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.IfKeyword;

    public static SyntaxToken Identifier(string text) => (SyntaxToken)InternalSyntax.SyntaxFactory.Identifier(text);
}

static partial class SyntaxFactory
{
    // Cache wrappers to avoid multiple instances for the same GreenNode
    private static readonly ConditionalWeakTable<GreenNode, SyntaxNode> _cache = new ConditionalWeakTable<GreenNode, SyntaxNode>();

    /// <summary>
    /// Creates a wrapper for a given GreenNode.
    /// </summary>
    /// <param name="node">The internal GreenNode.</param>
    /// <param name="parent">The parent SyntaxNode.</param>
    /// <returns>The external SyntaxNode wrapper.</returns>
    public static SyntaxNode CreateWrapper(GreenNode node, SyntaxNode parent = null)
    {
        if (node == null)
            throw new ArgumentNullException(nameof(node));

        return _cache.GetValue(node, n => CreateWrapperInternal(n, parent));
    }

    private static SyntaxNode CreateWrapperInternal(GreenNode node, SyntaxNode parent)
    {
        /*
        if (node is InternalSyntax.SyntaxToken token)
            return new SyntaxToken(token, parent); */

        if (node is InternalSyntax.SyntaxNode syntaxNode)
        {
            switch (syntaxNode.Kind)
            {
                case SyntaxKind.IfStatement:
                    return new IfStatementSyntax(syntaxNode, parent);
                case SyntaxKind.ElseClause:
                    return new ElseClauseSyntax((InternalSyntax.ElseClauseSyntax)syntaxNode, parent);
                case SyntaxKind.Block:
                    return new BlockSyntax((InternalSyntax.BlockSyntax)syntaxNode, parent);
                case SyntaxKind.MethodDeclaration:
                    return new MethodDeclarationSyntax(syntaxNode, parent);
                // Add cases for other SyntaxKinds with specialized wrappers here
                default:
                    throw new ArgumentException($"Unknown GreenNode type: {node.GetType().Name}");
                    //return new SyntaxNode(syntaxNode, parent);
            }
        }
        else
            throw new ArgumentException($"Unknown GreenNode type: {node.GetType().Name}");
    }

    /// <summary>
    /// Creates a SyntaxListWrapper from an internal SyntaxList.
    /// </summary>
    public static SyntaxList CreateListWrapper(InternalSyntax.SyntaxList list, SyntaxNode parent)
    {
        return new SyntaxList(list, parent);
    }

    /// <summary>
    /// Creates a SeparatedSyntaxListWrapper from an internal SyntaxList.
    /// </summary>
    public static SeparatedSyntaxList CreateSeparatedListWrapper(InternalSyntax.SyntaxList list, SyntaxNode parent)
    {
        return new SeparatedSyntaxList(list, parent);
    }

    /// <summary>
    /// Creates a SyntaxTriviaListWrapper from an internal SyntaxTriviaList.
    /// </summary>
    public static SyntaxTriviaList CreateTriviaListWrapper(InternalSyntax.SyntaxTriviaList triviaList, SyntaxToken parent)
    {
        return new SyntaxTriviaList(parent, triviaList);
    }
}
