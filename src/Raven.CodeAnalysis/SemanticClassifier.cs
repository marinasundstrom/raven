using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class SemanticClassifier
{
    public static SemanticClassificationResult Classify(SyntaxNode node, SemanticModel model)
    {
        var tokenMap = new Dictionary<SyntaxToken, SemanticClassification>();
        var triviaMap = new Dictionary<SyntaxTrivia, SemanticClassification>();

        foreach (var descendant in node.DescendantTokens(descendIntoTrivia: true))
        {
            var kind = descendant.Kind;

            // Reserved words
            if (descendant.IsKeyword())
            {
                tokenMap[descendant] = SemanticClassification.Keyword;
            }
            // Literals
            else if (kind == SyntaxKind.StringLiteralToken ||
                     kind == SyntaxKind.StringStartToken ||
                     kind == SyntaxKind.StringEndToken ||
                     kind == SyntaxKind.DollarToken ||
                     (kind == SyntaxKind.OpenBraceToken && descendant.Parent is InterpolationSyntax) ||
                     (kind == SyntaxKind.CloseBraceToken && descendant.Parent is InterpolationSyntax))
            {
                tokenMap[descendant] = SemanticClassification.StringLiteral;
            }
            else if (kind == SyntaxKind.NumericLiteralToken)
            {
                tokenMap[descendant] = SemanticClassification.NumericLiteral;
            }
            // Identifiers (with symbol resolution)
            else if (kind == SyntaxKind.IdentifierToken)
            {
                var bindNode = GetBindableParent(descendant);
                if (bindNode is not null)
                {
                    var info = model.GetSymbolInfo(bindNode);
                    var symbol = info.Symbol
                                 ?? info.CandidateSymbols.FirstOrDefault()
                                 ?? model.GetDeclaredSymbol(bindNode);

                    var classification = symbol is null
                        ? ClassifyBySyntax(bindNode)
                        : ClassifySymbol(symbol);

                    tokenMap[descendant] = classification;
                }
            }

            // Comments from trivia
            foreach (var trivia in descendant.LeadingTrivia.Concat(descendant.TrailingTrivia))
            {
                if (trivia.Kind == SyntaxKind.SingleLineCommentTrivia ||
                    trivia.Kind == SyntaxKind.MultiLineCommentTrivia)
                {
                    triviaMap[trivia] = SemanticClassification.Comment;
                }
            }
        }

        return new SemanticClassificationResult(tokenMap, triviaMap);
    }

    private static SemanticClassification ClassifySymbol(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol => SemanticClassification.Namespace,
            ITypeSymbol => SemanticClassification.Type,
            IMethodSymbol => SemanticClassification.Method,
            IParameterSymbol => SemanticClassification.Parameter,
            ILocalSymbol => SemanticClassification.Local,
            IFieldSymbol => SemanticClassification.Field,
            IPropertySymbol => SemanticClassification.Property,
            _ => SemanticClassification.Default
        };
    }

    private static SemanticClassification ClassifyBySyntax(SyntaxNode node)
    {
        return node switch
        {
            InvocationExpressionSyntax => SemanticClassification.Method,
            MemberAccessExpressionSyntax { Parent: InvocationExpressionSyntax } => SemanticClassification.Method,
            MemberBindingExpressionSyntax { Parent: InvocationExpressionSyntax } => SemanticClassification.Method,
            _ => SemanticClassification.Default
        };
    }

    private static SyntaxNode? GetBindableParent(SyntaxToken token)
    {
        var node = token.Parent;

        // Namespace declarations expose their symbol from the declaration node
        if (node is IdentifierNameSyntax && node.Parent is NamespaceDeclarationSyntax ns && ns.Name == node)
            return ns;

        // Climb to the outermost bindable node that includes this identifier
        while (node != null)
        {
            if (node.Parent is MemberAccessExpressionSyntax ma && ma.Name == node)
                node = ma;
            else if (node.Parent is MemberBindingExpressionSyntax mb && mb.Name == node)
                node = mb;
            else if (node.Parent is InvocationExpressionSyntax inv && inv.Expression == node)
                node = inv;
            else
                break;
        }

        return node;
    }
}

public sealed record SemanticClassificationResult(
    Dictionary<SyntaxToken, SemanticClassification> Tokens,
    Dictionary<SyntaxTrivia, SemanticClassification> Trivia
);

public enum SemanticClassification
{
    Default,
    Keyword,
    NumericLiteral,
    StringLiteral,
    Comment,
    Namespace,
    Type,
    Method,
    Parameter,
    Local,
    Property,
    Field
}
