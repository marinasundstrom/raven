using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class SemanticClassifier
{
    public static Dictionary<SyntaxToken, SemanticClassification> Classify(SyntaxNode node, SemanticModel model)
    {
        var map = new Dictionary<SyntaxToken, SemanticClassification>();

        foreach (var descendant in node.DescendantTokens(descendIntoTrivia: true))
        {
            var kind = descendant.Kind;

            if (descendant.IsKeyword())
            {
                map[descendant] = SemanticClassification.Keyword;
            }
            else if (kind == SyntaxKind.StringLiteralToken)
            {
                map[descendant] = SemanticClassification.StringLiteral;
            }
            else if (kind == SyntaxKind.IdentifierToken)
            {
                var symbol = model.GetSymbolInfo(descendant.Parent!).Symbol;

                if (symbol is INamespaceSymbol)
                    map[descendant] = SemanticClassification.Namespace;
                else if (symbol is ITypeSymbol)
                    map[descendant] = SemanticClassification.Type;
                else if (symbol is IMethodSymbol)
                    map[descendant] = SemanticClassification.Method;
                else if (symbol is IParameterSymbol)
                    map[descendant] = SemanticClassification.Parameter;
                else if (symbol is IFieldSymbol)
                    map[descendant] = SemanticClassification.Field;
                else if (symbol is IPropertySymbol)
                    map[descendant] = SemanticClassification.Property;
                else
                    map[descendant] = SemanticClassification.Default;
            }
        }

        return map;
    }
}

public enum SemanticClassification
{
    Default,
    Keyword,
    StringLiteral,
    Comment,
    Namespace,
    Type,
    Method,
    Parameter,
    Property,
    Field
}