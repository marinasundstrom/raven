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

            if (kind is SyntaxKind.IsKeyword or SyntaxKind.AsKeyword)
            {
                tokenMap[descendant] = SemanticClassification.Operator;
            }
            // Reserved words
            else if (descendant.IsKeyword())
            {
                tokenMap[descendant] = SemanticClassification.Keyword;
            }
            // Interpolated-string punctuation: color ${ and } as interpolation (but only when they belong to an Interpolation node).
            else if ((kind == SyntaxKind.DollarToken ||
                      kind == SyntaxKind.OpenBraceToken ||
                      kind == SyntaxKind.CloseBraceToken) &&
                     IsInterpolationPunctuation(descendant))
            {
                tokenMap[descendant] = SemanticClassification.Interpolation;
            }
            // Literals
            else if (kind == SyntaxKind.StringLiteralToken ||
                     kind == SyntaxKind.MultiLineStringLiteralToken ||
                     kind == SyntaxKind.StringStartToken ||
                     kind == SyntaxKind.StringEndToken)
            {
                tokenMap[descendant] = SemanticClassification.StringLiteral;
            }
            else if (kind == SyntaxKind.NumericLiteralToken)
            {
                tokenMap[descendant] = SemanticClassification.NumericLiteral;
            }
            else if (kind == SyntaxKind.QuestionToken && descendant.Parent is NullableTypeSyntax)
            {
                tokenMap[descendant] = SemanticClassification.NullableAnnotation;
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
                        ? ClassifyBySyntaxOrEventFallback(bindNode, model)
                        : ClassifySymbol(symbol);

                    classification = ClassifyDiscriminatedUnionCasePattern(descendant, bindNode, symbol, model, classification);
                    tokenMap[descendant] = classification;
                }
            }

            // Comments from trivia
            foreach (var trivia in descendant.LeadingTrivia.Concat(descendant.TrailingTrivia))
            {
                if (trivia.Kind == SyntaxKind.SingleLineCommentTrivia ||
                    trivia.Kind == SyntaxKind.MultiLineCommentTrivia ||
                    trivia.Kind == SyntaxKind.DirectiveTrivia ||
                    trivia.Kind == SyntaxKind.DocumentationCommentTrivia)
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
            ILabelSymbol => SemanticClassification.Label,
            IFieldSymbol => SemanticClassification.Field,
            IPropertySymbol => SemanticClassification.Property,
            IEventSymbol => SemanticClassification.Event,
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
            MemberPatternPathSyntax => SemanticClassification.Type,
            ParameterSyntax => SemanticClassification.Parameter,
            TypeSyntax => SemanticClassification.Type,
            _ => SemanticClassification.Default
        };
    }

    private static SemanticClassification ClassifyBySyntaxOrEventFallback(SyntaxNode node, SemanticModel model)
    {
        var bySyntax = ClassifyBySyntax(node);
        if (bySyntax != SemanticClassification.Default)
            return bySyntax;

        if (node is MemberAccessExpressionSyntax memberAccess)
        {
            var receiverType = model.GetTypeInfo(memberAccess.Expression).Type;
            if (receiverType is not null)
            {
                var memberName = memberAccess.Name.Identifier.ValueText;
                var member = receiverType.GetMembers(memberName).OfType<IEventSymbol>().FirstOrDefault();
                if (member is not null)
                    return SemanticClassification.Event;
            }
        }
        else if (node is IdentifierNameSyntax identifier)
        {
            var binder = model.GetBinder(identifier);
            var memberName = identifier.Identifier.ValueText;
            if (binder.LookupSymbols(memberName).OfType<IEventSymbol>().Any())
                return SemanticClassification.Event;
        }

        return SemanticClassification.Default;
    }

    private static SemanticClassification ClassifyDiscriminatedUnionCasePattern(
        SyntaxToken token,
        SyntaxNode bindNode,
        ISymbol? symbol,
        SemanticModel model,
        SemanticClassification current)
    {
        if (IsUnionCasePatternNode(bindNode))
        {
            if (current == SemanticClassification.Type)
                return SemanticClassification.Type;

            if (symbol is ITypeSymbol typeSymbol && typeSymbol.IsUnionCase)
                return SemanticClassification.Type;

            if (symbol is IMethodSymbol methodSymbol && methodSymbol.ReturnType.IsUnionCase)
                return SemanticClassification.Type;

            if (symbol is IMethodSymbol methodOnDiscriminatedUnion &&
                (methodOnDiscriminatedUnion.ContainingType.IsUnionCase ||
                 methodOnDiscriminatedUnion.ContainingType.IsUnion))
            {
                return SemanticClassification.Type;
            }

            if (symbol is IPropertySymbol propertyOnDiscriminatedUnion &&
                (propertyOnDiscriminatedUnion.ContainingType?.IsUnionCase == true ||
                 propertyOnDiscriminatedUnion.ContainingType?.IsUnion == true ||
                 propertyOnDiscriminatedUnion.Type.IsUnionCase))
            {
                return SemanticClassification.Type;
            }

            if (symbol is IFieldSymbol fieldOnDiscriminatedUnion &&
                (fieldOnDiscriminatedUnion.ContainingType?.IsUnionCase == true ||
                 fieldOnDiscriminatedUnion.ContainingType?.IsUnion == true ||
                 fieldOnDiscriminatedUnion.Type.IsUnionCase))
            {
                return SemanticClassification.Type;
            }

            if (current == SemanticClassification.Method && bindNode is IdentifierNameSyntax { Parent: ConstantPatternSyntax })
                return SemanticClassification.Type;

            if (bindNode is ExpressionSyntax expression)
            {
                var typeInfo = model.GetTypeInfo(expression).Type;
                if (typeInfo?.IsUnionCase == true)
                    return SemanticClassification.Type;
            }
        }

        if (current == SemanticClassification.Method && IsIdentifierInMatchArmPatternPosition(token))
            return SemanticClassification.Type;

        return current;
    }

    private static bool IsUnionCasePatternNode(SyntaxNode bindNode)
    {
        return bindNode switch
        {
            MemberPatternPathSyntax => true,
            IdentifierNameSyntax { Parent: ConstantPatternSyntax } => true,
            GenericNameSyntax { Parent: ConstantPatternSyntax } => true,
            _ => false
        };
    }

    private static bool IsIdentifierInMatchArmPatternPosition(SyntaxToken token)
    {
        if (token.Kind != SyntaxKind.IdentifierToken)
            return false;

        for (var current = token.Parent; current is not null; current = current.Parent)
        {
            if (current is MatchArmSyntax matchArm)
                return token.Span.End <= matchArm.ArrowToken.Span.Start;
        }

        return false;
    }

    private static bool IsInterpolationPunctuation(SyntaxToken token)
    {
        // We only want to color these tokens as interpolation when they are part of an InterpolationSyntax node.
        // This avoids coloring normal braces elsewhere.
        var parent = token.Parent;

        if (parent is InterpolationSyntax)
            return true;

        return parent?.Parent is InterpolationSyntax;
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
    Operator,
    Interpolation,
    Comment,
    Namespace,
    Type,
    Method,
    Parameter,
    Local,
    Label,
    Property,
    Field,
    Event,
    NullableAnnotation
}
