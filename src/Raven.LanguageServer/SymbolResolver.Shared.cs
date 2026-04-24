using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    // Shared resolver helpers used across the intent-specific partials.
    private static bool IsTypeContext(SyntaxNode node)
    {
        return node.AncestorsAndSelf()
            .OfType<TypeSyntax>()
            .Any(IsExplicitTypeSyntaxContext);
    }

    private static bool IsExplicitTypeSyntaxContext(TypeSyntax typeSyntax)
    {
        return typeSyntax.Parent switch
        {
            TypeAnnotationClauseSyntax => true,
            ArrowTypeClauseSyntax => true,
            TypeSyntax => true,
            TypeOfExpressionSyntax => true,
            SizeOfExpressionSyntax => true,
            DefaultExpressionSyntax => true,
            CastExpressionSyntax cast when ReferenceEquals(cast.Type, typeSyntax) => true,
            _ => false
        };
    }

    private static bool HaveEquivalentSpan(SyntaxNode? left, SyntaxNode? right)
        => left is not null &&
           right is not null &&
           left.Kind == right.Kind &&
           left.Span == right.Span;

    private static bool IsInvocationTargetPosition(SyntaxNode node, SyntaxToken token)
    {
        if (node.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().Any(invocation => IsInvocationTargetMatch(invocation.Expression, node, token)))
            return true;

        return node.AncestorsAndSelf()
            .OfType<InfixOperatorExpressionSyntax>()
            .Any(pipe =>
                pipe.Kind == SyntaxKind.PipeExpression &&
                pipe.Right is not InvocationExpressionSyntax &&
                IsInvocationTargetMatch(pipe.Right, node, token));
    }

    private static bool IsInvocationTargetMatch(SyntaxNode target, SyntaxNode node, SyntaxToken token)
    {
        if (ReferenceEquals(target, node))
            return true;

        if (target.Span.Contains(node.Span))
            return true;

        if (node.Span.Contains(target.Span))
            return true;

        if (target.Span.Contains(token.Span))
            return true;

        return token.SpanStart == target.Span.End;
    }

    private static ISymbol ProjectTypeContextSymbol(ISymbol symbol)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
            return constructor.ContainingType ?? symbol;

        return symbol;
    }

    private static ISymbol? ChoosePreferredSymbol(
        ISymbol? primarySymbol,
        ImmutableArray<ISymbol> candidates,
        SyntaxNode node)
    {
        var all = new List<ISymbol>(capacity: 1 + (candidates.IsDefaultOrEmpty ? 0 : candidates.Length));
        if (primarySymbol is not null)
            all.Add(primarySymbol);

        if (!candidates.IsDefaultOrEmpty)
            all.AddRange(candidates.Where(static c => c is not null));

        if (all.Count == 0)
            return null;

        return all
            .Distinct(SymbolEqualityComparer.Default)
            .OrderByDescending(symbol => ScoreSymbol(symbol, node))
            .FirstOrDefault();
    }

    private static int ScoreSymbol(ISymbol symbol, SyntaxNode node)
    {
        var score = 0;

        var inTypePosition = node.AncestorsAndSelf().OfType<TypeSyntax>().Any();
        if (inTypePosition)
            score += symbol.Kind == SymbolKind.Type ? 300 : symbol.Kind == SymbolKind.Namespace ? -300 : 0;

        if (symbol.DeclaringSyntaxReferences.Length > 0)
            score += 200;

        var docs = symbol.GetDocumentationComment();
        if (docs is not null)
        {
            if (docs.Format == DocumentationFormat.Markdown)
                score += 120;
            else if (docs.Format == DocumentationFormat.Xml)
                score += 40;
        }

        if (symbol.Kind == SymbolKind.Method)
            score += 20;

        if (symbol.Kind == SymbolKind.Namespace)
            score -= 20;

        return score;
    }

    private static ISymbol? ProjectSymbolForDisplay(ISymbol? symbol)
    {
        if (symbol is IMethodSymbol methodSymbol &&
            methodSymbol.AssociatedSymbol is { } associatedMember &&
            associatedMember is IPropertySymbol or IEventSymbol)
        {
            return associatedMember;
        }

        if (symbol is IFieldSymbol fieldSymbol &&
            fieldSymbol.AssociatedSymbol is { } associatedFieldMember &&
            associatedFieldMember is IPropertySymbol or IEventSymbol)
        {
            return associatedFieldMember;
        }

        return symbol;
    }

    private static ISymbol? FindReferencedSymbolAtToken(IOperation? operation, TextSpan tokenSpan)
    {
        if (operation is null || !operation.Syntax.Span.Contains(tokenSpan))
            return null;

        var current = GetOperationSymbol(operation);

        foreach (var child in operation.ChildOperations)
        {
            if (!child.Syntax.Span.Contains(tokenSpan))
                continue;

            var childSymbol = FindReferencedSymbolAtToken(child, tokenSpan);
            if (childSymbol is not null)
                return childSymbol;
        }

        return current;
    }

    private static ISymbol? GetOperationSymbol(IOperation operation)
    {
        return operation switch
        {
            ILiteralOperation literal => literal.Type?.UnwrapLiteralType(),
            ISymbolReferenceOperation<ISymbol> symbolReference => symbolReference.Symbol,
            ISingleVariableDesignatorOperation designator => designator.Local,
            IVariableDeclaratorOperation declarator => declarator.Symbol,
            IInvocationOperation invocation => invocation.TargetMethod,
            _ => null
        };
    }

    private static bool ShouldSkipCandidateNode(SyntaxNode node, SyntaxToken token)
    {
        return node switch
        {
            CompilationUnitSyntax => true,
            GlobalStatementSyntax => true,
            LocalDeclarationStatementSyntax => true,
            VariableDeclarationSyntax => true,
            VariableDeclaratorSyntax declarator => token != declarator.Identifier,
            FunctionStatementSyntax functionStatement => !IsFunctionDeclarationToken(functionStatement, token),
            FunctionExpressionSyntax functionExpression =>
                !IsFunctionExpressionDeclarationToken(functionExpression, token) ||
                IsInsideFunctionExpressionBody(functionExpression, token),
            MethodDeclarationSyntax methodDeclaration => token != methodDeclaration.Identifier,
            BaseTypeDeclarationSyntax typeDeclaration => token != typeDeclaration.Identifier,
            CaseDeclarationSyntax caseDeclaration => token != caseDeclaration.Identifier,
            _ => false
        };
    }

    private static bool IsFunctionDeclarationToken(FunctionStatementSyntax functionStatement, SyntaxToken token)
    {
        if (!token.Span.IntersectsWith(functionStatement.Span))
            return false;

        if (token == functionStatement.Identifier)
            return true;

        if (functionStatement.Body is { } body)
        {
            if (token == body.OpenBraceToken || token == body.CloseBraceToken)
                return true;

            return token.SpanStart < body.Span.Start;
        }

        if (functionStatement.ExpressionBody is { } expressionBody)
            return token.SpanStart < expressionBody.Span.Start;

        return true;
    }

    private static bool IsFunctionExpressionDeclarationToken(FunctionExpressionSyntax functionExpression, SyntaxToken token)
    {
        if (!token.Span.IntersectsWith(functionExpression.Span))
            return false;

        if (functionExpression.Body is { } body)
        {
            if (token == body.OpenBraceToken || token == body.CloseBraceToken)
                return true;

            return token.SpanStart < body.Span.Start;
        }

        if (functionExpression.ExpressionBody is { } expressionBody)
            return token.SpanStart < expressionBody.Expression.Span.Start;

        return true;
    }

    private static bool IsFunctionExpressionIdentifierToken(FunctionExpressionSyntax functionExpression, SyntaxToken token)
        => functionExpression is ParenthesizedFunctionExpressionSyntax { Identifier.IsMissing: false } parenthesized &&
           token == parenthesized.Identifier;

    private static bool IsInsideFunctionExpressionBody(FunctionExpressionSyntax functionExpression, SyntaxToken token)
    {
        if (functionExpression.Body?.Span.Contains(token.Span) == true)
            return true;

        if (functionExpression.ExpressionBody?.Expression.Span.Contains(token.Span) == true)
            return true;

        return false;
    }

    private static bool TryMatchDeclaringSpan(ISymbol? symbol, SyntaxToken token)
    {
        if (symbol is null || symbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return false;

        foreach (var reference in symbol.DeclaringSyntaxReferences)
        {
            if (reference.SyntaxTree != token.SyntaxTree)
                continue;

            var declarationSpan = GetDeclarationIdentifierSpan(reference) ?? reference.Span;
            if (declarationSpan.Contains(token.SpanStart))
                return true;
        }

        return false;
    }

    private static bool TryGetSymbolInfo(SemanticModel semanticModel, SyntaxNode node, out SymbolInfo symbolInfo)
    {
        try
        {
            if (semanticModel.TryGetNodeInterestSymbolInfo(node, out symbolInfo) &&
                (symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty))
            {
                return true;
            }

            symbolInfo = semanticModel.GetSymbolInfo(node);
            return true;
        }
        catch
        {
            symbolInfo = default;
            return false;
        }
    }

    private static IOperation? TryGetOperation(SemanticModel semanticModel, SyntaxNode node)
    {
        try
        {
            return semanticModel.GetOperation(node);
        }
        catch
        {
            return null;
        }
    }

    private static TextSpan? GetDeclarationIdentifierSpan(SyntaxReference reference)
    {
        return reference.GetSyntax() switch
        {
            ParameterSyntax parameter when !parameter.Identifier.IsMissing => parameter.Identifier.Span,
            VariableDeclaratorSyntax declarator when !declarator.Identifier.IsMissing => declarator.Identifier.Span,
            SingleVariableDesignationSyntax designation when !designation.Identifier.IsMissing => designation.Identifier.Span,
            FunctionStatementSyntax function when !function.Identifier.IsMissing => function.Identifier.Span,
            MethodDeclarationSyntax method when !method.Identifier.IsMissing => method.Identifier.Span,
            PropertyDeclarationSyntax property when !property.Identifier.IsMissing => property.Identifier.Span,
            EventDeclarationSyntax @event when !@event.Identifier.IsMissing => @event.Identifier.Span,
            FieldDeclarationSyntax field => field.Declaration?.Declarators.FirstOrDefault()?.Identifier.Span,
            _ => null
        };
    }
}
