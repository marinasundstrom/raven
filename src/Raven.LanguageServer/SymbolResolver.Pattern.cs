using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    private static bool TryResolvePatternDeclaredSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = node switch
        {
            SingleVariableDesignationSyntax single when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            VariablePatternSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            TypedVariableDesignationSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            _ => null
        };

        if (symbol is not null)
            return true;

        symbol = node.Parent switch
        {
            SingleVariableDesignationSyntax single when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            VariablePatternSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            TypedVariableDesignationSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            _ => null
        };

        if (symbol is null &&
            node is IdentifierNameSyntax identifierName &&
            token == identifierName.Identifier &&
            identifierName.Parent is ConstantPatternSyntax &&
            TryResolveFunctionExpressionPatternLocalSymbol(semanticModel, identifierName, out var implicitPatternLocal))
        {
            symbol = implicitPatternLocal;
        }

        return symbol is not null;
    }

    private static bool TryResolveContainingStatementDesignatorSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var containingStatement = node.AncestorsAndSelf().OfType<StatementSyntax>().FirstOrDefault();
        if (containingStatement is null)
            return false;

        var statementOperation = TryGetOperation(semanticModel, containingStatement);
        if (statementOperation is null)
            return false;

        var stack = new Stack<IOperation>();
        stack.Push(statementOperation);

        while (stack.Count > 0)
        {
            var current = stack.Pop();
            switch (current)
            {
                case ISingleVariableDesignatorOperation designator
                    when TryMatchDeclaringSpan(designator.Local, token):
                    symbol = designator.Local;
                    return true;
                case IVariableDeclaratorOperation declarator
                    when TryMatchDeclaringSpan(declarator.Symbol, token):
                    symbol = declarator.Symbol;
                    return true;
            }

            foreach (var child in current.ChildOperations)
                stack.Push(child);
        }

        return false;
    }

    private static bool TryResolvePatternOperationDeclaredLocal(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var patternSyntax = node.AncestorsAndSelf().FirstOrDefault(static n =>
            n is PositionalPatternSyntax or SequencePatternSyntax);
        if (patternSyntax is null)
            return false;

        var operation = TryGetOperation(semanticModel, patternSyntax);
        if (operation is null)
            return false;

        var targetName = token.ValueText;
        var stack = new Stack<IOperation>();
        stack.Push(operation);

        while (stack.Count > 0)
        {
            var current = stack.Pop();
            switch (current)
            {
                case ISingleVariableDesignatorOperation designator
                    when designator.Syntax.Span.Contains(token.Span) ||
                         TryMatchDeclaringSpan(designator.Local, token) ||
                         (!string.IsNullOrWhiteSpace(targetName) &&
                          string.Equals(designator.Local.Name, targetName, StringComparison.Ordinal)):
                    symbol = designator.Local;
                    return true;
                case IVariableDeclaratorOperation declarator
                    when declarator.Syntax.Span.Contains(token.Span) ||
                         TryMatchDeclaringSpan(declarator.Symbol, token) ||
                         (!string.IsNullOrWhiteSpace(targetName) &&
                          string.Equals(declarator.Symbol.Name, targetName, StringComparison.Ordinal)):
                    symbol = declarator.Symbol;
                    return true;
            }

            foreach (var child in current.ChildOperations)
                stack.Push(child);
        }

        return false;
    }

    private static bool TryResolveFromEnclosingBlockLocals(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        foreach (var blockSyntax in node.AncestorsAndSelf().OfType<BlockStatementSyntax>())
        {
            if (semanticModel.GetOperation(blockSyntax) is not IBlockOperation blockOperation)
                continue;

            foreach (var local in blockOperation.Locals)
            {
                if (!TryMatchDeclaringSpan(local, token))
                    continue;

                symbol = local;
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveFunctionExpressionPatternLocalSymbol(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifierName,
        out ISymbol? symbol)
    {
        symbol = null;

        var parameter = identifierName.Ancestors().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null || parameter.Pattern is null)
            return false;

        var functionExpression = parameter.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (functionExpression is null)
            return false;

        if (semanticModel.GetOperation(functionExpression) is not ILambdaOperation lambdaOperation ||
            lambdaOperation.Body is null)
        {
            return false;
        }

        var targetName = identifierName.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(targetName))
            return false;

        var stack = new Stack<IOperation>();
        stack.Push(lambdaOperation.Body);
        while (stack.Count > 0)
        {
            var current = stack.Pop();
            if (current is IVariableDeclaratorOperation declarator &&
                string.Equals(declarator.Symbol.Name, targetName, StringComparison.Ordinal))
            {
                symbol = declarator.Symbol;
                return true;
            }

            if (current is ISingleVariableDesignatorOperation designator &&
                string.Equals(designator.Local.Name, targetName, StringComparison.Ordinal))
            {
                symbol = designator.Local;
                return true;
            }

            foreach (var child in current.ChildOperations)
                stack.Push(child);
        }

        return false;
    }

    private static bool TryResolveByDeclaringSyntaxReference(
        SemanticModel semanticModel,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (token.Kind != SyntaxKind.IdentifierToken || string.IsNullOrWhiteSpace(token.ValueText))
            return false;

        var root = token.SyntaxTree!.GetRoot();
        foreach (var candidate in root.DescendantNodes().OfType<IdentifierNameSyntax>())
        {
            if (!string.Equals(candidate.Identifier.ValueText, token.ValueText, StringComparison.Ordinal))
                continue;

            var info = semanticModel.GetSymbolInfo(candidate);
            if (TryMatchDeclaringSpan(info.Symbol, token))
            {
                symbol = info.Symbol;
                return true;
            }

            if (info.CandidateSymbols.IsDefaultOrEmpty)
                continue;

            foreach (var candidateSymbol in info.CandidateSymbols)
            {
                if (!TryMatchDeclaringSpan(candidateSymbol, token))
                    continue;

                symbol = candidateSymbol;
                return true;
            }
        }

        return false;
    }
}
