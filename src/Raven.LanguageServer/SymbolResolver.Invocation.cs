using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    // Invocation-oriented resolution handles:
    // - direct call targets
    // - constructor/type projection at call sites
    // - contextual union-case invocations such as Ok(...) / Error(...)
    private static bool TryResolveInvocationTargetSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var invocation = node switch
        {
            InvocationExpressionSyntax direct => direct,
            IdentifierNameSyntax { Parent: InvocationExpressionSyntax direct } => direct,
            MemberAccessExpressionSyntax { Parent: InvocationExpressionSyntax parent } => parent,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     HaveEquivalentSpan(memberAccess.Name, identifier) &&
                     memberAccess.Parent is InvocationExpressionSyntax parent => parent,
            _ => null
        };

        if (invocation is null)
            return false;

        // Only resolve invocation targets when hovering the call target itself,
        // not arguments/parameter lists/lambda bodies.
        if (!invocation.Expression.Span.Contains(token.Span))
            return false;

        if (TryResolvePipeBoundInvocationTargetSymbol(semanticModel, invocation, out var pipeBoundSymbol))
        {
            symbol = pipeBoundSymbol;
            return true;
        }

        if (TryResolveUnionCaseFromInvocationContext(semanticModel, invocation, out var unionCaseSymbol))
        {
            symbol = unionCaseSymbol;
            return true;
        }

        if (TryGetSymbolInfo(semanticModel, invocation, out var symbolInfo))
        {
            if (symbolInfo.Symbol is not null)
            {
                symbol = ProjectInvocationSymbolForDisplay(symbolInfo.Symbol, semanticModel, invocation);
                return true;
            }

            if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                symbol = ProjectInvocationSymbolForDisplay(symbolInfo.CandidateSymbols[0], semanticModel, invocation);
                return true;
            }
        }

        switch (semanticModel.GetOperation(invocation))
        {
            case IUnionCaseOperation unionCaseOperation:
                symbol = unionCaseOperation.CaseType;
                return true;
            case IInvocationOperation operation when operation.TargetMethod is not null:
                symbol = ProjectInvocationSymbolForDisplay(operation.TargetMethod, semanticModel, invocation);
                return true;
        }

        var invocationExpressionOperation = TryGetOperation(semanticModel, invocation.Expression);
        var invocationExpressionSymbol = invocationExpressionOperation is null
            ? null
            : ProjectSymbolForDisplay(GetOperationSymbol(invocationExpressionOperation));
        if (invocationExpressionSymbol is not null)
        {
            symbol = ProjectInvocationSymbolForDisplay(invocationExpressionSymbol, semanticModel, invocation);
            return true;
        }

        // When this invocation is the right-hand side of a pipe expression the binder
        // resolves overloads using the piped value as an implicit first argument and
        // stores the constructed symbol on the BinaryExpression node, not on the
        // InvocationExpression itself.
        return false;
    }

    private static bool TryResolvePipeBoundInvocationTargetSymbol(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        var pipeExpr = invocation.Ancestors()
            .OfType<InfixOperatorExpressionSyntax>()
            .FirstOrDefault(pipe => pipe.Kind == SyntaxKind.PipeExpression && pipe.Right == invocation);
        if (pipeExpr is null)
            return false;

        if (TryGetSymbolInfo(semanticModel, pipeExpr, out var pipeInfo))
        {
            if (pipeInfo.Symbol is not null)
            {
                symbol = ProjectInvocationSymbolForDisplay(pipeInfo.Symbol, semanticModel, invocation);
                return true;
            }

            if (!pipeInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                symbol = ProjectInvocationSymbolForDisplay(pipeInfo.CandidateSymbols[0], semanticModel, invocation);
                return true;
            }
        }

        switch (semanticModel.GetOperation(pipeExpr))
        {
            case IUnionCaseOperation pipeUnionCaseOperation:
                symbol = pipeUnionCaseOperation.CaseType;
                return true;
            case IInvocationOperation pipeOperation when pipeOperation.TargetMethod is not null:
                symbol = ProjectInvocationSymbolForDisplay(pipeOperation.TargetMethod, semanticModel, invocation);
                return true;
        }

        var invocationExpressionOperation = TryGetOperation(semanticModel, invocation.Expression);
        var invocationExpressionSymbol = invocationExpressionOperation is null
            ? null
            : ProjectSymbolForDisplay(GetOperationSymbol(invocationExpressionOperation));
        if (invocationExpressionSymbol is not null)
        {
            symbol = ProjectInvocationSymbolForDisplay(invocationExpressionSymbol, semanticModel, invocation);
            return true;
        }

        return false;
    }

    private static ISymbol ProjectInvocationSymbolForDisplay(
        ISymbol symbol,
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
        {
            // Prefer the instantiated type at this call site (for example Option<int>)
            // instead of the constructor's containing generic definition.
            if (TryGetInstantiatedInvocationType(semanticModel, invocation) is { } instantiated)
                return instantiated;

            return constructor.ContainingType ?? symbol;
        }

        return symbol;
    }

    private static INamedTypeSymbol? TryGetInstantiatedInvocationType(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        if (semanticModel.GetOperation(invocation) is IInvocationOperation { TargetMethod: { MethodKind: MethodKind.Constructor } method } &&
            method.ContainingType is INamedTypeSymbol methodContainingType)
        {
            return methodContainingType;
        }

        var invocationTypeInfo = semanticModel.GetTypeInfo(invocation);
        if ((invocationTypeInfo.ConvertedType ?? invocationTypeInfo.Type) is INamedTypeSymbol invocationType)
            return invocationType;

        var expressionTypeInfo = semanticModel.GetTypeInfo(invocation.Expression);
        if ((expressionTypeInfo.ConvertedType ?? expressionTypeInfo.Type) is INamedTypeSymbol expressionType)
            return expressionType;

        return null;
    }

    private static bool TryResolveUnionCaseFromInvocationContext(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        out ISymbol? symbol)
    {
        symbol = null;

        var invokedName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberAccessExpressionSyntax { Name: IdentifierNameSyntax identifier } => identifier.Identifier.ValueText,
            _ => null
        };

        if (string.IsNullOrWhiteSpace(invokedName))
            return false;

        var typeInfo = semanticModel.GetTypeInfo(invocation);
        var targetType = typeInfo.ConvertedType ?? typeInfo.Type;
        if ((targetType is null || (!targetType.IsUnion && !targetType.IsUnionCase)) &&
            TryResolveContextualUnionTargetType(invocation, semanticModel, out var contextualTargetType))
        {
            targetType = contextualTargetType;
        }

        if (targetType is null)
            return false;

        if (targetType is IUnionSymbol union && targetType.IsUnion)
        {
            var unionCase = union.CaseTypes.FirstOrDefault(c => string.Equals(c.Name, invokedName, StringComparison.Ordinal));
            if (unionCase is not null)
            {
                symbol = unionCase;
                return true;
            }
        }

        if (targetType is IUnionCaseTypeSymbol caseType &&
            targetType.IsUnionCase &&
            string.Equals(caseType.Name, invokedName, StringComparison.Ordinal))
        {
            symbol = caseType;
            return true;
        }

        return false;
    }

    private static bool TryResolveContextualUnionTargetType(
        InvocationExpressionSyntax invocation,
        SemanticModel semanticModel,
        [NotNullWhen(true)] out ITypeSymbol? targetType)
    {
        targetType = null;

        if (invocation.AncestorsAndSelf().OfType<FunctionStatementSyntax>().FirstOrDefault() is { } functionStatement &&
            semanticModel.GetDeclaredSymbol(functionStatement) is IMethodSymbol { ReturnType: { } functionReturnType } &&
            (functionReturnType.IsUnion || functionReturnType.IsUnionCase))
        {
            targetType = functionReturnType;
            return true;
        }

        if (invocation.AncestorsAndSelf().OfType<FunctionExpressionSyntax>().FirstOrDefault() is { } functionExpression)
        {
            var functionTypeInfo = semanticModel.GetTypeInfo(functionExpression);
            var functionType = functionTypeInfo.ConvertedType ?? functionTypeInfo.Type;
            if (functionType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType &&
                delegateType.GetDelegateInvokeMethod() is { ReturnType: { } delegateReturnType } &&
                (delegateReturnType.IsUnion || delegateReturnType.IsUnionCase))
            {
                targetType = delegateReturnType;
                return true;
            }
        }

        return false;
    }
}
