using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    internal void EnsureAsyncLoweredForDeclaredMethod(MethodDeclarationSyntax methodDeclaration, IMethodSymbol methodSymbol)
    {
        var methodKey = GetSyntaxNodeMapKey(methodDeclaration);
        if (!_asyncLoweringInProgress.Add(methodKey))
            return;

        try
        {
            if (methodSymbol is not SourceMethodSymbol sourceMethod || sourceMethod.AsyncStateMachine is not null)
                return;

            var hasAwaitSyntax = methodDeclaration.Body is not null
                ? Compilation.ContainsAwaitExpressionOutsideNestedFunctions(methodDeclaration.Body)
                : methodDeclaration.ExpressionBody is not null &&
                  Compilation.ContainsAwaitExpressionOutsideNestedFunctions(methodDeclaration.ExpressionBody.Expression);
            if (hasAwaitSyntax)
                sourceMethod.SetContainsAwait(true);

            if (!sourceMethod.IsAsync &&
                methodDeclaration.Modifiers.Any(modifier => modifier.Kind == SyntaxKind.AsyncKeyword) &&
                sourceMethod.ContainingType is INamedTypeSymbol containingType)
            {
                var parameterCount = methodDeclaration.ParameterList?.Parameters.Count ?? 0;
                var arity = methodDeclaration.TypeParameterList?.Parameters.Count ?? 0;
                var asyncCandidate = containingType
                    .GetMembers(sourceMethod.Name)
                    .OfType<SourceMethodSymbol>()
                    .FirstOrDefault(candidate =>
                        candidate.IsAsync &&
                        candidate.Parameters.Length == parameterCount &&
                        candidate.TypeParameters.Length == arity &&
                        candidate.DeclaringSyntaxReferences.Any(reference =>
                            reference.SyntaxTree == methodDeclaration.SyntaxTree &&
                            reference.Span == methodDeclaration.Span));

                if (asyncCandidate is not null)
                    sourceMethod = asyncCandidate;
            }

            if (methodDeclaration.Body is not null)
            {
                _ = GetBoundNode(methodDeclaration.Body, BoundTreeView.Lowered);

                if (sourceMethod.AsyncStateMachine is null &&
                    GetBinder(methodDeclaration.Body).ContainingSymbol is SourceMethodSymbol loweredMethod &&
                    loweredMethod.AsyncStateMachine is not null)
                {
                    sourceMethod.SetAsyncStateMachine(loweredMethod.AsyncStateMachine);
                }
            }
            else if (methodDeclaration.ExpressionBody is not null)
            {
                _ = GetBoundNode(methodDeclaration.ExpressionBody.Expression, BoundTreeView.Lowered);

                if (sourceMethod.AsyncStateMachine is null &&
                    GetBinder(methodDeclaration.ExpressionBody.Expression).ContainingSymbol is SourceMethodSymbol loweredMethod &&
                    loweredMethod.AsyncStateMachine is not null)
                {
                    sourceMethod.SetAsyncStateMachine(loweredMethod.AsyncStateMachine);
                }
            }

        if (sourceMethod.AsyncStateMachine is null)
        {
            BoundBlockStatement? originalBody = null;

            if (methodDeclaration.Body is not null)
            {
                originalBody = GetBoundNode(methodDeclaration.Body, BoundTreeView.Original) as BoundBlockStatement;

                if ((originalBody is null || !originalBody.Statements.Any()) &&
                    methodDeclaration.Body.Statements.Count > 0 &&
                    TryForceBindMethodBody(methodDeclaration, sourceMethod) is { } reboundBody)
                {
                    originalBody = reboundBody;
                }
            }
            else if (methodDeclaration.ExpressionBody is not null &&
                GetBoundNode(methodDeclaration.ExpressionBody.Expression, BoundTreeView.Original) is BoundExpression expressionBody)
            {
                originalBody = ConvertExpressionBodyToBlock(sourceMethod, expressionBody);
                }

            if (originalBody is not null && AsyncLowerer.ShouldRewrite(sourceMethod, originalBody))
                _ = AsyncLowerer.Rewrite(sourceMethod, originalBody);
        }

            if (sourceMethod.AsyncStateMachine is null &&
                sourceMethod.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            {
                var targetTree = methodDeclaration.SyntaxTree;
                var targetSpan = methodDeclaration.Span;

                var matched = sourceAssembly.Compilation
                    .GetSynthesizedAsyncStateMachineTypes()
                    .FirstOrDefault(machine =>
                        machine.AsyncMethod.DeclaringSyntaxReferences.Any(reference =>
                            reference.SyntaxTree == targetTree && reference.Span == targetSpan));

                matched ??= sourceAssembly.Compilation
                    .GetSynthesizedAsyncStateMachineTypes()
                    .FirstOrDefault(machine =>
                        string.Equals(machine.AsyncMethod.Name, sourceMethod.Name, StringComparison.Ordinal) &&
                        machine.AsyncMethod.Parameters.Length == sourceMethod.Parameters.Length &&
                        string.Equals(machine.AsyncMethod.ContainingType?.Name, sourceMethod.ContainingType?.Name, StringComparison.Ordinal) &&
                        string.Equals(
                            machine.AsyncMethod.ContainingType?.ContainingNamespace?.ToMetadataName(),
                            sourceMethod.ContainingType?.ContainingNamespace?.ToMetadataName(),
                            StringComparison.Ordinal));

                if (matched is not null)
                    sourceMethod.SetAsyncStateMachine(matched);
            }

            if (!ReferenceEquals(methodSymbol, sourceMethod) &&
                methodSymbol is SourceMethodSymbol originalSourceMethod &&
                originalSourceMethod.AsyncStateMachine is null &&
                sourceMethod.AsyncStateMachine is not null)
            {
                originalSourceMethod.SetAsyncStateMachine(sourceMethod.AsyncStateMachine);
            }
        }
        finally
        {
            _asyncLoweringInProgress.Remove(methodKey);
        }
    }

    private BoundBlockStatement? TryForceBindMethodBody(MethodDeclarationSyntax methodDeclaration, SourceMethodSymbol sourceMethod)
    {
        if (methodDeclaration.Body is null)
            return null;

        var containingTypeSyntax = methodDeclaration.Parent as TypeDeclarationSyntax;
        var parentBinder = containingTypeSyntax is not null
            ? GetBinder(containingTypeSyntax)
            : GetBinder(methodDeclaration.Parent);

        var methodBinder = new MethodBinder(sourceMethod, parentBinder);
        CacheBinder(methodDeclaration, methodBinder);

        var methodBodyBinder = new MethodBodyBinder(sourceMethod, methodBinder);
        CacheBinder(methodDeclaration.Body, methodBodyBinder);

        return methodBodyBinder.GetOrBind(methodDeclaration.Body) as BoundBlockStatement;
    }
}
