using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public ImmutableArray<Symbol> GetCapturedVariables(Symbol symbol)
    {
        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        if (symbol is null)
            return ImmutableArray<Symbol>.Empty;

        return symbol switch
        {
            MethodSymbol method => GetCapturedVariablesFromMethodSymbol(method),
            LocalSymbol local => GetCapturedVariablesFromLocalSymbol(local),
            _ => ImmutableArray<Symbol>.Empty
        };
    }

    private ImmutableArray<Symbol> GetCapturedVariablesFromMethodSymbol(MethodSymbol method)
    {
        if (method is SourceLambdaSymbol sourceLambda)
        {
            if (sourceLambda.CapturedVariables.IsDefaultOrEmpty &&
                TryGetFunctionExpressionSyntax(sourceLambda, out var functionExpression))
            {
                return GetCapturedVariables(functionExpression);
            }

            return sourceLambda.CapturedVariables;
        }

        if (TryGetFunctionExpressionSyntax(method, out var declaringFunctionExpression))
            return GetCapturedVariables(declaringFunctionExpression);

        if (method is SourceMethodSymbol sourceMethod)
            return GetOrComputeFunctionCapturedVariables(sourceMethod);

        if (method.OriginalDefinition is { } originalDefinition &&
            !SymbolEqualityComparer.Default.Equals(originalDefinition, method))
        {
            return GetCapturedVariablesFromMethodSymbol(originalDefinition);
        }

        if (method.ConstructedFrom is { } constructedFrom &&
            !SymbolEqualityComparer.Default.Equals(constructedFrom, method))
        {
            return GetCapturedVariablesFromMethodSymbol(constructedFrom);
        }

        return ImmutableArray<Symbol>.Empty;
    }

    private static bool TryGetFunctionExpressionSyntax(
        MethodSymbol method,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out FunctionExpressionSyntax? functionExpression)
    {
        foreach (var reference in method.DeclaringSyntaxReferences)
        {
            var syntax = reference.GetSyntax();
            functionExpression = syntax as FunctionExpressionSyntax
                ?? syntax?.AncestorsAndSelf().OfType<FunctionExpressionSyntax>().FirstOrDefault();

            if (functionExpression is not null)
                return true;
        }

        functionExpression = null;
        return false;
    }

    private ImmutableArray<Symbol> GetCapturedVariablesFromLocalSymbol(LocalSymbol local)
    {
        if (local is SourceFunctionValueSymbol functionValue)
            return GetCapturedVariables(functionValue.TargetMethod);

        if (local.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return ImmutableArray<Symbol>.Empty;

        foreach (var syntaxRef in local.DeclaringSyntaxReferences)
        {
            if (syntaxRef.GetSyntax() is VariableDeclaratorSyntax { Initializer.Value: FunctionExpressionSyntax functionExpression })
                return GetCapturedVariables(functionExpression);
        }

        return ImmutableArray<Symbol>.Empty;
    }

    public ImmutableArray<Symbol> GetCapturedVariables(SyntaxNode node)
    {
        ValidateSyntaxNode(node, nameof(node));

        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        if (node is FunctionStatementSyntax functionStatement &&
            !MayFunctionStatementCapture(functionStatement))
        {
            return ImmutableArray<Symbol>.Empty;
        }

        if (node is FunctionStatementSyntax function &&
            GetDeclaredSymbol(function) is Symbol functionSymbol)
        {
            return GetCapturedVariables(functionSymbol);
        }

        if (node is FunctionExpressionSyntax lambdaSyntax)
        {
            if (TryGetBoundFunctionExpressionForCapture(lambdaSyntax, out var boundLambda))
            {
                return boundLambda.CapturedVariables
                    .Where(static symbol => symbol is not null)
                    .Distinct(SymbolEqualityComparer.Default)
                    .ToImmutableArray();
            }
        }

        var symbol = node switch
        {
            VariableDeclaratorSyntax variableDeclarator => GetDeclaredSymbol(variableDeclarator),
            ParameterSyntax parameter => GetDeclaredSymbol(parameter),
            _ => GetSymbolInfo(node).Symbol
        };

        return symbol is null
            ? ImmutableArray<Symbol>.Empty
            : IsCapturedVariable(symbol) ? ImmutableArray.Create(symbol) : ImmutableArray<Symbol>.Empty;
    }

    private bool TryGetBoundFunctionExpressionForCapture(
        FunctionExpressionSyntax functionExpression,
        out BoundFunctionExpression boundFunction)
    {
        var ownerRoot = GetInterestBindingRoot(functionExpression, includeExtendedExecutableRoots: true);
        if (ownerRoot is not null && !ReferenceEquals(ownerRoot, functionExpression))
        {
            var boundOwner = BindContextualRootForSemanticQuery(ownerRoot);
            if (TryFindBoundNodeBySyntax(boundOwner, functionExpression, out var ownerFunctionNode) &&
                ownerFunctionNode is BoundFunctionExpression ownerFunction)
            {
                if (!ownerFunction.CapturedVariables.Any())
                {
                    ClearCachedSemanticState(ownerRoot);
                    boundOwner = BindContextualRootForSemanticQuery(ownerRoot);
                    if (TryFindBoundNodeBySyntax(boundOwner, functionExpression, out var reboundOwnerFunctionNode) &&
                        reboundOwnerFunctionNode is BoundFunctionExpression reboundOwnerFunction)
                    {
                        ownerFunction = reboundOwnerFunction;
                    }
                }

                CacheBoundNode(functionExpression, ownerFunction, GetBinderForIncrementalSemanticQuery(functionExpression));
                boundFunction = ownerFunction;
                return true;
            }
        }

        var contextualRoot = GetFunctionExpressionRebindRoot(functionExpression);
        if (!ReferenceEquals(contextualRoot, functionExpression))
        {
            var boundRoot = BindContextualRootForSemanticQuery(contextualRoot);
            if (TryFindBoundNodeBySyntax(boundRoot, functionExpression, out var reboundNode) &&
                reboundNode is BoundFunctionExpression reboundFunction)
            {
                CacheBoundNode(functionExpression, reboundFunction, GetBinderForIncrementalSemanticQuery(functionExpression));
                boundFunction = reboundFunction;
                return true;
            }
        }

        if (TryGetContextualBoundFunctionExpression(functionExpression, out boundFunction))
            return true;

        if (GetBoundNode(functionExpression) is BoundFunctionExpression directFunction)
        {
            boundFunction = directFunction;
            return true;
        }

        boundFunction = null!;
        return false;
    }

    public bool IsCapturedVariable(Symbol symbol)
    {
        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        if (symbol is not LocalSymbol and not ParameterSymbol and not TypeSymbol)
            return false;

        var root = SyntaxTree.GetRoot();
        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            if (GetDeclaredSymbol(function) is not Symbol functionSymbol)
                continue;

            var captures = GetCapturedVariables(functionSymbol);
            if (!captures.IsDefaultOrEmpty &&
                ContainsCapturedSymbol(captures, symbol))
            {
                return true;
            }
        }

        foreach (var lambda in root.DescendantNodes().OfType<FunctionExpressionSyntax>())
        {
            var captures = GetCapturedVariables(lambda);
            if (!captures.IsDefaultOrEmpty &&
                ContainsCapturedSymbol(captures, symbol))
            {
                return true;
            }
        }

        return false;
    }

    private static bool ContainsCapturedSymbol(ImmutableArray<Symbol> captures, Symbol symbol)
    {
        if (captures.Contains(symbol, SymbolEqualityComparer.Default))
            return true;

        return captures.Any(captured =>
            captured is not null &&
            captured.DeclaringSyntaxReferences.Any(capturedReference =>
                symbol.DeclaringSyntaxReferences.Any(symbolReference =>
                    capturedReference.SyntaxTree == symbolReference.SyntaxTree &&
                    capturedReference.Span == symbolReference.Span)));
    }

    private ImmutableArray<Symbol> GetOrComputeFunctionCapturedVariables(SourceMethodSymbol method)
    {
        if (!method.CapturedVariables.IsDefaultOrEmpty)
            return method.CapturedVariables;

        if (method.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not FunctionStatementSyntax function ||
            !MayFunctionStatementCapture(function))
        {
            return ImmutableArray<Symbol>.Empty;
        }

        if (function.Parent is GlobalStatementSyntax globalStatement &&
            SyntaxTree.GetRoot() is CompilationUnitSyntax root)
        {
            EnsureTopLevelCompilationUnitBound(root);
            BindPrecedingGlobalStatementsForScope(
                root,
                globalStatement,
                GetBinderForIncrementalSemanticQuery(globalStatement),
                requireCompleteDeclarations: false);
        }
        else
        {
            BindPrecedingStatementsForScope(function);
        }

        BoundBlockStatement? functionBody = function.Body is not null
            ? GetBoundNode(function.Body, BoundTreeView.Original) as BoundBlockStatement
                ?? GetBoundNode(function.Body, BoundTreeView.Lowered) as BoundBlockStatement
            : GetBoundNode(function.ExpressionBody!, BoundTreeView.Original) as BoundBlockStatement
                ?? GetBoundNode(function.ExpressionBody!, BoundTreeView.Lowered) as BoundBlockStatement;

        if (functionBody is null)
            return ImmutableArray<Symbol>.Empty;

        LocalFunctionCaptureAnalysis.Update(method, functionBody);
        return method.CapturedVariables;
    }

    private void BindPrecedingStatementsForScope(StatementSyntax owner)
    {
        if (owner.Parent is null)
            return;

        foreach (var sibling in owner.Parent.ChildNodes().OfType<StatementSyntax>())
        {
            if (sibling.Span.Start >= owner.Span.Start)
                break;

            GetBinderForIncrementalSemanticQuery(sibling).EnsureStatementDeclarations(sibling);
        }
    }

    private bool MayFunctionStatementCapture(FunctionStatementSyntax function)
    {
        for (SyntaxNode? current = function.Parent; current is not null; current = current.Parent)
        {
            if (current is BlockStatementSyntax)
                return true;

            if (current is GlobalStatementSyntax globalStatement &&
                Compilation.IsTopLevelFunctionMember(globalStatement) &&
                globalStatement.Ancestors().OfType<CompilationUnitSyntax>().FirstOrDefault() is { } compilationUnit)
            {
                return Compilation.HasRunnableFileScopeCode(compilationUnit);
            }

            if (current is CompilationUnitSyntax or BaseNamespaceDeclarationSyntax)
                return false;
        }

        return false;
    }
}
