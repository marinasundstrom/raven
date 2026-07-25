using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed class RefSafetyAnalysis : BoundTreeWalker
{
    private readonly HashSet<ILocalSymbol> _stackAllocBackedLocals =
        new(SymbolEqualityComparer.Default);
    private readonly HashSet<ILocalSymbol> _localReferenceBackedLocals =
        new(SymbolEqualityComparer.Default);
    private readonly Dictionary<ILocalSymbol, ISymbol> _scopedBackedLocals =
        new(SymbolEqualityComparer.Default);
    private readonly List<BoundExpression> _escapingExpressions = [];
    private readonly List<BoundExpression> _escapingLocalReferenceExpressions = [];
    private readonly List<(BoundExpression Expression, ISymbol Origin)>
        _escapingScopedExpressions = [];

    public static RefSafetyAnalysisResult Analyze(
        BoundNode body,
        bool expressionResultEscapes = false)
    {
        var analysis = new RefSafetyAnalysis();
        analysis.Visit(body);

        if (expressionResultEscapes && TryGetResultExpression(body) is { } expression)
        {
            if (analysis.IsStackAllocBacked(expression))
                analysis.AddEscape(expression);
            if (analysis.IsLocalReferenceBacked(expression))
                analysis.AddLocalReferenceEscape(expression);
            if (SemanticFacts.MayBeRefLike(expression.Type) &&
                analysis.TryGetScopedOrigin(expression) is { } scopedOrigin)
            {
                analysis.AddScopedEscape(expression, scopedOrigin);
            }
        }

        return analysis.ToResult();
    }

    public IReadOnlyList<BoundExpression> EscapingExpressions => _escapingExpressions;
    public IReadOnlyList<BoundExpression> EscapingLocalReferenceExpressions =>
        _escapingLocalReferenceExpressions;
    public IReadOnlyList<(BoundExpression Expression, ISymbol Origin)>
        EscapingScopedExpressions => _escapingScopedExpressions;

    public RefSafetyAnalysisResult ToResult()
    {
        var violations = ImmutableArray.CreateBuilder<RefSafetyViolation>(
            _escapingExpressions.Count +
            _escapingLocalReferenceExpressions.Count +
            _escapingScopedExpressions.Count);
        violations.AddRange(
            _escapingExpressions.Select(static expression =>
                new RefSafetyViolation(
                    RefSafetyViolationKind.StackAllocationEscape,
                    expression)));
        violations.AddRange(
            _escapingLocalReferenceExpressions.Select(static expression =>
                new RefSafetyViolation(
                    RefSafetyViolationKind.LocalReferenceEscape,
                    expression)));
        violations.AddRange(
            _escapingScopedExpressions.Select(static item =>
                new RefSafetyViolation(
                    RefSafetyViolationKind.ScopedValueEscape,
                    item.Expression,
                    item.Origin)));
        return new RefSafetyAnalysisResult(violations.ToImmutable());
    }

    public override void VisitVariableDeclarator(BoundVariableDeclarator node)
    {
        if (node.Initializer is { } initializer && IsStackAllocBacked(initializer))
            _stackAllocBackedLocals.Add(node.Local);
        if (node.Initializer is { } localReferenceInitializer &&
            IsLocalReferenceBacked(localReferenceInitializer))
        {
            _localReferenceBackedLocals.Add(node.Local);
        }
        if (node.Initializer is { } scopedInitializer &&
            TryGetScopedOrigin(scopedInitializer) is { } scopedOrigin)
        {
            _scopedBackedLocals[node.Local] = scopedOrigin;
        }

        base.VisitVariableDeclarator(node);
    }

    public override void VisitExpression(BoundExpression node)
    {
        if (node is BoundLocalAssignmentExpression assignment &&
            IsStackAllocBacked(assignment.Right))
        {
            _stackAllocBackedLocals.Add(assignment.Local);
        }
        if (node is BoundLocalAssignmentExpression localAssignment &&
            IsLocalReferenceBacked(localAssignment.Right))
        {
            _localReferenceBackedLocals.Add(localAssignment.Local);
        }
        if (node is BoundLocalAssignmentExpression scopedAssignment &&
            TryGetScopedOrigin(scopedAssignment.Right) is { } scopedOrigin)
        {
            _scopedBackedLocals[scopedAssignment.Local] = scopedOrigin;
        }
        if (node is BoundParameterAssignmentExpression
            {
                Parameter.RefKind: not RefKind.None,
                Right: { } parameterValue,
            } &&
            TryGetScopedOrigin(parameterValue) is { } parameterScopedOrigin)
        {
            AddScopedEscape(parameterValue, parameterScopedOrigin);
        }
        if (node is BoundByRefAssignmentExpression
            {
                Right: { } byRefValue,
            } &&
            TryGetScopedOrigin(byRefValue) is { } byRefScopedOrigin)
        {
            AddScopedEscape(byRefValue, byRefScopedOrigin);
        }
        if (node is BoundFieldAssignmentExpression
            {
                Field.RefKind: not RefKind.None,
                Receiver: { } receiver,
                Right: { } right,
            } &&
            IsMethodLocalReference(right) &&
            TryGetLocal(receiver) is { } receiverLocal)
        {
            _localReferenceBackedLocals.Add(receiverLocal);
        }
        if (node is BoundFieldAssignmentExpression
            {
                Field.Type: { } fieldType,
                Receiver: { } refLikeReceiver,
                Right: { } refLikeValue,
            } &&
            SemanticFacts.MayBeRefLike(fieldType) &&
            IsStackAllocBacked(refLikeValue) &&
            TryGetLocal(refLikeReceiver) is { } stackAllocReceiverLocal)
        {
            _stackAllocBackedLocals.Add(stackAllocReceiverLocal);
        }
        if (node is BoundFieldAssignmentExpression
            {
                Field.Type: { } scopedFieldType,
                Receiver: { } scopedReceiver,
                Right: { } scopedValue,
            } &&
            SemanticFacts.MayBeRefLike(scopedFieldType) &&
            TryGetScopedOrigin(scopedValue) is { } fieldScopedOrigin &&
            TryGetLocal(scopedReceiver) is { } scopedReceiverLocal)
        {
            _scopedBackedLocals[scopedReceiverLocal] = fieldScopedOrigin;
        }
        if (node is BoundFieldAssignmentExpression
            {
                Field.Type: { } escapingFieldType,
                Receiver: { } escapingReceiver,
                Right: { } escapingFieldValue,
            } &&
            SemanticFacts.MayBeRefLike(escapingFieldType) &&
            (escapingReceiver is BoundSelfExpression ||
             TryGetParameter(escapingReceiver) is { RefKind: not RefKind.None }) &&
            TryGetScopedOrigin(escapingFieldValue) is { } escapingFieldOrigin)
        {
            AddScopedEscape(escapingFieldValue, escapingFieldOrigin);
        }

        base.VisitExpression(node);
    }

    public override void VisitReturnStatement(BoundReturnStatement node)
    {
        if (node.Expression is { } expression && IsStackAllocBacked(expression))
            AddEscape(expression);
        if (node.Expression is { } localReferenceExpression &&
            IsLocalReferenceBacked(localReferenceExpression))
        {
            AddLocalReferenceEscape(localReferenceExpression);
        }
        if (node.Expression is { } scopedExpression &&
            SemanticFacts.MayBeRefLike(scopedExpression.Type) &&
            TryGetScopedOrigin(scopedExpression) is { } scopedOrigin)
        {
            AddScopedEscape(scopedExpression, scopedOrigin);
        }

        base.VisitReturnStatement(node);
    }

    public override void VisitReturnExpression(BoundReturnExpression node)
    {
        if (node.Expression is { } expression && IsStackAllocBacked(expression))
            AddEscape(expression);
        if (node.Expression is { } localReferenceExpression &&
            IsLocalReferenceBacked(localReferenceExpression))
        {
            AddLocalReferenceEscape(localReferenceExpression);
        }
        if (node.Expression is { } scopedExpression &&
            SemanticFacts.MayBeRefLike(scopedExpression.Type) &&
            TryGetScopedOrigin(scopedExpression) is { } scopedOrigin)
        {
            AddScopedEscape(scopedExpression, scopedOrigin);
        }

        base.VisitReturnExpression(node);
    }

    public override void VisitFunctionExpression(BoundFunctionExpression node)
    {
    }

    public override void VisitFunctionStatement(BoundFunctionStatement node)
    {
    }

    public bool IsStackAllocBacked(BoundExpression expression)
    {
        return expression switch
        {
            BoundStackAllocExpression => true,
            BoundConversionExpression conversion => IsStackAllocBacked(conversion.Expression),
            BoundParenthesizedExpression parenthesized => IsStackAllocBacked(parenthesized.Expression),
            BoundLocalAccess localAccess => _stackAllocBackedLocals.Contains(localAccess.Local),
            BoundVariableExpression variable => _stackAllocBackedLocals.Contains(variable.Variable),
            BoundIfExpression conditional =>
                IsStackAllocBacked(conditional.ThenBranch) ||
                conditional.ElseBranch is { } elseBranch && IsStackAllocBacked(elseBranch),
            _ => false,
        };
    }

    public void AddEscape(BoundExpression expression)
    {
        if (!_escapingExpressions.Contains(expression))
            _escapingExpressions.Add(expression);
    }

    public bool IsLocalReferenceBacked(BoundExpression expression)
    {
        return expression switch
        {
            BoundConversionExpression conversion => IsLocalReferenceBacked(conversion.Expression),
            BoundParenthesizedExpression parenthesized => IsLocalReferenceBacked(parenthesized.Expression),
            BoundLocalAccess localAccess => _localReferenceBackedLocals.Contains(localAccess.Local),
            BoundVariableExpression variable => _localReferenceBackedLocals.Contains(variable.Variable),
            BoundIfExpression conditional =>
                IsLocalReferenceBacked(conditional.ThenBranch) ||
                conditional.ElseBranch is { } elseBranch && IsLocalReferenceBacked(elseBranch),
            _ => false,
        };
    }

    private static bool IsMethodLocalReference(BoundExpression expression)
    {
        return expression switch
        {
            BoundAddressOfExpression
            {
                Storage: BoundLocalAccess or BoundVariableExpression,
            } => true,
            BoundConversionExpression conversion => IsMethodLocalReference(conversion.Expression),
            BoundParenthesizedExpression parenthesized => IsMethodLocalReference(parenthesized.Expression),
            _ => false,
        };
    }

    private static ILocalSymbol? TryGetLocal(BoundExpression expression)
    {
        return expression switch
        {
            BoundLocalAccess localAccess => localAccess.Local,
            BoundVariableExpression variable => variable.Variable,
            BoundConversionExpression conversion => TryGetLocal(conversion.Expression),
            BoundParenthesizedExpression parenthesized => TryGetLocal(parenthesized.Expression),
            _ => null,
        };
    }

    private static IParameterSymbol? TryGetParameter(BoundExpression expression)
    {
        return expression switch
        {
            BoundParameterAccess parameterAccess => parameterAccess.Parameter,
            BoundConversionExpression conversion => TryGetParameter(conversion.Expression),
            BoundParenthesizedExpression parenthesized => TryGetParameter(parenthesized.Expression),
            _ => null,
        };
    }

    public void AddLocalReferenceEscape(BoundExpression expression)
    {
        if (!_escapingLocalReferenceExpressions.Contains(expression))
            _escapingLocalReferenceExpressions.Add(expression);
    }

    public ISymbol? TryGetScopedOrigin(BoundExpression expression)
    {
        return expression switch
        {
            BoundParameterAccess { Parameter.ScopedKind: not ScopedKind.None } parameterAccess =>
                parameterAccess.Parameter,
            BoundLocalAccess { Local.ScopedKind: not ScopedKind.None } localAccess =>
                localAccess.Local,
            BoundVariableExpression { Variable.ScopedKind: not ScopedKind.None } variable =>
                variable.Variable,
            BoundConversionExpression conversion => TryGetScopedOrigin(conversion.Expression),
            BoundParenthesizedExpression parenthesized => TryGetScopedOrigin(parenthesized.Expression),
            BoundLocalAccess localAccess =>
                _scopedBackedLocals.GetValueOrDefault(localAccess.Local),
            BoundVariableExpression variable =>
                _scopedBackedLocals.GetValueOrDefault(variable.Variable),
            BoundIfExpression conditional =>
                TryGetScopedOrigin(conditional.ThenBranch) ??
                (conditional.ElseBranch is { } elseBranch ? TryGetScopedOrigin(elseBranch) : null),
            BoundInvocationExpression invocation => TryGetScopedInvocationOrigin(invocation),
            _ => null,
        };
    }

    private ISymbol? TryGetScopedInvocationOrigin(BoundInvocationExpression invocation)
    {
        if (!SemanticFacts.MayBeRefLike(invocation.Type))
            return null;

        if (invocation.Receiver is { } receiver &&
            TryGetScopedOrigin(receiver) is { } receiverOrigin)
        {
            return receiverOrigin;
        }

        if (invocation.ExtensionReceiver is { } extensionReceiver &&
            TryGetScopedOrigin(extensionReceiver) is { } extensionReceiverOrigin)
        {
            return extensionReceiverOrigin;
        }

        foreach (var (argument, parameter) in invocation.Arguments.Zip(invocation.Method.Parameters))
        {
            if (parameter.ScopedKind != ScopedKind.None)
                continue;

            if (TryGetScopedOrigin(argument) is { } argumentOrigin)
                return argumentOrigin;
        }

        return null;
    }

    public void AddScopedEscape(
        BoundExpression expression,
        ISymbol origin)
    {
        if (!_escapingScopedExpressions.Any(item =>
                ReferenceEquals(item.Expression, expression) &&
                SymbolEqualityComparer.Default.Equals(item.Origin, origin)))
        {
            _escapingScopedExpressions.Add((expression, origin));
        }
    }

    private static BoundExpression? TryGetResultExpression(BoundNode node)
    {
        return node switch
        {
            BoundBlockStatement block =>
                (block.Statements.LastOrDefault() as BoundExpressionStatement)?.Expression,
            BoundBlockExpression block =>
                (block.Statements.LastOrDefault() as BoundExpressionStatement)?.Expression,
            BoundExpression expression => expression,
            _ => null,
        };
    }
}
