using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Moves suspending catch and finally bodies out of CLR exception handlers before the
/// generated async state machine is built. CLR handlers cannot be left and resumed by
/// the state dispatch branch, so their observable control flow is recorded in locals
/// and replayed from ordinary code after the protected region.
/// </summary>
internal sealed class AsyncProtectedRegionLowerer : BoundTreeRewriter
{
    private readonly ISymbol _containingSymbol;
    private readonly Compilation _compilation;
    private int _localOrdinal;
    private int _labelOrdinal;
    private bool _needsFinalThrow;

    private AsyncProtectedRegionLowerer(ISymbol containingSymbol, Compilation compilation)
    {
        _containingSymbol = containingSymbol;
        _compilation = compilation;
    }

    public static BoundBlockStatement Rewrite(ISymbol containingSymbol, BoundBlockStatement body)
    {
        if (containingSymbol.ContainingAssembly is not SourceAssemblySymbol sourceAssembly)
            return body;

        var rewriter = new AsyncProtectedRegionLowerer(
            containingSymbol,
            sourceAssembly.Compilation);
        var rewritten = (BoundBlockStatement)rewriter.VisitBlockStatement(body)!;
        if (!rewriter._needsFinalThrow)
            return rewritten;

        var statements = rewritten.Statements.ToList();
        if (containingSymbol is IMethodSymbol method &&
            AsyncReturnTypeUtilities.ExtractAsyncResultType(sourceAssembly.Compilation, method.ReturnType) is
            { SpecialType: SpecialType.System_Unit or SpecialType.System_Void })
        {
            // Raven does not synthesize a source-level return for a Task method that
            // reaches the end of its body. Make that normal path explicit before the
            // verifier sentinel so both async backends complete successfully.
            statements.Add(new BoundReturnStatement(null));
        }

        statements.Add(new BoundThrowStatement(new BoundDefaultValueExpression(
            sourceAssembly.Compilation.GetSpecialType(SpecialType.System_Exception))));
        return new BoundBlockStatement(statements, rewritten.LocalsToDispose);
    }

    public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;

    public override BoundNode? VisitFunctionStatement(BoundFunctionStatement node) => node;

    public override BoundNode? VisitTryStatement(BoundTryStatement node)
    {
        var tryBlock = (BoundBlockStatement)VisitBlockStatement(node.TryBlock)!;
        var catches = node.CatchClauses
            .Select(clause => new BoundCatchClause(
                clause.ExceptionType,
                clause.Local,
                clause.Pattern,
                clause.Guard,
                (BoundBlockStatement)VisitBlockStatement(clause.Block)!))
            .ToImmutableArray();
        var finallyBlock = node.FinallyBlock is null
            ? null
            : (BoundBlockStatement)VisitBlockStatement(node.FinallyBlock)!;

        BoundStatement protectedBody = RewriteSuspendingCatches(tryBlock, catches, node.Kind);

        if (finallyBlock is null)
            return protectedBody;

        if (!AsyncLowerer.ContainsAwait(finallyBlock))
        {
            var block = protectedBody as BoundBlockStatement
                ?? new BoundBlockStatement([protectedBody]);
            return new BoundTryStatement(
                block,
                ImmutableArray<BoundCatchClause>.Empty,
                finallyBlock,
                node.Kind);
        }

        return RewriteSuspendingFinally(protectedBody, finallyBlock, node.Kind);
    }

    private BoundStatement RewriteSuspendingCatches(
        BoundBlockStatement tryBlock,
        ImmutableArray<BoundCatchClause> catches,
        BoundTryStatementKind kind)
    {
        if (catches.IsDefaultOrEmpty)
            return tryBlock;

        if (!catches.Any(static clause => AsyncLowerer.ContainsAwait(clause.Block)))
            return new BoundTryStatement(tryBlock, catches, finallyBlock: null, kind);

        _needsFinalThrow = true;

        var intType = _compilation.GetSpecialType(SpecialType.System_Int32);
        var catchId = CreateLocal("asyncCatch", intType);
        var declarations = new List<BoundVariableDeclarator>
        {
            new(catchId, CreateIntLiteral(0))
        };
        var rewrittenCatches = ImmutableArray.CreateBuilder<BoundCatchClause>(catches.Length);
        var dispatches = new List<BoundStatement>();
        var nextCatchId = 1;

        foreach (var clause in catches)
        {
            if (!AsyncLowerer.ContainsAwait(clause.Block))
            {
                rewrittenCatches.Add(clause);
                continue;
            }

            var handlerMap = new Dictionary<ILocalSymbol, ILocalSymbol>(ReferenceEqualityComparer.Instance);
            var storageMap = new Dictionary<ILocalSymbol, ILocalSymbol>(ReferenceEqualityComparer.Instance);
            foreach (var local in GetCatchLocals(clause))
            {
                var handlerLocal = CreateLocal("asyncCatchHandler", local.Type);
                var storageLocal = CreateLocal("asyncCatchValue", local.Type);
                handlerMap.Add(local, handlerLocal);
                storageMap.Add(local, storageLocal);
                declarations.Add(new BoundVariableDeclarator(storageLocal, new BoundDefaultValueExpression(local.Type)));
            }

            var handlerStatements = new List<BoundStatement>();
            foreach (var pair in storageMap)
            {
                handlerStatements.Add(CreateAssignment(
                    pair.Value,
                    new BoundLocalAccess(handlerMap[pair.Key])));
            }
            handlerStatements.Add(CreateAssignment(catchId, CreateIntLiteral(nextCatchId)));

            var handlerMapper = new LocalSubstitutionRewriter(handlerMap);
            rewrittenCatches.Add(new BoundCatchClause(
                clause.ExceptionType,
                clause.Local is null ? null : handlerMap[clause.Local],
                (BoundPattern?)handlerMapper.Visit(clause.Pattern),
                (BoundExpression?)handlerMapper.Visit(clause.Guard),
                new BoundBlockStatement(handlerStatements)));

            var movedBody = (BoundBlockStatement)new LocalSubstitutionRewriter(storageMap)
                .VisitBlockStatement(clause.Block)!;
            dispatches.Add(new BoundIfStatement(
                CreateIntEquality(catchId, nextCatchId),
                movedBody));
            nextCatchId++;
        }

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement(declarations),
            new BoundTryStatement(tryBlock, rewrittenCatches.MoveToImmutable(), finallyBlock: null, kind)
        };
        statements.AddRange(dispatches);
        return new BoundBlockStatement(statements);
    }

    private BoundStatement RewriteSuspendingFinally(
        BoundStatement protectedBody,
        BoundBlockStatement finallyBlock,
        BoundTryStatementKind kind)
    {
        _needsFinalThrow = true;
        var protectedBlock = protectedBody as BoundBlockStatement
            ?? new BoundBlockStatement([protectedBody]);
        var labels = LabelCollector.Collect(protectedBlock);
        var exits = ExitCollector.Collect(protectedBlock, labels);
        var intType = _compilation.GetSpecialType(SpecialType.System_Int32);
        var exceptionType = _compilation.GetSpecialType(SpecialType.System_Exception);
        var exitKind = CreateLocal("asyncFinallyExit", intType);
        var pendingException = CreateLocal("asyncFinallyException", exceptionType);
        var returnValue = exits.ReturnType is null ? null : CreateLocal("asyncFinallyReturn", exits.ReturnType);
        var exitLabel = CreateProtectedRegionExitLabel("asyncFinallyExit");
        var gotoIds = new Dictionary<ILabelSymbol, int>(SymbolEqualityComparer.Default);
        for (var index = 0; index < exits.ExternalGotoTargets.Length; index++)
            gotoIds.Add(exits.ExternalGotoTargets[index], index + 2);

        var rewrittenProtectedBlock = (BoundBlockStatement)new ProtectedExitRewriter(
            exitKind,
            returnValue,
            exitLabel,
            gotoIds,
            _compilation).VisitBlockStatement(protectedBlock)!;

        var caughtException = CreateLocal("asyncFinallyCaught", exceptionType);
        var catchBlock = new BoundBlockStatement([
            CreateAssignment(pendingException, new BoundLocalAccess(caughtException))
        ]);
        var catchClause = new BoundCatchClause(
            exceptionType,
            caughtException,
            pattern: null,
            guard: null,
            catchBlock);

        var declarations = new List<BoundVariableDeclarator>
        {
            new(exitKind, CreateIntLiteral(0)),
            new(pendingException, new BoundDefaultValueExpression(exceptionType))
        };
        if (returnValue is not null)
            declarations.Add(new BoundVariableDeclarator(returnValue, new BoundDefaultValueExpression(returnValue.Type)));

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement(declarations),
            new BoundTryStatement(
                rewrittenProtectedBlock,
                ImmutableArray.Create(catchClause),
                finallyBlock: null,
                kind),
            new BoundLabeledStatement(exitLabel, finallyBlock),
            new BoundIfStatement(
                CreateNotNull(pendingException),
                new BoundBlockStatement([
                    new BoundThrowStatement(new BoundLocalAccess(pendingException))
                ]))
        };

        if (exits.HasReturn)
        {
            statements.Add(new BoundIfStatement(
                CreateIntEquality(exitKind, 1),
                new BoundBlockStatement([
                    new BoundReturnStatement(returnValue is null ? null : new BoundLocalAccess(returnValue))
                ])));
        }

        foreach (var pair in gotoIds.OrderBy(static pair => pair.Value))
        {
            statements.Add(new BoundIfStatement(
                CreateIntEquality(exitKind, pair.Value),
                new BoundBlockStatement([new BoundGotoStatement(pair.Key)])));
        }

        return new BoundBlockStatement(statements);
    }

    private IEnumerable<ILocalSymbol> GetCatchLocals(BoundCatchClause clause)
    {
        var seen = new HashSet<ILocalSymbol>(ReferenceEqualityComparer.Instance);
        if (clause.Local is not null && seen.Add(clause.Local))
            yield return clause.Local;

        if (clause.Pattern is null)
            yield break;

        foreach (var designator in clause.Pattern.GetDesignators())
        {
            if (designator is BoundSingleVariableDesignator single && seen.Add(single.Local))
                yield return single.Local;
        }
    }

    private SourceLocalSymbol CreateLocal(string prefix, ITypeSymbol type)
    {
        var name = $"<{prefix}>__{_localOrdinal++}";
        return new SourceLocalSymbol(
            name,
            type,
            isMutable: true,
            _containingSymbol,
            _containingSymbol.ContainingType,
            _containingSymbol.ContainingNamespace,
            [Location.None],
            Array.Empty<SyntaxReference>(),
            isImplicitlyDeclared: true);
    }

    private AsyncProtectedRegionExitLabelSymbol CreateProtectedRegionExitLabel(string prefix)
        => new(
            $"<{prefix}>__{_labelOrdinal++}",
            _containingSymbol,
            _containingSymbol.ContainingType,
            _containingSymbol.ContainingNamespace,
            [Location.None],
            Array.Empty<SyntaxReference>());

    private BoundExpression CreateIntEquality(ILocalSymbol local, int value)
    {
        var left = new BoundLocalAccess(local);
        var right = CreateIntLiteral(value);
        if (!BoundBinaryOperator.TryLookup(
                _compilation,
                SyntaxKind.EqualsEqualsToken,
                left.Type!,
                right.Type!,
                out var equals))
        {
            throw new InvalidOperationException("Async protected-region lowering requires integer equality.");
        }

        return new BoundBinaryExpression(left, equals, right);
    }

    private BoundExpression CreateNotNull(ILocalSymbol local)
    {
        var left = new BoundLocalAccess(local);
        var right = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NullLiteral,
            null!,
            local.Type);
        if (!BoundBinaryOperator.TryLookup(
                _compilation,
                SyntaxKind.NotEqualsToken,
                local.Type,
                local.Type,
                out var notEquals))
        {
            throw new InvalidOperationException("Async protected-region lowering requires reference inequality.");
        }

        return new BoundBinaryExpression(left, notEquals, right);
    }

    private BoundLiteralExpression CreateIntLiteral(int value)
        => new(
            BoundLiteralExpressionKind.NumericLiteral,
            value,
            _compilation.GetSpecialType(SpecialType.System_Int32));

    private BoundStatement CreateAssignment(ILocalSymbol local, BoundExpression value)
        => new BoundExpressionStatement(new BoundLocalAssignmentExpression(
            local,
            new BoundLocalAccess(local),
            value,
            _compilation.GetSpecialType(SpecialType.System_Unit)));

    private sealed class LocalSubstitutionRewriter(
        IReadOnlyDictionary<ILocalSymbol, ILocalSymbol> replacements) : BoundTreeRewriter
    {
        public override ILocalSymbol VisitLocal(ILocalSymbol local)
            => replacements.TryGetValue(local, out var replacement) ? replacement : local;

        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;

        public override BoundNode? VisitFunctionStatement(BoundFunctionStatement node) => node;
    }

    private sealed class LabelCollector : BoundTreeWalker
    {
        private readonly HashSet<ILabelSymbol> _labels = new(SymbolEqualityComparer.Default);

        public static HashSet<ILabelSymbol> Collect(BoundNode node)
        {
            var collector = new LabelCollector();
            collector.Visit(node);
            return collector._labels;
        }

        public override void VisitLabeledStatement(BoundLabeledStatement node)
        {
            _labels.Add(node.Label);
            base.VisitLabeledStatement(node);
        }

        public override void VisitFunctionExpression(BoundFunctionExpression node)
        {
        }

        public override void VisitFunctionStatement(BoundFunctionStatement node)
        {
        }
    }

    private sealed class ExitCollector : BoundTreeWalker
    {
        private readonly HashSet<ILabelSymbol> _internalLabels;
        private readonly List<ILabelSymbol> _externalGotoTargets = new();

        private ExitCollector(HashSet<ILabelSymbol> internalLabels)
        {
            _internalLabels = internalLabels;
        }

        public bool HasReturn { get; private set; }

        public ITypeSymbol? ReturnType { get; private set; }

        public ImmutableArray<ILabelSymbol> ExternalGotoTargets => _externalGotoTargets.ToImmutableArray();

        public static ExitCollector Collect(BoundNode node, HashSet<ILabelSymbol> internalLabels)
        {
            var collector = new ExitCollector(internalLabels);
            collector.Visit(node);
            return collector;
        }

        public override void VisitReturnStatement(BoundReturnStatement node)
        {
            HasReturn = true;
            ReturnType ??= node.Expression?.Type;
            base.VisitReturnStatement(node);
        }

        public override void VisitGotoStatement(BoundGotoStatement node)
        {
            if (!_internalLabels.Contains(node.Target) &&
                !_externalGotoTargets.Contains(node.Target, SymbolEqualityComparer.Default))
            {
                _externalGotoTargets.Add(node.Target);
            }
        }

        public override void VisitFunctionExpression(BoundFunctionExpression node)
        {
        }

        public override void VisitFunctionStatement(BoundFunctionStatement node)
        {
        }
    }

    private sealed class ProtectedExitRewriter(
        ILocalSymbol exitKind,
        ILocalSymbol? returnValue,
        ILabelSymbol exitLabel,
        IReadOnlyDictionary<ILabelSymbol, int> gotoIds,
        Compilation compilation) : BoundTreeRewriter
    {
        private readonly ITypeSymbol _unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        private readonly ITypeSymbol _intType = compilation.GetSpecialType(SpecialType.System_Int32);

        public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
        {
            var statements = new List<BoundStatement>();
            if (node.Expression is not null && returnValue is not null)
            {
                statements.Add(CreateAssignment(returnValue, (BoundExpression)VisitExpression(node.Expression)!));
            }

            statements.Add(CreateAssignment(exitKind, CreateIntLiteral(1)));
            statements.Add(new BoundGotoStatement(exitLabel));
            return new BoundBlockStatement(statements);
        }

        public override BoundNode? VisitGotoStatement(BoundGotoStatement node)
        {
            if (!gotoIds.TryGetValue(node.Target, out var id))
                return node;

            return new BoundBlockStatement([
                CreateAssignment(exitKind, CreateIntLiteral(id)),
                new BoundGotoStatement(exitLabel)
            ]);
        }

        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;

        public override BoundNode? VisitFunctionStatement(BoundFunctionStatement node) => node;

        private BoundStatement CreateAssignment(ILocalSymbol local, BoundExpression value)
            => new BoundExpressionStatement(new BoundLocalAssignmentExpression(
                local,
                new BoundLocalAccess(local),
                value,
                _unitType));

        private BoundLiteralExpression CreateIntLiteral(int value)
            => new(BoundLiteralExpressionKind.NumericLiteral, value, _intType);
    }
}

internal sealed class AsyncProtectedRegionExitLabelSymbol : LabelSymbol
{
    public AsyncProtectedRegionExitLabelSymbol(
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences)
        : base(name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }
}
