using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal void InitializeIncrementalStateFrom(
        Compilation previousCompilation,
        IncrementalCompilationPlan plan)
    {
        ArgumentNullException.ThrowIfNull(previousCompilation);

        var blockReusedDeclarationSensitiveState =
            plan.BlocksSemanticDiagnosticTransfer ||
            plan.ChangedSyntaxTrees.Any(static tree => tree.BlocksSemanticDiagnosticTransfer);
        InitializeIncrementalState(previousCompilation.CreateIncrementalState(
            plan.ReusedSyntaxTrees,
            plan.MatchedSyntaxTrees,
            blockReusedDeclarationSensitiveState));
        AdoptIncrementalReuseFrom(previousCompilation);

        foreach (var changedTree in plan.ChangedSyntaxTrees)
        {
            RegisterChangedExecutableOwnerDescriptors(changedTree.CurrentTree, changedTree.ChangedOwners);
            RegisterMatchedExecutableOwners(changedTree.CurrentTree, changedTree.MatchedOwners);
            RegisterExecutableOwnerChanges(changedTree.CurrentTree, changedTree.OwnerChanges);

            if (changedTree.BlocksSemanticDiagnosticTransfer)
                RegisterSemanticDiagnosticTransferBlocked(changedTree.CurrentTree);
        }

        if (blockReusedDeclarationSensitiveState)
        {
            foreach (var syntaxTree in plan.ReusedSyntaxTrees)
                RegisterSemanticDiagnosticTransferBlocked(syntaxTree);
        }
    }

    internal IncrementalCompilationState? CreateIncrementalState(
        ImmutableArray<SyntaxTree> reusedSyntaxTrees,
        ImmutableArray<IncrementalMatchedSyntaxTree> matchedSyntaxTrees,
        bool blockReusedDeclarationSensitiveState)
    {
        var state = new IncrementalCompilationState();
        var exactTransferTables = CreateExactTransferTables(
            state,
            includeDeclarationSensitiveDescriptors: !blockReusedDeclarationSensitiveState);
        var ownerRelativeTransferTables = CreateOwnerRelativeTransferTables(state);

        foreach (var syntaxTree in reusedSyntaxTrees)
        {
            foreach (var table in exactTransferTables)
                table.Copy(syntaxTree);
        }

        foreach (var matchedTree in matchedSyntaxTrees)
        {
            foreach (var match in matchedTree.Matches)
            {
                foreach (var table in ownerRelativeTransferTables)
                    table.Copy(matchedTree, match);
            }
        }

        return HasTransferredState(exactTransferTables, ownerRelativeTransferTables) ? state : null;
    }

    private IExactIncrementalStateTransferTable[] CreateExactTransferTables(
        IncrementalCompilationState state,
        bool includeDeclarationSensitiveDescriptors)
    {
        var tables = new List<IExactIncrementalStateTransferTable>
        {
            new ExactIncrementalStateTransferTable<ExecutableOwnerKey, ExecutableOwnerDescriptor>(
                _descriptorState.ExecutableOwnerDescriptors,
                state.ExecutableOwnerDescriptors),
            new ExactIncrementalStateTransferTable<FunctionExpressionRebindRootKey, FunctionExpressionRebindRootDescriptor>(
                _descriptorState.FunctionExpressionRebindRootDescriptors,
                state.FunctionExpressionRebindRootDescriptors),
            new ExactIncrementalStateTransferTable<BinderParentAnchorKey, BinderParentAnchorDescriptor>(
                _descriptorState.BinderParentAnchorDescriptors,
                state.BinderParentAnchorDescriptors)
        };

        if (includeDeclarationSensitiveDescriptors)
        {
            tables.Add(new ExactIncrementalStateTransferTable<VisibleValueScopeKey, ImmutableArray<VisibleValueDeclarationDescriptor>>(
                _descriptorState.VisibleValueScopeDeclarations,
                state.VisibleValueScopeDeclarations));
            tables.Add(new ExactIncrementalStateTransferTable<NodeInterestSymbolKey, NodeInterestSymbolDescriptor>(
                _descriptorState.NodeInterestSymbolDescriptors,
                state.NodeInterestSymbolDescriptors));
            tables.Add(new ExactIncrementalStateTransferTable<ContextualBindingRootKey, ContextualBindingRootDescriptor>(
                _descriptorState.ContextualBindingRootDescriptors,
                state.ContextualBindingRootDescriptors));
            tables.Add(new ExactIncrementalStateTransferTable<InterestBindingRootKey, InterestBindingRootDescriptor>(
                _descriptorState.InterestBindingRootDescriptors,
                state.InterestBindingRootDescriptors));
            tables.Add(new ExactIncrementalStateTransferTable<ExecutableOwnerDescriptor, ImmutableArray<SemanticDiagnosticDescriptor>>(
                _descriptorState.SemanticDiagnosticsByOwner,
                state.SemanticDiagnosticsByOwner));
        }

        return tables.ToArray();
    }

    private IOwnerRelativeIncrementalStateTransferTable[] CreateOwnerRelativeTransferTables(IncrementalCompilationState state)
        =>
        [
            new OwnerRelativeIncrementalStateTransferTable<ImmutableArray<VisibleValueDeclarationDescriptor>>(
                _descriptorState.VisibleValueScopeDeclarationsByOwner,
                state.VisibleValueScopeDeclarationsByOwner,
                IncrementalBindingStateTransferPolicy.TryRemapDeclarationSensitiveDescriptorKey),
            new OwnerRelativeIncrementalStateTransferTable<NodeInterestSymbolDescriptor>(
                _descriptorState.NodeInterestSymbolDescriptorsByOwner,
                state.NodeInterestSymbolDescriptorsByOwner,
                IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey),
            new OwnerRelativeIncrementalStateTransferTable<ContextualBindingRootDescriptor>(
                _descriptorState.ContextualBindingRootDescriptorsByOwner,
                state.ContextualBindingRootDescriptorsByOwner,
                IncrementalBindingStateTransferPolicy.TryRemapDeclarationSensitiveDescriptorKey),
            new OwnerRelativeIncrementalStateTransferTable<InterestBindingRootDescriptor>(
                _descriptorState.InterestBindingRootDescriptorsByOwner,
                state.InterestBindingRootDescriptorsByOwner,
                IncrementalBindingStateTransferPolicy.TryRemapDeclarationSensitiveDescriptorKey),
            new OwnerRelativeIncrementalStateTransferTable<FunctionExpressionRebindRootDescriptor>(
                _descriptorState.FunctionExpressionRebindRootDescriptorsByOwner,
                state.FunctionExpressionRebindRootDescriptorsByOwner,
                IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey),
            new OwnerRelativeIncrementalStateTransferTable<BinderParentAnchorDescriptor>(
                _descriptorState.BinderParentAnchorDescriptorsByOwner,
                state.BinderParentAnchorDescriptorsByOwner,
                IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey),
            new SemanticDiagnosticsIncrementalStateTransferTable(
                _descriptorState.SemanticDiagnosticsByRelativeOwner,
                state.SemanticDiagnosticsByRelativeOwner)
        ];

    private interface IExactIncrementalStateTransferTable
    {
        bool HasTransferredState { get; }

        void Copy(SyntaxTree syntaxTree);
    }

    private sealed class ExactIncrementalStateTransferTable<TKey, TValue> : IExactIncrementalStateTransferTable
        where TKey : notnull
    {
        private readonly IDictionary<SyntaxTree, ConcurrentDictionary<TKey, TValue>> _source;
        private readonly IDictionary<SyntaxTree, Dictionary<TKey, TValue>> _destination;

        public ExactIncrementalStateTransferTable(
            IDictionary<SyntaxTree, ConcurrentDictionary<TKey, TValue>> source,
            IDictionary<SyntaxTree, Dictionary<TKey, TValue>> destination)
        {
            _source = source;
            _destination = destination;
        }

        public bool HasTransferredState => _destination.Count != 0;

        public void Copy(SyntaxTree syntaxTree)
        {
            if (!_source.TryGetValue(syntaxTree, out var values) || values.Count == 0)
                return;

            _destination[syntaxTree] = new Dictionary<TKey, TValue>(values);
        }
    }

    private interface IOwnerRelativeIncrementalStateTransferTable
    {
        bool HasTransferredState { get; }

        void Copy(IncrementalMatchedSyntaxTree matchedTree, MatchedExecutableOwner match);
    }

    private sealed class OwnerRelativeIncrementalStateTransferTable<TValue> : IOwnerRelativeIncrementalStateTransferTable
    {
        private readonly IDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, TValue>> _source;
        private readonly IDictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, TValue>> _destination;
        private readonly TryRemapOwnerRelativeDescriptorKeyDelegate _tryRemapKey;

        public OwnerRelativeIncrementalStateTransferTable(
            IDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, TValue>> source,
            IDictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, TValue>> destination,
            TryRemapOwnerRelativeDescriptorKeyDelegate tryRemapKey)
        {
            _source = source;
            _destination = destination;
            _tryRemapKey = tryRemapKey;
        }

        public bool HasTransferredState => _destination.Count != 0;

        public void Copy(IncrementalMatchedSyntaxTree matchedTree, MatchedExecutableOwner match)
        {
            if (!_source.TryGetValue(matchedTree.PreviousTree, out var values) || values.Count == 0)
                return;

            var ownerChange = TryGetOwnerChange(matchedTree.OwnerChanges, match.CurrentOwner, out var change)
                ? change
                : (OwnerRelativeTextChange?)null;
            Dictionary<OwnerRelativeDescriptorKey, TValue>? remappedValues = null;

            foreach (var (key, value) in values)
            {
                if (key.Owner != match.PreviousOwner)
                    continue;

                if (!_tryRemapKey(
                        key,
                        matchedTree.PreviousTree,
                        matchedTree.CurrentTree,
                        match.CurrentOwner,
                        ownerChange,
                        out var remappedKey))
                {
                    continue;
                }

                remappedValues ??= _destination.TryGetValue(matchedTree.CurrentTree, out var existing)
                    ? existing
                    : new Dictionary<OwnerRelativeDescriptorKey, TValue>();

                remappedValues[remappedKey] = value;
            }

            if (remappedValues is not null)
                _destination[matchedTree.CurrentTree] = remappedValues;
        }
    }

    private sealed class SemanticDiagnosticsIncrementalStateTransferTable : IOwnerRelativeIncrementalStateTransferTable
    {
        private readonly IDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>> _source;
        private readonly IDictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>> _destination;

        public SemanticDiagnosticsIncrementalStateTransferTable(
            IDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>> source,
            IDictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>> destination)
        {
            _source = source;
            _destination = destination;
        }

        public bool HasTransferredState => _destination.Count != 0;

        public void Copy(IncrementalMatchedSyntaxTree matchedTree, MatchedExecutableOwner match)
        {
            if (!_source.TryGetValue(matchedTree.PreviousTree, out var values) || values.Count == 0)
                return;

            var ownerChange = TryGetOwnerChange(matchedTree.OwnerChanges, match.CurrentOwner, out var change)
                ? change
                : (OwnerRelativeTextChange?)null;

            Dictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>? remappedValues = null;

            foreach (var (key, value) in values)
            {
                if (key.Owner != match.PreviousOwner ||
                    !IsSemanticDiagnosticOwnerKey(key))
                {
                    continue;
                }

                if (ownerChange is { } semanticOwnerChange &&
                    (!value.IsEmpty || !HasChangedNestedOwner(matchedTree.OwnerChanges, match.CurrentOwner, semanticOwnerChange)))
                {
                    continue;
                }

                var remappedKey = new OwnerRelativeDescriptorKey(
                    match.CurrentOwner,
                    RelativeStart: 0,
                    match.CurrentOwner.Span.Length,
                    match.CurrentOwner.Kind);

                remappedValues ??= _destination.TryGetValue(matchedTree.CurrentTree, out var existing)
                    ? existing
                    : new Dictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>();

                remappedValues[remappedKey] = value;
            }

            if (remappedValues is not null)
                _destination[matchedTree.CurrentTree] = remappedValues;
        }

        private static bool IsSemanticDiagnosticOwnerKey(OwnerRelativeDescriptorKey key)
            => key.RelativeStart == 0 &&
               key.Length == key.Owner.Span.Length &&
               key.Kind == key.Owner.Kind;

        private static bool HasChangedNestedOwner(
            ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange> ownerChanges,
            ExecutableOwnerDescriptor owner,
            OwnerRelativeTextChange ownerChange)
        {
            var changedSpan = new Text.TextSpan(owner.Span.Start + ownerChange.CurrentSpan.Start, ownerChange.CurrentSpan.Length);
            return ownerChanges.Keys.Any(candidate =>
                candidate != owner &&
                ContainsSpan(owner.Span, candidate.Span) &&
                ContainsOrTouchesInsertion(candidate.Span, changedSpan));
        }

        private static bool ContainsOrTouchesInsertion(Text.TextSpan container, Text.TextSpan span)
        {
            if (span.Length == 0)
                return span.Start >= container.Start && span.Start <= container.End + 1;

            return span.Start >= container.Start && span.End <= container.End;
        }

        private static bool ContainsSpan(Text.TextSpan container, Text.TextSpan span)
            => span.Start >= container.Start && span.End <= container.End;
    }

    private delegate bool TryRemapOwnerRelativeDescriptorKeyDelegate(
        OwnerRelativeDescriptorKey key,
        SyntaxTree previousTree,
        SyntaxTree currentTree,
        ExecutableOwnerDescriptor currentOwner,
        OwnerRelativeTextChange? ownerChange,
        out OwnerRelativeDescriptorKey remappedKey);

    private static bool TryGetOwnerChange(
        ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange> ownerChanges,
        ExecutableOwnerDescriptor owner,
        out OwnerRelativeTextChange change)
        => ownerChanges.TryGetValue(owner, out change);

    private static bool HasTransferredState(
        IEnumerable<IExactIncrementalStateTransferTable> exactTransferTables,
        IEnumerable<IOwnerRelativeIncrementalStateTransferTable> ownerRelativeTransferTables)
        => exactTransferTables.Any(static table => table.HasTransferredState) ||
           ownerRelativeTransferTables.Any(static table => table.HasTransferredState);
}
