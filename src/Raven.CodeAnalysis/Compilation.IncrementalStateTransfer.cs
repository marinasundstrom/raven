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

        InitializeIncrementalState(previousCompilation.CreateIncrementalState(
            plan.ReusedSyntaxTrees,
            plan.MatchedSyntaxTrees));
        AdoptIncrementalReuseFrom(previousCompilation);

        foreach (var changedTree in plan.ChangedSyntaxTrees)
        {
            RegisterChangedExecutableOwnerDescriptors(changedTree.CurrentTree, changedTree.ChangedOwners);
            RegisterMatchedExecutableOwners(changedTree.CurrentTree, changedTree.MatchedOwners);
        }
    }

    internal IncrementalCompilationState? CreateIncrementalState(
        ImmutableArray<SyntaxTree> reusedSyntaxTrees,
        ImmutableArray<IncrementalMatchedSyntaxTree> matchedSyntaxTrees)
    {
        var state = new IncrementalCompilationState();
        var exactTransferTables = CreateExactTransferTables(state);
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

    private IExactIncrementalStateTransferTable[] CreateExactTransferTables(IncrementalCompilationState state)
        =>
        [
            new ExactIncrementalStateTransferTable<VisibleValueScopeKey, ImmutableArray<VisibleValueDeclarationDescriptor>>(
                _descriptorState.VisibleValueScopeDeclarations,
                state.VisibleValueScopeDeclarations),
            new ExactIncrementalStateTransferTable<NodeInterestSymbolKey, NodeInterestSymbolDescriptor>(
                _descriptorState.NodeInterestSymbolDescriptors,
                state.NodeInterestSymbolDescriptors),
            new ExactIncrementalStateTransferTable<ContextualBindingRootKey, ContextualBindingRootDescriptor>(
                _descriptorState.ContextualBindingRootDescriptors,
                state.ContextualBindingRootDescriptors),
            new ExactIncrementalStateTransferTable<InterestBindingRootKey, InterestBindingRootDescriptor>(
                _descriptorState.InterestBindingRootDescriptors,
                state.InterestBindingRootDescriptors),
            new ExactIncrementalStateTransferTable<ExecutableOwnerKey, ExecutableOwnerDescriptor>(
                _descriptorState.ExecutableOwnerDescriptors,
                state.ExecutableOwnerDescriptors),
            new ExactIncrementalStateTransferTable<FunctionExpressionRebindRootKey, FunctionExpressionRebindRootDescriptor>(
                _descriptorState.FunctionExpressionRebindRootDescriptors,
                state.FunctionExpressionRebindRootDescriptors),
            new ExactIncrementalStateTransferTable<BinderParentAnchorKey, BinderParentAnchorDescriptor>(
                _descriptorState.BinderParentAnchorDescriptors,
                state.BinderParentAnchorDescriptors)
        ];

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
                IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey)
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
