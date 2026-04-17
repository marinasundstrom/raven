using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private IncrementalCompilationState? _incrementalState;

    internal sealed class DescriptorState
    {
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<VisibleValueScopeKey, ImmutableArray<VisibleValueDeclarationDescriptor>>> VisibleValueScopeDeclarations { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, ImmutableArray<VisibleValueDeclarationDescriptor>>> VisibleValueScopeDeclarationsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<NodeInterestSymbolKey, NodeInterestSymbolDescriptor>> NodeInterestSymbolDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, NodeInterestSymbolDescriptor>> NodeInterestSymbolDescriptorsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<ContextualBindingRootKey, ContextualBindingRootDescriptor>> ContextualBindingRootDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, ContextualBindingRootDescriptor>> ContextualBindingRootDescriptorsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<InterestBindingRootKey, InterestBindingRootDescriptor>> InterestBindingRootDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, InterestBindingRootDescriptor>> InterestBindingRootDescriptorsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<ExecutableOwnerKey, ExecutableOwnerDescriptor>> ExecutableOwnerDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<FunctionExpressionRebindRootKey, FunctionExpressionRebindRootDescriptor>> FunctionExpressionRebindRootDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, FunctionExpressionRebindRootDescriptor>> FunctionExpressionRebindRootDescriptorsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<BinderParentAnchorKey, BinderParentAnchorDescriptor>> BinderParentAnchorDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, BinderParentAnchorDescriptor>> BinderParentAnchorDescriptorsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ImmutableHashSet<ExecutableOwnerDescriptor>> ChangedExecutableOwnerDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<ExecutableOwnerDescriptor, MatchedExecutableOwner>> MatchedExecutableOwners { get; } = new();
    }

    internal readonly record struct IncrementalMatchedSyntaxTree(
        SyntaxTree CurrentTree,
        SyntaxTree PreviousTree,
        ImmutableArray<MatchedExecutableOwner> Matches,
        ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange> OwnerChanges);

    internal readonly record struct OwnerRelativeTextChange(
        Text.TextSpan PreviousSpan,
        Text.TextSpan CurrentSpan);

    internal sealed class IncrementalCompilationState
    {
        public Dictionary<SyntaxTree, Dictionary<VisibleValueScopeKey, ImmutableArray<VisibleValueDeclarationDescriptor>>> VisibleValueScopeDeclarations { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, ImmutableArray<VisibleValueDeclarationDescriptor>>> VisibleValueScopeDeclarationsByOwner { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<NodeInterestSymbolKey, NodeInterestSymbolDescriptor>> NodeInterestSymbolDescriptors { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, NodeInterestSymbolDescriptor>> NodeInterestSymbolDescriptorsByOwner { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<ContextualBindingRootKey, ContextualBindingRootDescriptor>> ContextualBindingRootDescriptors { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, ContextualBindingRootDescriptor>> ContextualBindingRootDescriptorsByOwner { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<InterestBindingRootKey, InterestBindingRootDescriptor>> InterestBindingRootDescriptors { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, InterestBindingRootDescriptor>> InterestBindingRootDescriptorsByOwner { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<ExecutableOwnerKey, ExecutableOwnerDescriptor>> ExecutableOwnerDescriptors { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<FunctionExpressionRebindRootKey, FunctionExpressionRebindRootDescriptor>> FunctionExpressionRebindRootDescriptors { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, FunctionExpressionRebindRootDescriptor>> FunctionExpressionRebindRootDescriptorsByOwner { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<BinderParentAnchorKey, BinderParentAnchorDescriptor>> BinderParentAnchorDescriptors { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, BinderParentAnchorDescriptor>> BinderParentAnchorDescriptorsByOwner { get; } = new();

        public bool TryGetVisibleValueScopeDeclarations(SyntaxTree syntaxTree, VisibleValueScopeKey key, out ImmutableArray<VisibleValueDeclarationDescriptor> declarations)
            => TryGetExact(VisibleValueScopeDeclarations, syntaxTree, key, out declarations);

        public bool TryGetVisibleValueScopeDeclarations(SyntaxTree syntaxTree, OwnerRelativeDescriptorKey key, out ImmutableArray<VisibleValueDeclarationDescriptor> declarations)
            => TryGetExact(VisibleValueScopeDeclarationsByOwner, syntaxTree, key, out declarations);

        public bool TryGetNodeInterestSymbolDescriptor(SyntaxTree syntaxTree, NodeInterestSymbolKey key, out NodeInterestSymbolDescriptor descriptor)
            => TryGetExact(NodeInterestSymbolDescriptors, syntaxTree, key, out descriptor);

        public bool TryGetNodeInterestSymbolDescriptor(SyntaxTree syntaxTree, OwnerRelativeDescriptorKey key, out NodeInterestSymbolDescriptor descriptor)
            => TryGetExact(NodeInterestSymbolDescriptorsByOwner, syntaxTree, key, out descriptor);

        public bool TryGetContextualBindingRootDescriptor(SyntaxTree syntaxTree, ContextualBindingRootKey key, out ContextualBindingRootDescriptor descriptor)
            => TryGetExact(ContextualBindingRootDescriptors, syntaxTree, key, out descriptor);

        public bool TryGetContextualBindingRootDescriptor(SyntaxTree syntaxTree, OwnerRelativeDescriptorKey key, out ContextualBindingRootDescriptor descriptor)
            => TryGetExact(ContextualBindingRootDescriptorsByOwner, syntaxTree, key, out descriptor);

        public bool TryGetInterestBindingRootDescriptor(SyntaxTree syntaxTree, InterestBindingRootKey key, out InterestBindingRootDescriptor descriptor)
            => TryGetExact(InterestBindingRootDescriptors, syntaxTree, key, out descriptor);

        public bool TryGetInterestBindingRootDescriptor(SyntaxTree syntaxTree, OwnerRelativeDescriptorKey key, out InterestBindingRootDescriptor descriptor)
            => TryGetExact(InterestBindingRootDescriptorsByOwner, syntaxTree, key, out descriptor);

        public bool TryGetExecutableOwnerDescriptor(SyntaxTree syntaxTree, ExecutableOwnerKey key, out ExecutableOwnerDescriptor descriptor)
            => TryGetExact(ExecutableOwnerDescriptors, syntaxTree, key, out descriptor);

        public bool TryGetFunctionExpressionRebindRootDescriptor(SyntaxTree syntaxTree, FunctionExpressionRebindRootKey key, out FunctionExpressionRebindRootDescriptor descriptor)
            => TryGetExact(FunctionExpressionRebindRootDescriptors, syntaxTree, key, out descriptor);

        public bool TryGetFunctionExpressionRebindRootDescriptor(SyntaxTree syntaxTree, OwnerRelativeDescriptorKey key, out FunctionExpressionRebindRootDescriptor descriptor)
            => TryGetExact(FunctionExpressionRebindRootDescriptorsByOwner, syntaxTree, key, out descriptor);

        public bool TryGetBinderParentAnchorDescriptor(SyntaxTree syntaxTree, BinderParentAnchorKey key, out BinderParentAnchorDescriptor descriptor)
            => TryGetExact(BinderParentAnchorDescriptors, syntaxTree, key, out descriptor);

        public bool TryGetBinderParentAnchorDescriptor(SyntaxTree syntaxTree, OwnerRelativeDescriptorKey key, out BinderParentAnchorDescriptor descriptor)
            => TryGetExact(BinderParentAnchorDescriptorsByOwner, syntaxTree, key, out descriptor);

        private static bool TryGetExact<TKey, TValue>(
            Dictionary<SyntaxTree, Dictionary<TKey, TValue>> storage,
            SyntaxTree syntaxTree,
            TKey key,
            out TValue value)
            where TKey : notnull
        {
            value = default!;
            return storage.TryGetValue(syntaxTree, out var values) &&
                   values.TryGetValue(key, out value);
        }
    }

    internal void InitializeIncrementalState(IncrementalCompilationState? state)
    {
        _incrementalState = state;
    }

    internal IncrementalCompilationState? CreateIncrementalState(
        ImmutableArray<SyntaxTree> reusedSyntaxTrees,
        ImmutableArray<IncrementalMatchedSyntaxTree> matchedSyntaxTrees)
    {
        var state = new IncrementalCompilationState();

        foreach (var syntaxTree in reusedSyntaxTrees)
        {
            CopyExactState(_descriptorState.VisibleValueScopeDeclarations, state.VisibleValueScopeDeclarations, syntaxTree);
            CopyExactState(_descriptorState.NodeInterestSymbolDescriptors, state.NodeInterestSymbolDescriptors, syntaxTree);
            CopyExactState(_descriptorState.ContextualBindingRootDescriptors, state.ContextualBindingRootDescriptors, syntaxTree);
            CopyExactState(_descriptorState.InterestBindingRootDescriptors, state.InterestBindingRootDescriptors, syntaxTree);
            CopyExactState(_descriptorState.ExecutableOwnerDescriptors, state.ExecutableOwnerDescriptors, syntaxTree);
            CopyExactState(_descriptorState.FunctionExpressionRebindRootDescriptors, state.FunctionExpressionRebindRootDescriptors, syntaxTree);
            CopyExactState(_descriptorState.BinderParentAnchorDescriptors, state.BinderParentAnchorDescriptors, syntaxTree);
        }

        foreach (var matchedTree in matchedSyntaxTrees)
        {
            foreach (var match in matchedTree.Matches)
            {
                if (matchedTree.OwnerChanges.ContainsKey(match.CurrentOwner))
                {
                    // Any edit within an executable owner can invalidate semantic anchors
                    // anywhere else in that owner through local declaration order,
                    // contextual lambda binding, or symbol-interest mappings.
                    // Rebind the owner from scratch instead of trying to preserve
                    // owner-relative semantic descriptors around the edited span.
                    continue;
                }

                // Visible value scopes and node-interest symbol descriptors encode semantic state.
                // Reusing them across an edited executable owner can preserve stale symbol mappings
                // even when spans remap cleanly. Let the next compilation recompute them.
                CopyOwnerRelativeState(
                    _descriptorState.ContextualBindingRootDescriptorsByOwner,
                    state.ContextualBindingRootDescriptorsByOwner,
                    matchedTree.PreviousTree,
                    matchedTree.CurrentTree,
                    match.PreviousOwner,
                    match.CurrentOwner,
                    TryGetOwnerChange(matchedTree.OwnerChanges, match.CurrentOwner, out var contextualOwnerChange)
                        ? contextualOwnerChange
                        : null);
                CopyOwnerRelativeState(
                    _descriptorState.InterestBindingRootDescriptorsByOwner,
                    state.InterestBindingRootDescriptorsByOwner,
                    matchedTree.PreviousTree,
                    matchedTree.CurrentTree,
                    match.PreviousOwner,
                    match.CurrentOwner,
                    TryGetOwnerChange(matchedTree.OwnerChanges, match.CurrentOwner, out var interestOwnerChange)
                        ? interestOwnerChange
                        : null);
                CopyOwnerRelativeState(
                    _descriptorState.FunctionExpressionRebindRootDescriptorsByOwner,
                    state.FunctionExpressionRebindRootDescriptorsByOwner,
                    matchedTree.PreviousTree,
                    matchedTree.CurrentTree,
                    match.PreviousOwner,
                    match.CurrentOwner,
                    TryGetOwnerChange(matchedTree.OwnerChanges, match.CurrentOwner, out var rebindOwnerChange)
                        ? rebindOwnerChange
                        : null);
                CopyOwnerRelativeState(
                    _descriptorState.BinderParentAnchorDescriptorsByOwner,
                    state.BinderParentAnchorDescriptorsByOwner,
                    matchedTree.PreviousTree,
                    matchedTree.CurrentTree,
                    match.PreviousOwner,
                    match.CurrentOwner,
                    TryGetOwnerChange(matchedTree.OwnerChanges, match.CurrentOwner, out var binderOwnerChange)
                        ? binderOwnerChange
                        : null);
            }
        }

        return HasTransferredState(state) ? state : null;
    }

    private static void CopyExactState<TKey, TValue>(
        IDictionary<SyntaxTree, ConcurrentDictionary<TKey, TValue>> source,
        IDictionary<SyntaxTree, Dictionary<TKey, TValue>> destination,
        SyntaxTree syntaxTree)
        where TKey : notnull
    {
        if (!source.TryGetValue(syntaxTree, out var values) || values.Count == 0)
            return;

        destination[syntaxTree] = new Dictionary<TKey, TValue>(values);
    }

    private static void CopyOwnerRelativeState<TValue>(
        IDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, TValue>> source,
        IDictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, TValue>> destination,
        SyntaxTree previousTree,
        SyntaxTree currentTree,
        ExecutableOwnerDescriptor previousOwner,
        ExecutableOwnerDescriptor currentOwner,
        OwnerRelativeTextChange? ownerChange)
    {
        if (!source.TryGetValue(previousTree, out var values) || values.Count == 0)
            return;

        Dictionary<OwnerRelativeDescriptorKey, TValue>? remappedValues = null;

        foreach (var (key, value) in values)
        {
            if (key.Owner != previousOwner)
                continue;

            if (!TryRemapOwnerRelativeDescriptorKey(key, currentOwner, ownerChange, out var remappedKey))
                continue;

            remappedValues ??= destination.TryGetValue(currentTree, out var existing)
                ? existing
                : new Dictionary<OwnerRelativeDescriptorKey, TValue>();

            remappedValues[remappedKey] = value;
        }

        if (remappedValues is not null)
            destination[currentTree] = remappedValues;
    }

    private static bool TryGetOwnerChange(
        ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange> ownerChanges,
        ExecutableOwnerDescriptor owner,
        out OwnerRelativeTextChange change)
        => ownerChanges.TryGetValue(owner, out change);

    private static bool TryRemapOwnerRelativeDescriptorKey(
        OwnerRelativeDescriptorKey key,
        ExecutableOwnerDescriptor currentOwner,
        OwnerRelativeTextChange? ownerChange,
        out OwnerRelativeDescriptorKey remappedKey)
    {
        if (ownerChange is not { } change)
        {
            remappedKey = new OwnerRelativeDescriptorKey(currentOwner, key.RelativeStart, key.Length, key.Kind);
            return true;
        }

        var previousChangedSpan = change.PreviousSpan;
        var currentChangedSpan = change.CurrentSpan;
        var descriptorSpan = new Text.TextSpan(key.RelativeStart, key.Length);

        if (descriptorSpan.IntersectsWith(previousChangedSpan))
        {
            remappedKey = default;
            return false;
        }

        var delta = currentChangedSpan.Length - previousChangedSpan.Length;
        var remappedRelativeStart = key.RelativeStart >= previousChangedSpan.End
            ? key.RelativeStart + delta
            : key.RelativeStart;

        remappedKey = new OwnerRelativeDescriptorKey(currentOwner, remappedRelativeStart, key.Length, key.Kind);
        return true;
    }

    private static bool HasTransferredState(IncrementalCompilationState state)
    {
        return state.VisibleValueScopeDeclarations.Count != 0 ||
               state.VisibleValueScopeDeclarationsByOwner.Count != 0 ||
               state.NodeInterestSymbolDescriptors.Count != 0 ||
               state.NodeInterestSymbolDescriptorsByOwner.Count != 0 ||
               state.ContextualBindingRootDescriptors.Count != 0 ||
               state.ContextualBindingRootDescriptorsByOwner.Count != 0 ||
               state.InterestBindingRootDescriptors.Count != 0 ||
               state.InterestBindingRootDescriptorsByOwner.Count != 0 ||
               state.ExecutableOwnerDescriptors.Count != 0 ||
               state.FunctionExpressionRebindRootDescriptors.Count != 0 ||
               state.FunctionExpressionRebindRootDescriptorsByOwner.Count != 0 ||
               state.BinderParentAnchorDescriptors.Count != 0 ||
               state.BinderParentAnchorDescriptorsByOwner.Count != 0;
    }

    private bool TryGetTransferredVisibleValueScopeDeclarations(
        SyntaxNode scopeNode,
        out ImmutableArray<VisibleValueDeclarationDescriptor> declarations)
    {
        declarations = default;

        if (_incrementalState is null)
            return false;

        if (_incrementalState.TryGetVisibleValueScopeDeclarations(
                scopeNode.SyntaxTree,
                new VisibleValueScopeKey(scopeNode.Span, scopeNode.Kind),
                out declarations))
        {
            return true;
        }

        return TryGetTransferredOwnerRelativeDescriptor(
            scopeNode,
            _incrementalState.TryGetVisibleValueScopeDeclarations,
            out declarations);
    }

    private bool TryGetTransferredNodeInterestSymbolDescriptor(
        SyntaxNode node,
        out NodeInterestSymbolDescriptor descriptor)
    {
        descriptor = default;

        if (_incrementalState is null)
            return false;

        if (_incrementalState.TryGetNodeInterestSymbolDescriptor(
                node.SyntaxTree,
                new NodeInterestSymbolKey(node.Span, node.Kind),
                out descriptor))
        {
            return true;
        }

        return TryGetTransferredOwnerRelativeDescriptor(
            node,
            _incrementalState.TryGetNodeInterestSymbolDescriptor,
            out descriptor);
    }

    private bool TryGetTransferredContextualBindingRootDescriptor(
        SyntaxNode node,
        out ContextualBindingRootDescriptor descriptor)
    {
        descriptor = default;

        if (_incrementalState is null)
            return false;

        if (_incrementalState.TryGetContextualBindingRootDescriptor(
                node.SyntaxTree,
                new ContextualBindingRootKey(node.Span, node.Kind),
                out descriptor))
        {
            return true;
        }

        return TryGetTransferredOwnerRelativeDescriptor(
            node,
            _incrementalState.TryGetContextualBindingRootDescriptor,
            out descriptor);
    }

    private bool TryGetTransferredInterestBindingRootDescriptor(
        SyntaxNode node,
        out InterestBindingRootDescriptor descriptor)
    {
        descriptor = default;

        if (_incrementalState is null)
            return false;

        if (_incrementalState.TryGetInterestBindingRootDescriptor(
                node.SyntaxTree,
                new InterestBindingRootKey(node.Span, node.Kind),
                out descriptor))
        {
            return true;
        }

        return TryGetTransferredOwnerRelativeDescriptor(
            node,
            _incrementalState.TryGetInterestBindingRootDescriptor,
            out descriptor);
    }

    private bool TryGetTransferredExecutableOwnerDescriptor(
        SyntaxNode node,
        out ExecutableOwnerDescriptor descriptor)
    {
        descriptor = default;
        return _incrementalState is not null &&
               _incrementalState.TryGetExecutableOwnerDescriptor(
                   node.SyntaxTree,
                   new ExecutableOwnerKey(node.Span, node.Kind),
                   out descriptor);
    }

    private bool TryGetTransferredFunctionExpressionRebindRootDescriptor(
        FunctionExpressionSyntax functionExpression,
        out FunctionExpressionRebindRootDescriptor descriptor)
    {
        descriptor = default;

        if (_incrementalState is null)
            return false;

        if (_incrementalState.TryGetFunctionExpressionRebindRootDescriptor(
                functionExpression.SyntaxTree,
                new FunctionExpressionRebindRootKey(functionExpression.Span, functionExpression.Kind),
                out descriptor))
        {
            return true;
        }

        return TryGetTransferredOwnerRelativeDescriptor(
            functionExpression,
            _incrementalState.TryGetFunctionExpressionRebindRootDescriptor,
            out descriptor);
    }

    private bool TryGetTransferredBinderParentAnchorDescriptor(
        SyntaxNode node,
        out BinderParentAnchorDescriptor descriptor)
    {
        descriptor = default;

        if (_incrementalState is null)
            return false;

        if (_incrementalState.TryGetBinderParentAnchorDescriptor(
                node.SyntaxTree,
                new BinderParentAnchorKey(node.Span, node.Kind),
                out descriptor))
        {
            return true;
        }

        return TryGetTransferredOwnerRelativeDescriptor(
            node,
            _incrementalState.TryGetBinderParentAnchorDescriptor,
            out descriptor);
    }

    private bool TryGetTransferredOwnerRelativeDescriptor<TDescriptor>(
        SyntaxNode node,
        TryGetTransferredOwnerRelativeDescriptorDelegate<TDescriptor> tryGetDescriptor,
        out TDescriptor descriptor)
    {
        descriptor = default!;

        if (!TryGetMatchedExecutableOwner(node, out var match))
            return false;

        var key = new OwnerRelativeDescriptorKey(
            match.CurrentOwner,
            node.Span.Start - match.CurrentOwner.Span.Start,
            node.Span.Length,
            node.Kind);

        return tryGetDescriptor(node.SyntaxTree, key, out descriptor);
    }

    private delegate bool TryGetTransferredOwnerRelativeDescriptorDelegate<TDescriptor>(
        SyntaxTree syntaxTree,
        OwnerRelativeDescriptorKey key,
        out TDescriptor descriptor);

    internal bool HasTransferredBinderParentAnchorDescriptorForTesting(SyntaxNode node)
        => TryGetTransferredBinderParentAnchorDescriptor(node, out _);

    internal bool HasTransferredNodeInterestSymbolDescriptorForTesting(SyntaxNode node)
        => TryGetTransferredNodeInterestSymbolDescriptor(node, out _);

    internal bool HasTransferredFunctionExpressionRebindRootDescriptorForTesting(FunctionExpressionSyntax functionExpression)
        => TryGetTransferredFunctionExpressionRebindRootDescriptor(functionExpression, out _);
}
