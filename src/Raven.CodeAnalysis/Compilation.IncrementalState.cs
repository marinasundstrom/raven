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
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<ExecutableOwnerDescriptor, ImmutableArray<SemanticDiagnosticDescriptor>>> SemanticDiagnosticsByOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>> SemanticDiagnosticsByRelativeOwner { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ImmutableHashSet<ExecutableOwnerDescriptor>> ChangedExecutableOwnerDescriptors { get; } = new();
        public ConcurrentDictionary<SyntaxTree, ConcurrentDictionary<ExecutableOwnerDescriptor, MatchedExecutableOwner>> MatchedExecutableOwners { get; } = new();
        public ConcurrentDictionary<SyntaxTree, byte> SemanticDiagnosticTransferBlockedSyntaxTrees { get; } = new();
    }

    internal readonly record struct IncrementalMatchedSyntaxTree(
        SyntaxTree CurrentTree,
        SyntaxTree PreviousTree,
        ImmutableArray<MatchedExecutableOwner> Matches,
        ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange> OwnerChanges,
        bool BlocksSemanticDiagnosticTransfer);

    internal readonly record struct IncrementalChangedSyntaxTree(
        SyntaxTree CurrentTree,
        SyntaxTree PreviousTree,
        ImmutableArray<ExecutableOwnerDescriptor> ChangedOwners,
        ImmutableArray<MatchedExecutableOwner> MatchedOwners,
        ImmutableDictionary<ExecutableOwnerDescriptor, OwnerRelativeTextChange> OwnerChanges,
        bool BlocksSemanticDiagnosticTransfer)
    {
        public IncrementalMatchedSyntaxTree ToMatchedSyntaxTree()
            => new(CurrentTree, PreviousTree, MatchedOwners, OwnerChanges, BlocksSemanticDiagnosticTransfer);
    }

    internal readonly record struct IncrementalCompilationPlan(
        ImmutableArray<SyntaxTree> ReusedSyntaxTrees,
        ImmutableArray<IncrementalChangedSyntaxTree> ChangedSyntaxTrees)
    {
        public ImmutableArray<IncrementalMatchedSyntaxTree> MatchedSyntaxTrees
            => ChangedSyntaxTrees
                .Where(static tree => !tree.MatchedOwners.IsDefaultOrEmpty)
                .Select(static tree => tree.ToMatchedSyntaxTree())
                .ToImmutableArray();
    }

    internal enum OwnerRelativeChangeKind
    {
        Unknown,
        BodyExpression,
        BodyDeclaration,
        SignatureOrDeclaration
    }

    internal readonly record struct OwnerRelativeTextChange(
        Text.TextSpan PreviousSpan,
        Text.TextSpan CurrentSpan,
        OwnerRelativeChangeKind Kind);

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
        public Dictionary<SyntaxTree, Dictionary<ExecutableOwnerDescriptor, ImmutableArray<SemanticDiagnosticDescriptor>>> SemanticDiagnosticsByOwner { get; } = new();
        public Dictionary<SyntaxTree, Dictionary<OwnerRelativeDescriptorKey, ImmutableArray<SemanticDiagnosticDescriptor>>> SemanticDiagnosticsByRelativeOwner { get; } = new();

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

        public bool TryGetSemanticDiagnostics(
            SyntaxTree syntaxTree,
            ExecutableOwnerDescriptor owner,
            out ImmutableArray<SemanticDiagnosticDescriptor> descriptors)
            => TryGetExact(SemanticDiagnosticsByOwner, syntaxTree, owner, out descriptors);

        public bool TryGetSemanticDiagnostics(
            SyntaxTree syntaxTree,
            OwnerRelativeDescriptorKey key,
            out ImmutableArray<SemanticDiagnosticDescriptor> descriptors)
            => TryGetExact(SemanticDiagnosticsByRelativeOwner, syntaxTree, key, out descriptors);

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

    internal bool HasTransferredContextualBindingRootDescriptorForTesting(SyntaxNode node)
        => TryGetTransferredContextualBindingRootDescriptor(node, out _);

    internal bool HasTransferredInterestBindingRootDescriptorForTesting(SyntaxNode node)
        => TryGetTransferredInterestBindingRootDescriptor(node, out _);

    internal bool HasTransferredFunctionExpressionRebindRootDescriptorForTesting(FunctionExpressionSyntax functionExpression)
        => TryGetTransferredFunctionExpressionRebindRootDescriptor(functionExpression, out _);

    internal bool HasTransferredVisibleValueScopeDeclarationsForTesting(SyntaxNode scopeNode)
        => TryGetTransferredVisibleValueScopeDeclarations(scopeNode, out _);
}
