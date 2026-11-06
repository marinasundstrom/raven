using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class IncrementalBindingStateTransferPolicy
{
    public static bool TryRemapOwnerRelativeDescriptorKey(
        Compilation.OwnerRelativeDescriptorKey key,
        SyntaxTree previousTree,
        SyntaxTree currentTree,
        Compilation.ExecutableOwnerDescriptor currentOwner,
        Compilation.OwnerRelativeTextChange? ownerChange,
        out Compilation.OwnerRelativeDescriptorKey remappedKey)
    {
        if (ownerChange is not { } change)
        {
            remappedKey = new Compilation.OwnerRelativeDescriptorKey(currentOwner, key.RelativeStart, key.Length, key.Kind);
            return true;
        }

        var descriptorSpan = new Text.TextSpan(key.RelativeStart, key.Length);
        if (ShouldInvalidateOwnerRelativeDescriptor(descriptorSpan, change))
        {
            remappedKey = default;
            return false;
        }

        // A retained descriptor precedes the edit and keeps its relative start.
        if (!TryValidateTransferredDescriptorIdentity(
                key,
                previousTree,
                currentTree,
                currentOwner,
                key.RelativeStart))
        {
            remappedKey = default;
            return false;
        }

        remappedKey = new Compilation.OwnerRelativeDescriptorKey(currentOwner, key.RelativeStart, key.Length, key.Kind);
        return true;
    }

    public static bool TryRemapDeclarationSensitiveDescriptorKey(
        Compilation.OwnerRelativeDescriptorKey key,
        SyntaxTree previousTree,
        SyntaxTree currentTree,
        Compilation.ExecutableOwnerDescriptor currentOwner,
        Compilation.OwnerRelativeTextChange? ownerChange,
        out Compilation.OwnerRelativeDescriptorKey remappedKey)
    {
        if (ownerChange is { Kind: Compilation.OwnerRelativeChangeKind.BodyDeclaration })
        {
            remappedKey = default;
            return false;
        }

        return TryRemapOwnerRelativeDescriptorKey(
            key,
            previousTree,
            currentTree,
            currentOwner,
            ownerChange,
            out remappedKey);
    }

    private static bool ShouldInvalidateOwnerRelativeDescriptor(
        Text.TextSpan descriptorSpan,
        Compilation.OwnerRelativeTextChange ownerChange)
    {
        return ownerChange.Kind switch
        {
            Compilation.OwnerRelativeChangeKind.SignatureOrDeclaration => true,
            Compilation.OwnerRelativeChangeKind.BodyDeclaration =>
                ShouldInvalidateFromChangedSpan(descriptorSpan, ownerChange.PreviousSpan),
            Compilation.OwnerRelativeChangeKind.BodyExpression =>
                ShouldInvalidateFromChangedSpan(descriptorSpan, ownerChange.PreviousSpan),
            _ => true
        };
    }

    private static bool ShouldInvalidateFromChangedSpan(
        Text.TextSpan descriptorSpan,
        Text.TextSpan previousChangedSpan)
    {
        if (descriptorSpan.IntersectsWith(previousChangedSpan))
            return true;

        // Binding inside an executable owner is order-sensitive. A changed
        // signature, local declaration, or earlier expression can cascade into
        // diagnostics and symbols for unchanged later body nodes. Reuse only
        // descriptors that are fully before the edit.
        return descriptorSpan.Start >= previousChangedSpan.Start;
    }

    private static bool TryValidateTransferredDescriptorIdentity(
        Compilation.OwnerRelativeDescriptorKey key,
        SyntaxTree previousTree,
        SyntaxTree currentTree,
        Compilation.ExecutableOwnerDescriptor currentOwner,
        int currentRelativeStart)
    {
        if (!TryFindNodeByOwnerRelativeDescriptor(
                previousTree,
                key.Owner,
                key.RelativeStart,
                key.Length,
                key.Kind,
                out var previousNode))
        {
            return false;
        }

        if (!TryFindNodeByOwnerRelativeDescriptor(
                currentTree,
                currentOwner,
                currentRelativeStart,
                key.Length,
                key.Kind,
                out var currentNode))
        {
            return false;
        }

        return AreIncrementallyEquivalentDescriptorNode(previousNode, currentNode);
    }

    private static bool AreIncrementallyEquivalentDescriptorNode(SyntaxNode previousNode, SyntaxNode currentNode)
    {
        if (previousNode.Kind != currentNode.Kind)
            return false;

        if (previousNode.Span.Length != currentNode.Span.Length)
            return false;

        if (ReferenceEquals(previousNode.Green, currentNode.Green))
            return true;

        return string.Equals(previousNode.ToFullString(), currentNode.ToFullString(), StringComparison.Ordinal);
    }

    private static bool TryFindNodeByOwnerRelativeDescriptor(
        SyntaxTree syntaxTree,
        Compilation.ExecutableOwnerDescriptor ownerDescriptor,
        int relativeStart,
        int length,
        SyntaxKind kind,
        out SyntaxNode node)
    {
        node = null!;

        var absoluteStart = ownerDescriptor.Span.Start + relativeStart;
        if (absoluteStart < ownerDescriptor.Span.Start)
            return false;

        var span = new Text.TextSpan(absoluteStart, length);
        if (!ownerDescriptor.Span.Contains(span))
            return false;

        var owner = syntaxTree.GetRoot()
            .DescendantNodesAndSelf()
            .FirstOrDefault(candidate =>
                candidate.Kind == ownerDescriptor.Kind &&
                candidate.Span == ownerDescriptor.Span);
        if (owner is null)
            return false;

        node = owner
            .DescendantNodesAndSelf()
            .FirstOrDefault(candidate =>
                candidate.Kind == kind &&
                candidate.Span == span)!;

        return node is not null;
    }
}
