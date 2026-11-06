using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Compilation;

namespace Raven.CodeAnalysis.Tests.Workspaces;

[Trait("Target", "Shared")]
public sealed class DescriptorTransferTests
{
    [Theory]
    [InlineData((int)OwnerRelativeChangeKind.BodyExpression, true, true)]
    [InlineData((int)OwnerRelativeChangeKind.BodyDeclaration, true, false)]
    [InlineData((int)OwnerRelativeChangeKind.SignatureOrDeclaration, false, false)]
    [InlineData((int)OwnerRelativeChangeKind.Unknown, false, false)]
    public void TransferDependsOnChangeKindAndDescriptorSensitivity(int kind, bool ordinary, bool sensitive)
    {
        const string source = "class Box { func Run(value: int) -> int { return value + 1 } }";
        var previous = SyntaxTree.ParseText(source);
        var current = SyntaxTree.ParseText("// new header\n" + source.Replace("+ 1", "+ 22"));
        var previousMethod = previous.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var currentMethod = current.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var type = previousMethod.DescendantNodes().OfType<PredefinedTypeSyntax>().First();
        var previousOwner = new ExecutableOwnerDescriptor(previousMethod.Span, previousMethod.Kind);
        var currentOwner = new ExecutableOwnerDescriptor(currentMethod.Span, currentMethod.Kind);
        var key = new OwnerRelativeDescriptorKey(previousOwner, type.Span.Start - previousMethod.Span.Start, type.Span.Length, type.Kind);
        var offset = source.IndexOf('1') - previousMethod.Span.Start;
        var change = new OwnerRelativeTextChange(new TextSpan(offset, 1), new TextSpan(offset, 2), (OwnerRelativeChangeKind)kind);
        Assert.Equal(ordinary, IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey(key, previous, current, currentOwner, change, out var mapped));
        if (ordinary) Assert.Equal(key with { Owner = currentOwner }, mapped);
        Assert.Equal(sensitive, IncrementalBindingStateTransferPolicy.TryRemapDeclarationSensitiveDescriptorKey(key, previous, current, currentOwner, change, out _));
        var later = key with { RelativeStart = offset };
        Assert.False(IncrementalBindingStateTransferPolicy.TryRemapOwnerRelativeDescriptorKey(later, previous, current, currentOwner, change, out _));
    }
}
