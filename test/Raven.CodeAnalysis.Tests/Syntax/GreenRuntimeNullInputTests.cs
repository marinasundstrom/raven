using System.Reflection;

using GreenNode = Raven.CodeAnalysis.Syntax.GreenNode;
using GreenSyntax = Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class GreenRuntimeNullInputTests
{
    [Fact]
    public void GreenList_PreservesEmptySlotsAndSkipsThemDuringTraversal()
    {
        var token = new GreenSyntax.SyntaxToken(SyntaxKind.IdentifierToken, "name");
        var list = new GreenSyntax.SyntaxList([null, token, null]);
        Assert.Equal(3, list.SlotCount);
        Assert.Null(list[0]);
        Assert.Same(token, list[1]);
        Assert.Null(list[2]);
        Assert.Equal(4, list.Width);
        Assert.Equal(4, list.FullWidth);
        Assert.Same(token, Assert.Single(list.GetChildren()));
        var updated = Assert.IsType<GreenSyntax.SyntaxList>(list.With([token, null]));
        Assert.Equal(2, updated.SlotCount);
        Assert.Null(updated[1]);
        Assert.Same(token, Assert.Single(updated.GetChildren()));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void GreenList_AnnotatesNullableSlotsWithoutAllowingNullArray()
    {
        var context = new NullabilityInfoContext();
        var items = context.Create(typeof(GreenSyntax.SyntaxList).GetConstructors().Single().GetParameters()[0]);
        Assert.Equal(NullabilityState.NotNull, items.ReadState);
        Assert.Equal(NullabilityState.Nullable, items.ElementType!.ReadState);
        var indexer = context.Create(typeof(GreenSyntax.SyntaxList).GetProperty("Item")!);
        Assert.Equal(NullabilityState.Nullable, indexer.ReadState);
    }

    [Fact]
    public void DefaultToken_HasNoGreenNodeAndBehavesAsEmpty()
    {
        var token = default(SyntaxToken);
        Assert.Null(token.Green);
        Assert.Equal(SyntaxKind.None, token.Kind);
        Assert.True(token.IsMissing);
        Assert.Equal(string.Empty, token.Text);
        Assert.Equal(string.Empty, token.ToFullString());
        Assert.Equal(0, token.FullWidth);
        Assert.False(token.HasLeadingTrivia);
        Assert.False(token.HasTrailingTrivia);
        Assert.Equal(token, token.WithRawKind(42));
        Assert.Equal(token, token.WithLeadingTrivia(SyntaxFactory.Space));
        Assert.Equal(token, token.WithTrailingTrivia(SyntaxFactory.Space));
    }

    [Fact]
    public void NullTriviaArray_CreatesEmptyListAndSlotsRemainChecked()
    {
        var empty = new GreenSyntax.SyntaxTriviaList(null!);
        Assert.Equal(0, empty.Count);
        Assert.Equal(0, empty.Width);
        Assert.Equal(0, empty.FullWidth);
        Assert.Throws<IndexOutOfRangeException>(() => empty.GetSlot(0));
        var trivia = GreenSyntax.SyntaxFactory.Space;
        var list = new GreenSyntax.SyntaxTriviaList([trivia]);
        Assert.Same(trivia, list.GetSlot(0));
        Assert.Equal(trivia.FullWidth, list.Width);
        Assert.Equal(list.Width, list.FullWidth);
        Assert.Throws<IndexOutOfRangeException>(() => list.GetSlot(-1));
        Assert.Throws<IndexOutOfRangeException>(() => list.GetSlot(1));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void TokenAndTriviaListAnnotations_MatchDefaultAndSlotContracts()
    {
        var context = new NullabilityInfoContext();
        var green = typeof(SyntaxToken).GetField("Green", BindingFlags.Instance | BindingFlags.NonPublic)!;
        Assert.Equal(NullabilityState.Nullable, context.Create(green).ReadState);
        var input = typeof(GreenSyntax.SyntaxTriviaList).GetConstructors().Single().GetParameters()[0];
        Assert.Equal(NullabilityState.Nullable, context.Create(input).ReadState);
        var result = typeof(GreenSyntax.SyntaxTriviaList).GetMethod("GetSlot")!.ReturnParameter;
        Assert.Equal(NullabilityState.NotNull, context.Create(result).ReadState);
    }

    [Fact]
    public void NullSlots_CreateAnEmptyNode()
    {
        var node = new EmptyNode(null);
        Assert.Equal(0, node.SlotCount);
        Assert.Equal(0, node.Width);
        Assert.Equal(0, node.FullWidth);
        Assert.Null(node.GetSlot(0));
    }

    [Fact]
    public void NullTriviaEnumerables_ClearTrivia()
    {
        var token = new GreenSyntax.SyntaxToken(SyntaxKind.IdentifierToken, "name")
            .WithLeadingTrivia(new[] { GreenSyntax.SyntaxFactory.Space })
            .WithTrailingTrivia(new[] { GreenSyntax.SyntaxFactory.Space });
        var withoutLeading = token.WithLeadingTrivia((IEnumerable<GreenSyntax.SyntaxTrivia>?)null);
        var withoutTrailing = token.WithTrailingTrivia((IEnumerable<GreenSyntax.SyntaxTrivia>?)null);
        Assert.Equal(0, withoutLeading.LeadingTrivia.Width);
        Assert.Equal(1, withoutLeading.TrailingTrivia.Width);
        Assert.Equal(0, withoutTrailing.TrailingTrivia.Width);
        Assert.Equal(1, withoutTrailing.LeadingTrivia.Width);
        Assert.Equal(6, token.FullWidth);
    }

    [Fact]
    public void NullListItems_StillThrowArgumentNullException()
    {
        Assert.Equal("items", Assert.Throws<ArgumentNullException>(() => new GreenSyntax.SyntaxList(null!)).ParamName);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void AcceptedNullInputs_AreAnnotatedNullable()
    {
        var context = new NullabilityInfoContext();
        var slots = typeof(GreenSyntax.SyntaxNode).GetConstructors().Single().GetParameters()[1];
        Assert.Equal(NullabilityState.Nullable, context.Create(slots).ReadState);
        foreach (var parameter in typeof(GreenSyntax.SyntaxToken).GetConstructors()
                     .SelectMany(constructor => constructor.GetParameters())
                     .Where(parameter => parameter.Name is "leadingTrivia" or "trailingTrivia"))
        {
            Assert.Equal(NullabilityState.Nullable, context.Create(parameter).ReadState);
        }
        foreach (var name in new[] { "WithLeadingTrivia", "WithTrailingTrivia" })
        {
            var method = typeof(GreenSyntax.SyntaxToken).GetMethod(name, [typeof(IEnumerable<GreenSyntax.SyntaxTrivia>)])!;
            Assert.Equal(NullabilityState.Nullable, context.Create(method.GetParameters()[0]).ReadState);
        }
    }

    private sealed class EmptyNode(GreenNode?[]? slots) : GreenSyntax.SyntaxNode(SyntaxKind.List, slots)
    {
        internal override void Accept(GreenSyntax.SyntaxVisitor visitor) => throw new NotSupportedException();
        internal override TResult Accept<TResult>(GreenSyntax.SyntaxVisitor<TResult> visitor) => throw new NotSupportedException();
    }
}
