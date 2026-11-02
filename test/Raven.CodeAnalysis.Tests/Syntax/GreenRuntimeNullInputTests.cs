using System.Reflection;

using GreenNode = Raven.CodeAnalysis.Syntax.GreenNode;
using GreenSyntax = Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class GreenRuntimeNullInputTests
{
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
