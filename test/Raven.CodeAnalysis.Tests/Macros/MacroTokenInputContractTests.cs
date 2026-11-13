using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroTokenInputContractTests
{
    [Fact]
    public void LookaheadPreservesOrderPositionsAndRepeatedEndOfFile()
    {
        IMacroTokenStream stream = Create("one + 2");
        var third = stream.PeekToken(2);
        Assert.Equal("2", third.Text);
        Assert.Equal(third, stream.PeekToken(2));
        var first = stream.ReadToken();
        Assert.Equal("one", first.Text);
        Assert.Equal(new TextSpan(0, 3), first.Span);
        Assert.Equal("+", stream.ReadToken().Text);
        Assert.Equal(third, stream.ReadToken());
        Assert.True(stream.IsEndOfFile);
        Assert.Equal(SyntaxKind.EndOfFileToken, stream.ReadToken().Kind);
        Assert.Equal(SyntaxKind.EndOfFileToken, stream.PeekToken(3).Kind);
    }

    [Fact]
    public void KeywordsReclassifyIdentifiersOrdinallyWithoutChangingTextOrPosition()
    {
        var stream = Create("then Then", [new MacroKeyword("then", 10001)]);
        var first = stream.ReadToken();
        Assert.Equal(SyntaxKind.ThenKeyword, first.Kind);
        Assert.Equal(10001, first.RawKind);
        Assert.Equal("then", first.Text);
        Assert.Equal(new TextSpan(0, 4), first.Span);
        var second = stream.ReadToken();
        Assert.Equal("Then", second.Text);
        Assert.Equal(SyntaxKind.IdentifierToken, second.Kind);
        Assert.NotEqual(10001, second.RawKind);
    }

    [Fact]
    public void DuplicateAndNullKeywordEntriesAreRejected()
    {
        var error = Assert.Throws<ArgumentException>(() => Create("", [new("x", 10001), new("x", 10002)]));
        Assert.Equal("keywords", error.ParamName);
        Assert.Contains("Duplicate macro keyword 'x'", error.Message);
        Assert.Throws<ArgumentNullException>(() => Create("", [null!]));
    }

    [Fact]
    public void CancellationIsObservedEvenForBufferedTokens()
    {
        using var cancellation = new CancellationTokenSource();
        var stream = Create("one two", cancellationToken: cancellation.Token);
        Assert.Equal("two", stream.PeekToken(1).Text);
        cancellation.Cancel();
        Assert.Throws<OperationCanceledException>(() => stream.ReadToken());
        Assert.Throws<OperationCanceledException>(() => stream.PeekToken());
        Assert.Throws<OperationCanceledException>(() => stream.IsEndOfFile);
        Assert.Throws<ArgumentOutOfRangeException>(() => stream.PeekToken(-1));
    }

    [Theory]
    [InlineData("")]
    [InlineData(" ")]
    [InlineData(null)]
    public void KeywordRejectsMissingText(string? text)
    {
        Assert.Equal("text", Assert.Throws<ArgumentException>(() => new MacroKeyword(text!, 1)).ParamName);
    }

    [Fact]
    public void KeywordValueEqualityIncludesClassificationAndRawKind()
    {
        var keyword = new MacroKeyword("word", 10001);
        var equal = new MacroKeyword("word", 10001);
        Assert.True(keyword == equal);
        Assert.Equal(keyword.GetHashCode(), equal.GetHashCode());
        Assert.True(keyword != new MacroKeyword("word", 10002));
        Assert.True(keyword != new MacroKeyword("word", 10001, MacroKeywordClassification.ReservedWord));
    }

    [Fact]
    public void ContextPreservesInputAndHasStoredFieldEquality()
    {
        var options = new ParseOptions();
        using var cancellation = new CancellationTokenSource();
        var context = new MacroTokenStreamContext("x", new TextSpan(10, 1), options, cancellation.Token);
        var equal = new MacroTokenStreamContext("x", new TextSpan(10, 1), options, cancellation.Token);
        Assert.True(context == equal);
        Assert.Equal(context.GetHashCode(), equal.GetHashCode());
        Assert.Equal("x", context.BodyText);
        Assert.Equal(new TextSpan(10, 1), context.BodySpan);
        Assert.Same(options, context.ParseOptions);
        Assert.Equal(cancellation.Token, context.CancellationToken);
        Assert.True(context != new MacroTokenStreamContext("y", context.BodySpan, options, cancellation.Token));
    }

    [Fact]
    public void ContextAndStreamRejectNullInputs()
    {
        Assert.Throws<ArgumentNullException>(() => new MacroTokenStreamContext(null!, default, new ParseOptions(), default));
        Assert.Throws<ArgumentNullException>(() => new MacroTokenStreamContext("", default, null!, default));
        Assert.Throws<ArgumentNullException>(() => new RavenMacroTokenStream(null!, []));
        Assert.Throws<ArgumentOutOfRangeException>(() => Create("").PeekToken(-1));
    }

    private static RavenMacroTokenStream Create(string text, ImmutableArray<MacroKeyword> keywords = default,
        CancellationToken cancellationToken = default)
        => new(new MacroTokenStreamContext(text, new TextSpan(0, text.Length), new ParseOptions(), cancellationToken),
            keywords.IsDefault ? [] : keywords);
}
