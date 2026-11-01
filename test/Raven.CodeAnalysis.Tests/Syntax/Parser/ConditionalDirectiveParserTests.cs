using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class ConditionalDirectiveParserTests
{
    private static readonly IReadOnlySet<string> Symbols = new HashSet<string>(StringComparer.Ordinal)
        { "DEBUG", "notable", "android", "origin", "λ", "TRUE" };

    [Theory]
    [InlineData("true", true)]
    [InlineData("false", false)]
    [InlineData("DEBUG", true)]
    [InlineData("debug", false)]
    [InlineData("MISSING", false)]
    [InlineData("TRUE", true)]
    [InlineData("false or true and false", false)]
    [InlineData("(false or true) and DEBUG", true)]
    [InlineData("not not DEBUG", true)]
    [InlineData("!DEBUG || true && false", false)]
    [InlineData("notable and android and origin", true)]
    [InlineData("λ && !MISSING", true)]
    [InlineData("\t DEBUG \u2003 or false \n", true)]
    public void EvaluatesCompleteCondition(string text, bool expected)
    {
        Assert.True(ConditionalDirectiveParser.TryEvaluate(text, Symbols, out var actual, out var offset, out var error));
        Assert.Equal(expected, actual);
        Assert.Equal(text.Length, offset);
        Assert.Null(error);
    }

    [Theory]
    [InlineData("", 0, "expected an expression")]
    [InlineData(" ", 1, "expected an expression")]
    [InlineData("1", 0, "expected a conditional symbol")]
    [InlineData("DEBUG junk", 6, "unexpected token")]
    [InlineData("true or", 7, "expected an expression")]
    [InlineData("false and (", 11, "expected an expression")]
    [InlineData("true || (", 9, "expected an expression")]
    [InlineData("(DEBUG", 6, "expected ')'")]
    [InlineData("true || false junk", 14, "unexpected token")]
    public void ReportsInvalidConditionsEvenWhenBooleanResultIsKnown(string text, int offset, string message)
    {
        Assert.False(ConditionalDirectiveParser.TryEvaluate(text, Symbols, out _, out var actualOffset, out var error));
        Assert.Equal(offset, actualOffset);
        Assert.Equal(message, error);
    }

    [Fact]
    public void DirectiveInfoHasReferenceStorageAndValueEquality()
    {
        var first = new ConditionalDirectiveInfo(ConditionalDirectiveKind.If, SyntaxKind.IfDirectiveTrivia,
            "DEBUG", true, true, 1, 2, 4, 5);
        var equal = new ConditionalDirectiveInfo(ConditionalDirectiveKind.If, SyntaxKind.IfDirectiveTrivia,
            "DEBUG", true, true, 1, 2, 4, 5);
        var different = new ConditionalDirectiveInfo(ConditionalDirectiveKind.If, SyntaxKind.IfDirectiveTrivia,
            "DEBUG", false, true, 1, 2, 4, 5);
        Assert.False(typeof(ConditionalDirectiveInfo).IsValueType);
        Assert.True(typeof(ConditionalDirectiveInfo).IsSealed);
        Assert.NotSame(first, equal);
        Assert.Equal(first, equal);
        Assert.Equal(first.GetHashCode(), equal.GetHashCode());
        Assert.True(first == equal);
        Assert.True(first != different);
        Assert.False(first.Equals(null));
        Assert.Equal("DEBUG", first.ConditionText);
        Assert.Equal(4, first.ConditionOffset);
        Assert.Equal(5, first.ConditionLength);
    }

    [Fact]
    public void DirectiveInfoSupportsNonMutatingCopyAndDeconstruction()
    {
        var original = new ConditionalDirectiveInfo(ConditionalDirectiveKind.Elif, SyntaxKind.ElifDirectiveTrivia,
            "TRACE", true, false, 1, 4, 6, 5);
        var changed = original with { IsBranchActive = false };
        Assert.NotSame(original, changed);
        Assert.True(original.IsBranchActive);
        Assert.False(changed.IsBranchActive);
        var (kind, syntaxKind, condition, active, taken, keywordOffset, keywordLength, conditionOffset, conditionLength) = original;
        Assert.Equal(ConditionalDirectiveKind.Elif, kind);
        Assert.Equal(SyntaxKind.ElifDirectiveTrivia, syntaxKind);
        Assert.Equal("TRACE", condition);
        Assert.True(active);
        Assert.False(taken);
        Assert.Equal((1, 4, 6, 5), (keywordOffset, keywordLength, conditionOffset, conditionLength));
    }

}
