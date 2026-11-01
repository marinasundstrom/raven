namespace Raven.CodeAnalysis.Syntax.Tests;

public class SyntaxFactsTests
{
    [Theory]
    [InlineData("unit", SyntaxKind.UnitKeyword)]
    [InlineData("then", SyntaxKind.ThenKeyword)]
    [InlineData("and", SyntaxKind.AndToken)]
    [InlineData("goto", SyntaxKind.GotoKeyword)]
    [InlineData("break", SyntaxKind.BreakKeyword)]
    [InlineData("continue", SyntaxKind.ContinueKeyword)]
    public void TryParseKeyword_ReturnsExpectedKind(string text, SyntaxKind expected)
    {
        SyntaxFacts.TryParseKeyword(text, out var kind).ShouldBeTrue();
        kind.ShouldBe(expected);
    }

    [Theory]
    [InlineData("bool", SyntaxKind.BoolKeyword)]
    [InlineData("char", SyntaxKind.CharKeyword)]
    [InlineData("sbyte", SyntaxKind.SByteKeyword)]
    [InlineData("byte", SyntaxKind.ByteKeyword)]
    [InlineData("short", SyntaxKind.ShortKeyword)]
    [InlineData("ushort", SyntaxKind.UShortKeyword)]
    [InlineData("int", SyntaxKind.IntKeyword)]
    [InlineData("uint", SyntaxKind.UIntKeyword)]
    [InlineData("long", SyntaxKind.LongKeyword)]
    [InlineData("ulong", SyntaxKind.ULongKeyword)]
    [InlineData("nint", SyntaxKind.NIntKeyword)]
    [InlineData("nuint", SyntaxKind.NUIntKeyword)]
    [InlineData("float", SyntaxKind.FloatKeyword)]
    [InlineData("double", SyntaxKind.DoubleKeyword)]
    [InlineData("decimal", SyntaxKind.DecimalKeyword)]
    [InlineData("string", SyntaxKind.StringKeyword)]
    [InlineData("object", SyntaxKind.ObjectKeyword)]
    public void TryParseKeyword_RecognizesPrimitiveKeywords(string text, SyntaxKind expected)
    {
        SyntaxFacts.TryParseKeyword(text, out var kind).ShouldBeTrue();
        kind.ShouldBe(expected);
    }

    [Fact]
    public void TryParseKeyword_ReturnsFalse_ForNonKeyword()
    {
        SyntaxFacts.TryParseKeyword("+", out _).ShouldBeFalse();
        SyntaxFacts.TryParseKeyword("trait", out _).ShouldBeFalse();
    }

    [Fact]
    public void IsKeywordKind_DistinguishesKinds()
    {
        SyntaxFacts.IsKeywordKind(SyntaxKind.ImportKeyword).ShouldBeTrue();
        SyntaxFacts.IsKeywordKind(SyntaxKind.AndToken).ShouldBeTrue();
        SyntaxFacts.IsKeywordKind(SyntaxKind.PlusToken).ShouldBeFalse();
    }

    [Fact]
    public void IsReservedWordKind_OnlyForReserved()
    {
        SyntaxFacts.IsReservedWordKind(SyntaxKind.UnitKeyword).ShouldBeFalse();
        SyntaxFacts.IsReservedWordKind(SyntaxKind.ThenKeyword).ShouldBeFalse();
        SyntaxFacts.IsReservedWordKind(SyntaxKind.AndToken).ShouldBeTrue();
        SyntaxFacts.IsReservedWordKind(SyntaxKind.GotoKeyword).ShouldBeTrue();
        SyntaxFacts.IsReservedWordKind(SyntaxKind.BreakKeyword).ShouldBeTrue();
        SyntaxFacts.IsReservedWordKind(SyntaxKind.ContinueKeyword).ShouldBeTrue();
    }

    [Fact]
    public void TryParseReservedWord_ReturnsExpected()
    {
        SyntaxFacts.TryParseReservedWord("and", out var kind).ShouldBeTrue();
        kind.ShouldBe(SyntaxKind.AndToken);
        SyntaxFacts.TryParseReservedWord("unit", out _).ShouldBeFalse();
    }
    [Theory]
    [InlineData("", "")]
    [InlineData("hello", "hello")]
    [InlineData("λ2", "λ2")]
    [InlineData("$value", "$value")]
    [InlineData("_", "`_`")]
    [InlineData("two words", "`two words`")]
    [InlineData("1value", "`1value`")]
    [InlineData("class", "@class")]
    [InlineData("unit", "@unit")]
    [InlineData("Class", "Class")]
    public void EscapeIdentifierPreservesSpelling(string input, string expected)
    {
        Assert.Equal(expected, SyntaxFacts.EscapeIdentifier(input));
    }

    [Theory]
    [InlineData(SyntaxKind.GreaterThanToken, 1)]
    [InlineData(SyntaxKind.GreaterThanOrEqualsToken, 1)]
    [InlineData(SyntaxKind.GreaterThanGreaterThanToken, 2)]
    [InlineData(SyntaxKind.GreaterThanGreaterThanEqualsToken, 2)]
    [InlineData(SyntaxKind.GreaterThanGreaterThanGreaterThanToken, 3)]
    [InlineData(SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken, 3)]
    [InlineData(SyntaxKind.LessThanToken, 0)]
    public void LeadingGreaterThanCountPreservesSplittableTokens(SyntaxKind kind, int expected)
    {
        Assert.Equal(expected, SyntaxFacts.LeadingGreaterThanCount(kind));
    }

    [Fact]
    public void TokenTextIsAvailableThroughStaticAndExtensionCalls()
    {
        Assert.Equal("+", SyntaxFacts.GetSyntaxTokenText(SyntaxKind.PlusToken));
        Assert.Equal("+", SyntaxKind.PlusToken.GetSyntaxTokenText());
        Assert.Null(SyntaxKind.IdentifierToken.GetSyntaxTokenText());
    }

}
