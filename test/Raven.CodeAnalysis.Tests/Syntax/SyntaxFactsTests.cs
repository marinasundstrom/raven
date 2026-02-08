namespace Raven.CodeAnalysis.Syntax.Tests;

public class SyntaxFactsTests
{
    [Theory]
    [InlineData("unit", SyntaxKind.UnitKeyword)]
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
}
