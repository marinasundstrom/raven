#if RAVEN_PORT
extern alias Port;
using AttributeApi = Port::Raven.CodeAnalysis.Syntax.AttributeSyntaxExtensions;
#else
using AttributeApi = Raven.CodeAnalysis.Syntax.AttributeSyntaxExtensions;
#endif

using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class DeclarationClauseParserContractTests
{
    [Theory]
    [InlineData(": int", false)]
    [InlineData(": System.Collections.Generic.List<string?>", false)]
    [InlineData(":", true)]
    public void TypeAnnotationPreservesSourceAndDiagnosesMissingType(string source, bool missing)
    {
        var parser = new TypeAnnotationClauseSyntaxParser(Context(source));
        var result = parser.ParseTypeAnnotation();
        Assert.NotNull(result);
        Assert.Equal(source, result!.CreateRed().ToFullString());
        Assert.Equal(missing, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("-> int", false)]
    [InlineData("-> [A] string", false)]
    [InlineData("->", true)]
    public void ReturnAnnotationPreservesAttributesAndMissingType(string source, bool missing)
    {
        var parser = new TypeAnnotationClauseSyntaxParser(Context(source));
        var result = parser.ParseReturnTypeAnnotation();
        Assert.NotNull(result);
        Assert.Equal(source, result!.CreateRed().ToFullString());
        Assert.Equal(missing, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void AbsentAnnotationsDoNotConsumeInput()
    {
        var parser = new TypeAnnotationClauseSyntaxParser(Context("name"));
        Assert.Null(parser.ParseTypeAnnotation());
        Assert.Null(parser.ParseReturnTypeAnnotation());
        Assert.Equal("name", parser.PeekToken().Text);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("[A]", false)]
    [InlineData("#[A(1, name: 2), B]", false)]
    [InlineData("[return: A][B]", false)]
    [InlineData("[A", true)]
    public void AttributeListsPreserveSourceAndRecoverMissingClose(string source, bool malformed)
    {
        var parser = new SyntaxParser(Context(source));
        var result = AttributeDeclarationParser.ParseAttributeLists(parser);
        Assert.Equal(source, string.Concat(Enumerable.Range(0, result.SlotCount)
            .Select(index => result.GetSlot(index)!.CreateRed().ToFullString())));
        Assert.Equal(malformed, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void MacroPrefixBelongsToListAndMarksOnlyFirstAttribute()
    {
        const string source = " #[First, Second]";
        var parser = new SyntaxParser(Context(source));
        var green = AttributeDeclarationParser.ParseAttributeList(parser);
        var list = (AttributeListSyntax)green.CreateRed();

        Assert.Equal(source, list.ToFullString());
        Assert.Equal(source.IndexOf('#'), list.HashToken.Span.Start);
        Assert.Equal(source.IndexOf('['), list.OpenBracketToken.Span.Start);
        Assert.Equal(source.IndexOf("First", StringComparison.Ordinal), list.Attributes[0].Span.Start);
        Assert.Same(list, list.HashToken.Parent);
        Assert.True(AttributeApi.IsMacroAttribute(list.Attributes[0]));
        Assert.False(AttributeApi.IsMacroAttribute(list.Attributes[1]));
        Assert.True(AttributeApi.TryGetMacroName(list.Attributes[0], out var name));
        Assert.Equal("First", name);
        Assert.Equal(source, list.WithAttributes(list.Attributes).ToFullString());
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void AbsentAttributesDoNotConsumeInput()
    {
        var parser = new SyntaxParser(Context("name"));
        Assert.Equal(0, AttributeDeclarationParser.ParseAttributeLists(parser).SlotCount);
        Assert.Equal("name", parser.PeekToken().Text);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("= 1 + 2", false)]
    [InlineData("=", true)]
    public void InitializerPreservesSourceAndRestoresNewlineMode(string source, bool missing)
    {
        var context = Context(source);
        context.SetTreatNewlinesAsTokens(true);
        var parser = new EqualsValueClauseSyntaxParser(context);
        var result = parser.Parse();
        Assert.NotNull(result);
        Assert.Equal(source, result!.CreateRed().ToFullString());
        Assert.Equal(missing, parser.Diagnostics.Count > 0);
        Assert.True(context.TreatNewlinesAsTokens);
    }

    [Fact]
    public void InitializerStopsBeforeFollowingDeclaration()
    {
        var parser = new EqualsValueClauseSyntaxParser(Context("= 1\nlet other = 2"));
        Assert.Equal("= 1\n", parser.Parse()!.CreateRed().ToFullString());
        Assert.Equal(SyntaxKind.LetKeyword, parser.PeekToken().Kind);
        Assert.Empty(parser.Diagnostics);
    }

    private static BaseParseContext Context(string source) => new(new Lexer(new StringReader(source)));
}
