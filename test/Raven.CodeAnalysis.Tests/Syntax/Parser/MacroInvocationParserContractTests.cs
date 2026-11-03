using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using GreenList = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxList;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class MacroInvocationParserContractTests
{
    [Theory]
    [InlineData("foo!(1)", false, true)]
    [InlineData("foo! { body }", false, true)]
    [InlineData("foo! value { body }", false, false)]
    [InlineData("foo! value { body }", true, true)]
    [InlineData("foo !(1)", true, false)]
    [InlineData("foo /* gap */!(1)", true, false)]
    [InlineData("if !foo()", true, false)]
    [InlineData("!foo()", true, false)]
    [InlineData("if! value { body }", true, true)]
    [InlineData("foo!\n{ body }", true, false)]
    [InlineData("foo! + 1", true, false)]
    public void InvocationLookaheadRespectsAdjacencyAndRestoresContext(string source, bool allowHeader, bool expected)
    {
        var parser = Parser(source);
        var first = parser.PeekToken();
        var position = parser.Position;
        Assert.Equal(expected, parser.IsBangInvocationStart(allowHeader));
        Assert.Equal(position, parser.Position);
        Assert.Equal(first.Kind, parser.PeekToken().Kind);
        Assert.Equal(first.Text, parser.PeekToken().Text);
        Assert.Equal(first.FullWidth, parser.PeekToken().FullWidth);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("foo!(1, name: 2)")]
    [InlineData("foo! { nested { body } }")]
    [InlineData("foo! value { body }")]
    [InlineData("if! value { body }")]
    [InlineData("Tools.foo<int>!(1)")]
    public void ExpressionPreservesCarrierAndSource(string source)
    {
        var parser = Parser(source);
        Assert.True(parser.IsBangInvocationStart(allowExpressionHeader: true));
        var result = (FreestandingMacroExpressionSyntax)parser.ParseExpression().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(SyntaxKind.ExclamationToken, result.ExclamationToken.Kind);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("derive! Widget { body }")]
    [InlineData("derive! Widget<T>(value: T) -> T where T: class { body }")]
    [InlineData("derive! Widget;")]
    public void DeclarationHeaderAndTerminatorPreserveSource(string source)
    {
        var parser = Parser(source);
        Assert.True(parser.IsDeclarationInvocationStart());
        var result = parser.ParseDeclaration(GreenList.Empty, GreenList.Empty).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void MemberInvocationConsumesItsTerminator()
    {
        const string source = "foo!(1);";
        var parser = Parser(source);
        var result = parser.ParseMember(GreenList.Empty, GreenList.Empty).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void UnterminatedTokenTreeReportsDiagnosticAndPreservesInput()
    {
        const string source = "foo! { body";
        var parser = Parser(source);
        var result = parser.ParseExpression().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Contains(parser.Diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CharacterExpected);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    private static MacroInvocationSyntaxParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
