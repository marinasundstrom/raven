using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using GreenFactory = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;
using GreenList = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxList;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class MacroDeclarationParserContractTests
{
    [Theory]
    [InlineData("macro Named() {}", true, true)]
    [InlineData("macro () {}", true, true)]
    [InlineData("public static macro Named() {}", false, true)]
    [InlineData("extern macro Native() -> int;", false, true)]
    [InlineData("async extern macro Native() -> int;", false, true)]
    [InlineData("macro + 1", false, false)]
    [InlineData("Macro Named() {}", false, false)]
    [InlineData("let macro = 1", false, false)]
    public void LookaheadPreservesInputAndRecognizesDeclarationContext(string source, bool immediate, bool afterModifiers)
    {
        var parser = Parser(source);
        var first = parser.PeekToken();
        Assert.True(parser.IsInMacro);
        Assert.Equal(immediate, parser.IsDeclarationStart());
        Assert.Equal(afterModifiers, parser.IsDeclarationStartAfterModifiers());
        Assert.Same(first, parser.PeekToken());
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("macro Named() {}", false, false)]
    [InlineData("macro Named(value: int) -> int => value", true, false)]
    [InlineData("macro Named<T>(value: T) -> T where T: class { return value }", false, false)]
    [InlineData("macro Named(on target: ClassDeclarationSyntax) { return target }", false, false)]
    [InlineData("macro () {}", false, true)]
    [InlineData("macro Named() keywords by handlers {}", false, false)]
    public void DeclarationPreservesSourceAndBodyShape(string source, bool expressionBody, bool missingName)
    {
        var parser = Parser(source);
        var result = (MacroDeclarationSyntax)parser.Parse(GreenList.Empty, GreenList.Empty).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(expressionBody, result.ExpressionBody is not null);
        Assert.Equal(!expressionBody, result.Body is not null);
        Assert.Equal(missingName, result.Identifier.IsMissing);
        Assert.Equal(missingName, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void ExternDeclarationDoesNotSynthesizeBody()
    {
        var parser = Parser("macro Native() -> int;");
        var modifiers = GreenFactory.List([GreenFactory.Token(SyntaxKind.ExternKeyword)]);
        var result = (MacroDeclarationSyntax)parser.Parse(GreenList.Empty, modifiers).CreateRed();
        Assert.Null(result.Body);
        Assert.Null(result.ExpressionBody);
        Assert.Equal(SyntaxKind.SemicolonToken, result.TerminatorToken.Kind);
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void InvalidEntryPointRejectsNonDeclarationWithoutConsuming()
    {
        var parser = Parser("macro + 1");
        var first = parser.PeekToken();
        Assert.Throws<InvalidOperationException>(() => parser.Parse(GreenList.Empty, GreenList.Empty));
        Assert.Same(first, parser.PeekToken());
    }

    private static MacroDeclarationParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
