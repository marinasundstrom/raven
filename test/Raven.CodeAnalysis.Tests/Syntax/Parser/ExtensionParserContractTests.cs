using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class ExtensionParserContractTests
{
    [Theory]
    [InlineData("extension Text for string {}")]
    [InlineData("extension for string {}")]
    [InlineData("extension Text<T> for T where T: class {}")]
    [InlineData("extension Text<T: class> for T {}")]
    public void ExtensionHeaderPreservesSource(string source)
    {
        var parser = Parser(source);
        var result = (ExtensionDeclarationSyntax)parser.Parse(InternalSyntax.SyntaxList.Empty, InternalSyntax.SyntaxList.Empty).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(result.Members);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("func Size() -> int => self.Length", SyntaxKind.MethodDeclaration)]
    [InlineData("func Map<T>(value: T) -> T => value", SyntaxKind.MethodDeclaration)]
    [InlineData("val Count: int => self.Length", SyntaxKind.PropertyDeclaration)]
    [InlineData("var Count: int { get => 1; set {} }", SyntaxKind.PropertyDeclaration)]
    [InlineData("val Default: int = 1", SyntaxKind.PropertyDeclaration)]
    [InlineData("val self[index: int]: char => self[index]", SyntaxKind.IndexerDeclaration)]
    [InlineData("event Changed: System.Action { add; remove; }", SyntaxKind.EventDeclaration)]
    [InlineData("static func +(left: string, right: string) -> string => left", SyntaxKind.OperatorDeclaration)]
    [InlineData("static func checked +(left: int, right: int) -> int => left + right", SyntaxKind.OperatorDeclaration)]
    [InlineData("static func explicit(value: string) -> int => 0", SyntaxKind.ConversionOperatorDeclaration)]
    public void MembersRetainDeclarationShapes(string member, SyntaxKind kind)
    {
        var source = $"extension Text for string {{ {member} }}";
        var parser = Parser(source);
        var result = (ExtensionDeclarationSyntax)parser.Parse(InternalSyntax.SyntaxList.Empty, InternalSyntax.SyntaxList.Empty).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(kind, Assert.Single(result.Members).Kind);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("extension Text for string { + }")]
    [InlineData("extension Text for string { val Count: int { + } }")]
    public void RecoveryPreservesUnexpectedTokens(string source)
    {
        var parser = Parser(source);
        var result = (ExtensionDeclarationSyntax)parser.Parse(InternalSyntax.SyntaxList.Empty, InternalSyntax.SyntaxList.Empty).CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.NotEmpty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    private static ExtensionDeclarationParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
