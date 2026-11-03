using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class NamespaceParserContractTests
{
    [Theory]
    [InlineData("namespace Example {}", 0, false)]
    [InlineData("namespace Example { class Item {} }", 1, false)]
    [InlineData("namespace Example;\nclass Item {}", 1, true)]
    [InlineData("namespace Example\nclass Item {}", 1, true)]
    [InlineData("module Example { class Item {} }", 1, false)]
    [InlineData("module Example\nclass Item {}", 1, true)]
    [InlineData("namespace Example { namespace Nested {} }", 1, false)]
    [InlineData("namespace Example { extern macro Build(); }", 1, false)]
    public void DeclarationPreservesSourceAndMembers(string source, int count, bool fileScoped)
    {
        var parser = Parser(source);
        var result = (BaseNamespaceDeclarationSyntax)parser.ParseNamespaceDeclaration().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(count, result.Members.Count);
        Assert.Equal(fileScoped, result is FileScopedNamespaceDeclarationSyntax);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("namespace Example { import System.*\nclass Item {} }", false)]
    [InlineData("namespace Example { class Item {}\nimport System.* }", true)]
    [InlineData("namespace Example;\nclass Item {}\nimport System.*", true)]
    public void ImportOrderingRetainsDiagnostics(string source, bool diagnostics)
    {
        var parser = Parser(source);
        var result = (BaseNamespaceDeclarationSyntax)parser.ParseNamespaceDeclaration().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(diagnostics ? 0 : 1, result.Imports.Count);
        Assert.Equal(diagnostics, parser.Diagnostics.Count > 0);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    private static NamespaceDeclarationParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
