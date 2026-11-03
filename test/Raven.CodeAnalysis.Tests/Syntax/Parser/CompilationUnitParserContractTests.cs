using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class CompilationUnitParserContractTests
{
    [Theory]
    [InlineData("", 0)]
    [InlineData("// comment\n", 0)]
    [InlineData("class Item {}", 1)]
    [InlineData("namespace Example { class Item {} }", 1)]
    [InlineData("module Example\nclass Item {}", 1)]
    [InlineData("import System.*\nalias Name = System.String;\nclass Item {}", 1)]
    [InlineData("[assembly: A]\nclass Item {}", 1)]
    [InlineData("import System.*\n[assembly: A]\nclass Item {}", 1)]
    [InlineData("global { import System.* }\nclass Item {}", 2)]
    [InlineData("let value = 1\nvalue + 2", 2)]
    [InlineData("func Identity<T>(value: T) -> T => value", 1)]
    [InlineData("extern macro Build() -> int;", 1)]
    public void ParseRetainsSourceAndMembers(string source, int count)
    {
        var parser = Parser(source);
        var result = (CompilationUnitSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Equal(count, result.Members.Count);
        Assert.Equal(SyntaxKind.EndOfFileToken, result.EndOfFileToken.Kind);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("class Item {}\nimport System.*")]
    [InlineData("class Item {}\nalias A = System.String;")]
    public void MisplacedDirectivesPreserveSourceAndDiagnostic(string source)
    {
        var parser = Parser(source);
        var result = (CompilationUnitSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(result.Imports);
        Assert.Empty(result.Aliases);
        Assert.IsType<IncompleteMemberDeclarationSyntax>(result.Members[1]);
        Assert.Single(parser.Diagnostics);
    }

    [Theory]
    [InlineData("global { + }")]
    [InlineData("global { + import System.* }")]
    [InlineData("global { import System.*\n+ }")]
    [InlineData("global { +")]
    public void GlobalImportBlockPreservesUnexpectedTokens(string source)
    {
        var parser = Parser(source);
        var result = (CompilationUnitSyntax)parser.Parse().CreateRed();
        Assert.Equal(source, result.ToFullString());
        Assert.NotEmpty(parser.Diagnostics);
    }

    [Fact]
    public void MemberEntryPointLeavesFollowingDeclarationUnconsumed()
    {
        var parser = Parser("class First {}\nclass Second {}");
        var member = Assert.IsType<ClassDeclarationSyntax>(parser.ParseMemberDeclaration()!.CreateRed());
        Assert.Equal("First", member.Identifier.ValueText);
        Assert.Equal(SyntaxKind.ClassKeyword, parser.PeekToken().Kind);
        Assert.Empty(parser.Diagnostics);
    }

    [Theory]
    [InlineData("import System.*")]
    [InlineData("alias Name = System.String;")]
    public void MemberEntryPointDoesNotReturnDirectivesAsIncompleteMembers(string source)
    {
        Assert.Null(Parser(source).ParseMemberDeclaration());
    }

    [Fact]
    public void MemberEntryPointReturnsNullAtEndOfFile()
    {
        Assert.Null(Parser("").ParseMemberDeclaration());
    }

    private static CompilationUnitSyntaxParser Parser(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
