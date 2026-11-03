using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class LanguageParserContractTests
{
    [Theory]
    [InlineData(typeof(CompilationUnitSyntax), "class C {}", SyntaxKind.CompilationUnit)]
    [InlineData(typeof(MemberDeclarationSyntax), "class C {}", SyntaxKind.ClassDeclaration)]
    [InlineData(typeof(PatternSyntax), "> 1 and < 10", SyntaxKind.AndPattern)]
    [InlineData(typeof(TypeSyntax), "System.String", SyntaxKind.QualifiedName)]
    [InlineData(typeof(StatementSyntax), "return 1", SyntaxKind.ReturnStatement)]
    [InlineData(typeof(ReturnStatementSyntax), "return 1", SyntaxKind.ReturnStatement)]
    [InlineData(typeof(BlockSyntax), "{ 1 }", SyntaxKind.Block)]
    [InlineData(typeof(ExpressionSyntax), "1 + 2", SyntaxKind.AddExpression)]
    [InlineData(typeof(NameSyntax), "System.String", SyntaxKind.QualifiedName)]
    [InlineData(typeof(IdentifierNameSyntax), "Name", SyntaxKind.IdentifierName)]
    public void DispatchesRequestedSyntaxFamily(Type requestedType, string source, SyntaxKind kind)
    {
        var parser = new LanguageParser(null, new ParseOptions());
        var result = parser.ParseSyntaxWithDiagnostics(requestedType, SourceText.From(source), 0);
        Assert.True(result.HasValue);
        var parsed = result.Value;
        var red = parsed.Root.CreateRed();
        Assert.True(requestedType.IsInstanceOfType(red), red.GetType().FullName);
        Assert.Equal(kind, red.Kind);
        Assert.Equal(source, red.ToFullString());
        Assert.Empty(parsed.Diagnostics);
        Assert.Equal(source.Length, parsed.ConsumedPosition);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FullTextOptionControlsTrailingTokenDiagnostic(bool consumeFullText)
    {
        var parser = new LanguageParser(null, new ParseOptions());
        var result = parser.ParseSyntaxWithDiagnostics(typeof(ExpressionSyntax),
            SourceText.From("1 unexpected"), 0, consumeFullText: consumeFullText)!.Value;
        Assert.Equal(SyntaxKind.NumericLiteralExpression, result.Root.Kind);
        Assert.Equal(consumeFullText, result.Diagnostics.Count != 0);
    }

    [Fact]
    public void FragmentStartsAtRequestedOffset()
    {
        var parser = new LanguageParser(null, new ParseOptions());
        const string source = "skip 1 + 2";
        var result = parser.ParseSyntaxWithDiagnostics(typeof(ExpressionSyntax), SourceText.From(source), 5)!.Value;
        Assert.Equal("1 + 2", result.Root.CreateRed().ToFullString());
        Assert.Equal(source.Length, result.ConsumedPosition);
        Assert.Empty(result.Diagnostics);
    }

    [Fact]
    public void UnsupportedRequestedTypeReturnsNull()
    {
        var parser = new LanguageParser(null, new ParseOptions());
        Assert.Null(parser.ParseSyntaxWithDiagnostics(typeof(string), SourceText.From("value"), 0));
        Assert.Null(parser.ParseSyntax(typeof(string), SourceText.From("value"), 0));
    }

    [Fact]
    public void MissingDelimiterDiagnosticsAreReturned()
    {
        var parser = new LanguageParser(null, new ParseOptions());
        var result = parser.ParseSyntaxWithDiagnostics(typeof(ExpressionSyntax), SourceText.From("[1"), 0)!.Value;
        Assert.NotEmpty(result.Diagnostics);
        Assert.Equal("[1", result.Root.CreateRed().ToFullString());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void StatementEntryPointHonorsFullTextOption(bool consumeFullText)
    {
        var parser = new LanguageParser(null, new ParseOptions());
        var root = parser.ParseStatement(SourceText.From("return 1; return 2"), consumeFullText: consumeFullText);
        Assert.Equal(consumeFullText, root.GetDiagnostics().Any());
    }

    [Fact]
    public void SyntaxEntryPointPreservesRecoveryDiagnostics()
    {
        var parser = new LanguageParser(null, new ParseOptions());
        var root = parser.ParseSyntax(typeof(ExpressionSyntax), SourceText.From("[1"), 0);
        Assert.NotNull(root);
        Assert.NotEmpty(root.GetDiagnostics());
    }

    [Fact]
    public void ParseResultUsesRecordValueEquality()
    {
        var parser = new LanguageParser(null, new ParseOptions());
        var result = parser.Parse(SourceText.From("let value = 1"));
        var same = new ParseResult(result.Root, result.Diagnostics, result.ConsumedPosition);
        Assert.Equal(result, same);
        Assert.True(result == same);
        Assert.Equal(result.GetHashCode(), same.GetHashCode());
        Assert.NotEqual(result, new ParseResult(result.Root, result.Diagnostics, result.ConsumedPosition + 1));
    }
}
