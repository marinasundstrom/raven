using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class ConstraintParserContractTests
{
    [Theory]
    [InlineData("where T: class, IDisposable, new()", 0)]
    [InlineData("where T: struct", 0)]
    [InlineData("where T: A & B", 0)]
    [InlineData("where T: func", 0)]
    [InlineData("where T: allows ref struct", 0)]
    [InlineData("where T: class, allows ref struct", 1)]
    [InlineData("where T: allows ref struct, IDisposable", 1)]
    [InlineData("where T: allows ref struct, allows ref struct", 1)]
    [InlineData("where T: class\nwhere U: struct", 0)]
    public void ClausesPreserveSourceDiagnosticsAndNewlineMode(string source, int diagnostics)
    {
        var parser = Create(source);
        parser.SetTreatNewlinesAsTokens(true);
        var clauses = parser.ParseConstraintClauseList();
        var text = string.Concat(Enumerable.Range(0, clauses.SlotCount)
            .Select(index => clauses.GetSlot(index)!.CreateRed().ToFullString()));
        Assert.Equal(source, text);
        Assert.Equal(diagnostics, parser.Diagnostics.Count);
        Assert.True(parser.TreatNewlinesAsTokens);
    }

    [Fact]
    public void MissingConstructorCloseParenNamesExpectedCharacter()
    {
        var parser = Create("new(");
        var constraint = parser.ParseTypeParameterConstraint();
        Assert.Equal(SyntaxKind.ConstructorConstraint, constraint.Kind);
        var diagnostic = Assert.Single(parser.Diagnostics);
        Assert.Equal(")", Assert.Single(diagnostic.Args)?.ToString());
    }

    [Fact]
    public void AbsentClauseDoesNotConsumeNextDeclarationToken()
    {
        var parser = Create("{");
        Assert.Equal(0, parser.ParseConstraintClauseList().SlotCount);
        Assert.Equal(0, parser.Position);
        Assert.Equal(SyntaxKind.OpenBraceToken, parser.PeekToken().Kind);
    }

    private static ConstrainClauseListParser Create(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
