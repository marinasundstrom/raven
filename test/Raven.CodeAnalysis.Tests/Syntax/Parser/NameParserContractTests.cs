using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class NameParserContractTests
{
    [Theory]
    [InlineData("A.B<C<int>>", SyntaxKind.QualifiedName)]
    [InlineData("int?", SyntaxKind.NullableType)]
    [InlineData("int[]", SyntaxKind.ArrayType)]
    [InlineData("int[10][,]", SyntaxKind.ArrayType)]
    [InlineData("A | B & C", SyntaxKind.UnionType)]
    [InlineData("A & B", SyntaxKind.IntersectionType)]
    [InlineData("()", SyntaxKind.UnitType)]
    [InlineData("(int, string)", SyntaxKind.TupleType)]
    [InlineData("(int)", SyntaxKind.ParenthesizedType)]
    [InlineData("(int, string) -> bool", SyntaxKind.FunctionType)]
    [InlineData("int -> string", SyntaxKind.FunctionType)]
    [InlineData("&int", SyntaxKind.ByRefType)]
    [InlineData("*int", SyntaxKind.PointerType)]
    public void TypeNamesPreserveShapeAndSource(string source, SyntaxKind kind)
    {
        var parser = Create(source);
        var result = parser.ParseTypeName();
        Assert.Equal(kind, result.Kind);
        Assert.Equal(source, result.CreateRed().ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Fact]
    public void NestedGenericClosersLeaveAssignmentTokenForCaller()
    {
        var parser = Create("A<B<C>>=value");
        var result = parser.ParseTypeName();
        Assert.Equal("A<B<C>>", result.CreateRed().ToFullString());
        Assert.Equal(SyntaxKind.EqualsToken, parser.ReadToken().Kind);
        Assert.Equal("value", parser.ReadToken().Text);
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void OmittedTypeArgumentsPreserveSeparators()
    {
        var parser = new NameSyntaxParser(new BaseParseContext(new Lexer(new StringReader("Dictionary<,>"))), true);
        var result = parser.ParseTypeName();
        Assert.Equal("Dictionary<,>", result.CreateRed().ToFullString());
        Assert.Empty(parser.Diagnostics);
    }

    [Fact]
    public void SingleElementTupleReportsDiagnosticAndKeepsSource()
    {
        var parser = Create("(int,)");
        var result = parser.ParseTypeName();
        Assert.Equal(SyntaxKind.TupleType, result.Kind);
        Assert.Equal("(int,)", result.CreateRed().ToFullString());
        Assert.Single(parser.Diagnostics);
    }

    private static NameSyntaxParser Create(string source) => new(new BaseParseContext(new Lexer(new StringReader(source))));
}
