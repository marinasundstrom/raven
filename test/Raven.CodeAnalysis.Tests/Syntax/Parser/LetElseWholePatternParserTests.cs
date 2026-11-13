using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class LetElseWholePatternParserTests
{
    [Theory]
    [InlineData("[item]", SyntaxKind.SequencePattern)]
    [InlineData("(first, second)", SyntaxKind.PositionalPattern)]
    public void GuardedDeclarationPreservesWholeDesignation(string pattern, SyntaxKind expectedKind)
    {
        var context = new BaseParseContext(new Lexer(new StringReader($"let {pattern} whole = value else {{ return }}")));
        var parser = new StatementSyntaxParser(context);
        var statement = Assert.IsType<PatternDeclarationAssignmentStatementSyntax>(parser.ParseStatement().CreateRed());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(expectedKind, statement.Left.Kind);
        var designation = statement.Left switch
        {
            SequencePatternSyntax sequence => sequence.Designation,
            PositionalPatternSyntax positional => positional.Designation,
            _ => throw new InvalidOperationException()
        };
        Assert.Equal("whole", Assert.IsType<SingleVariableDesignationSyntax>(designation).Identifier.ValueText);
        Assert.NotNull(statement.ElseClause);
        Assert.Equal("value", statement.Right.ToString());
    }
}
