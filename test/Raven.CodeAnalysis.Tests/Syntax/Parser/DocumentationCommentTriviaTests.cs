namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

using System.IO;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

public class DocumentationCommentTriviaTests
{
    [Fact]
    public void SingleLineDocumentationComment_MergesContiguousLinesIntoMultilineTrivia()
    {
        var code = """
/// <summary>
/// Returns a value.
/// </summary>
func Foo() {}
""";

        var documentationTrivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .Where(t => t.Kind == SyntaxKind.DocumentationCommentTrivia)
            .ToList();

        documentationTrivia.Count.ShouldBe(1);
        documentationTrivia[0].Text.ShouldBe("/// <summary>\n/// Returns a value.\n/// </summary>\n");
    }

    [Fact]
    public void SingleLineDocumentationComment_WithoutAdditionalLines_RemainsSingleLineTrivia()
    {
        var code = """
/// Hello
func Foo() {}
""";

        var documentationTrivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .Where(t => t.Kind == SyntaxKind.DocumentationCommentTrivia)
            .ToList();

        documentationTrivia.Count.ShouldBe(1);
        documentationTrivia[0].Text.ShouldBe("/// Hello\n");
    }

    [Fact]
    public void MultiLineComment_ProducesMultilineTrivia()
    {
        var code = """
/**
 * <summary>
 * Returns a value.
 * </summary>
 */
func Foo() {}
""";

        var trivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .FirstOrDefault(t => t.Kind == SyntaxKind.MultiLineCommentTrivia);

        trivia.ShouldNotBeNull();
    }

    [Fact]
    public void SingleLineDocComments_DoNotEatNewlineTokens_WhenNewlinesAreSignificant()
    {
        var code = """
Foo()

/// <summary>
/// Returns a value.
/// </summary>
func Foo() {}
""";

        var lexer = new Lexer(new StringReader(code));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new StatementSyntaxParser(context);

        var firstStatement = parser.ParseStatement().CreateRed();
        var secondStatement = parser.ParseStatement().CreateRed();

        firstStatement.GetLastToken().Kind.ShouldBe(SyntaxKind.NewLineToken);

        var nextFirstToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        nextFirstToken.LeadingTrivia.ShouldContain(t => t.Kind == SyntaxKind.DocumentationCommentTrivia);
    }

    [Fact]
    public void MultiLineMultilineDocComment_RemainsLeadingTriviaAfterPreviousStatementTerminates()
    {
        var code = """
Foo()

/// <summary>
/// Returns a hash code for the current object.
/// </summary>
/// <remarks>
/// This method is intended to support hash-based collections such as
/// <see cref="System.Collections.Generic.Dictionary{TKey, TValue}"/> and
/// <see cref="System.Collections.Generic.HashSet{T}"/>.
/// </remarks>
/// <returns>
/// A 32-bit signed integer hash code for the current object.
/// </returns>
/// <seealso cref="Equals(object)"/>
func Foo() {}
""";

        var lexer = new Lexer(new StringReader(code));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new StatementSyntaxParser(context);

        var firstStatement = parser.ParseStatement().CreateRed();
        var secondStatement = parser.ParseStatement().CreateRed();

        firstStatement.GetLastToken().Kind.ShouldBe(SyntaxKind.NewLineToken);

        var funcToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        funcToken.LeadingTrivia.ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);
        funcToken.LeadingTrivia.ShouldContain(t => t.Kind == SyntaxKind.DocumentationCommentTrivia);
        funcToken.LeadingTrivia.First(t => t.Kind == SyntaxKind.DocumentationCommentTrivia)
            .Text.StartsWith("/// <summary>").ShouldBeTrue();
    }

    [Fact]
    public void LongMultilineDocComment_IsNotSplitIntoSkippedTokens()
    {
        var code = """
Foo()

/// <summary>
/// Returns a hash code for the current object.
/// </summary>
/// <remarks>
/// This method is intended to support hash-based collections such as
/// <see cref="System.Collections.Generic.Dictionary{TKey, TValue}"/> and
/// <see cref="System.Collections.Generic.HashSet{T}"/>.
/// The default implementation provided by <see cref="object"/> attempts to return
/// different hash codes for different object instances, where reasonably practical.
/// </remarks>
/// <returns>
/// A 32-bit signed integer hash code for the current object.
/// </returns>
/// <seealso cref="Equals(object)"/>
/// <seealso cref="System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(object)"/>
func Foo() {}
""";

        var lexer = new Lexer(new StringReader(code));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new StatementSyntaxParser(context);

        var firstStatement = parser.ParseStatement().CreateRed();
        var secondStatement = parser.ParseStatement().CreateRed();

        firstStatement.GetLastToken().Kind.ShouldBe(SyntaxKind.NewLineToken);
        firstStatement.DescendantTrivia(descendIntoStructuredTrivia: true)
            .ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);

        var funcToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        funcToken.LeadingTrivia.ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);

        var docTrivia = funcToken.LeadingTrivia.First(t => t.Kind == SyntaxKind.DocumentationCommentTrivia);
        docTrivia.Text.ShouldStartWith("/// <summary>\n");
        docTrivia.Text.ShouldContain("// A 32-bit signed integer hash code for the current object.\n");
    }

    [Fact]
    public void TerminatorBeforeDocComment_ComesFromImmediateNewline()
    {
        var code = """
Foo()

/// <summary>
/// Returns a hash code for the current object.
/// </summary>
func Foo() {}
""";

        var lexer = new Lexer(new StringReader(code));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new StatementSyntaxParser(context);

        var firstStatement = parser.ParseStatement().CreateRed();
        var secondStatement = parser.ParseStatement().CreateRed();

        firstStatement.GetLastToken().Kind.ShouldBe(SyntaxKind.NewLineToken);
        firstStatement.DescendantTrivia(descendIntoStructuredTrivia: true)
            .ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);

        var funcToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        funcToken.LeadingTrivia.ShouldContain(t => t.Kind == SyntaxKind.DocumentationCommentTrivia);
        funcToken.LeadingTrivia.ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);
    }

    [Fact]
    public void LongDocCommentBeyondDefaultLexerBuffer_RemainsLeadingTrivia()
    {
        var longLine = "/// " + new string('x', 80);
        var repeatedComment = string.Join("\n", Enumerable.Repeat(longLine, 40));

        var code = $""""
Foo()

{repeatedComment}
"""" + "\nfunc Foo() {}";


        var lexer = new Lexer(new StringReader(code));
        var context = new BaseParseContext(lexer);
        context.SetTreatNewlinesAsTokens(true);

        var parser = new StatementSyntaxParser(context);

        var firstStatement = parser.ParseStatement().CreateRed();
        var secondStatement = parser.ParseStatement().CreateRed();

        firstStatement.GetLastToken().Kind.ShouldBe(SyntaxKind.NewLineToken);

        var funcToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        funcToken.LeadingTrivia.ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);

        var docTrivia = funcToken.LeadingTrivia.First(t => t.Kind == SyntaxKind.DocumentationCommentTrivia);
        docTrivia.Text.ShouldStartWith("/// ");
        docTrivia.Text.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length.ShouldBeGreaterThanOrEqualTo(40);
    }
}
