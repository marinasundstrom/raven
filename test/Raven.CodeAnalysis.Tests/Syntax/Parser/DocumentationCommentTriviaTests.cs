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
            .Where(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia)
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
            .Where(t => t.Kind == SyntaxKind.SingleLineDocumentationCommentTrivia)
            .ToList();

        documentationTrivia.Count.ShouldBe(1);
        documentationTrivia[0].Text.ShouldBe("/// Hello\n");
    }

    [Fact]
    public void MultiLineDocumentationComment_ProducesMultilineTrivia()
    {
        var code = """
/**
 * <summary>
 * Returns a value.
 * </summary>
 */
func Foo() {}
""";

        var documentationTrivia = SyntaxTree.ParseText(code)
            .GetRoot()
            .DescendantTrivia(descendIntoStructuredTrivia: true)
            .FirstOrDefault(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia);

        documentationTrivia.ShouldNotBeNull();

        DocumentationComment.TryParse(documentationTrivia!, DocumentationFormat.Xml, out var comment)
            .ShouldBeTrue();

        comment.ShouldNotBeNull();
        comment!.IsMultiline.ShouldBeTrue();
        comment.Content.ShouldBe("<summary>\nReturns a value.\n</summary>");
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
        nextFirstToken.LeadingTrivia.ShouldContain(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia);
    }

    [Fact]
    public void MultiLineTripleSlashDocComment_RemainsLeadingTriviaAfterPreviousStatementTerminates()
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
        funcToken.LeadingTrivia.ShouldContain(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia);
        funcToken.LeadingTrivia.First(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia)
            .Text.StartsWith("/// <summary>").ShouldBeTrue();
    }

    [Fact]
    public void LongTripleSlashDocComment_IsNotSplitIntoSkippedTokens()
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
///
/// ## Hash Code Contract
///
/// - Calling this method multiple times on the same object **must return the same value**
///   during a single execution of the program, provided that no data used by
///   <see cref="Equals(object)"/> comparisons is modified.
///   The returned value **is not required** to be stable across different program executions.
///
/// - If two objects are considered equal according to <see cref="Equals(object)"/>,
///   they **must return the same hash code**.
///
/// - If two objects are not equal according to <see cref="Equals(object)"/>,
///   they are **not required** to return different hash codes.
///   However, returning distinct hash codes for unequal objects generally improves
///   the performance of hash-based collections.
///
/// ## Implementation Notes
///
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

        var funcToken = secondStatement.GetFirstToken(includeZeroWidth: true);
        funcToken.LeadingTrivia.ShouldAllBe(t => t.Kind != SyntaxKind.SkippedTokensTrivia);

        var docTrivia = funcToken.LeadingTrivia.First(t => t.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia);
        docTrivia.Text.ShouldStartWith("/// <summary>\n");
        docTrivia.Text.ShouldContain("///   The returned value **is not required** to be stable across different program executions.\n");
    }
}
