using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.MissingToken(kind);

    public static SyntaxToken Token(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.Token(kind);

    public static SyntaxToken Token(int rawKind, string text, int position = 0, object? value = null)
    {
        ArgumentNullException.ThrowIfNull(text);
        ArgumentOutOfRangeException.ThrowIfNegative(position);

        var green = new InternalSyntax.SyntaxToken(
            rawKind,
            SyntaxKind.None,
            text,
            value,
            text.Length);
        return new SyntaxToken(green, parent: null, position);
    }

    public static SyntaxToken Identifier(string text) => (SyntaxToken)InternalSyntax.SyntaxFactory.IdentifierToken(text);

    public static SyntaxToken Literal(int value) => (SyntaxToken)InternalSyntax.SyntaxFactory.Literal(value);
    public static SyntaxToken Literal(string text) => (SyntaxToken)InternalSyntax.SyntaxFactory.Literal(text);

    public static SyntaxToken Literal(string text, int value) => (SyntaxToken)InternalSyntax.SyntaxFactory.Literal(text, value);
    public static SyntaxToken Literal(string text, string value) => (SyntaxToken)InternalSyntax.SyntaxFactory.Literal(text, value);

    public static SyntaxToken EndOfFileToken => Token(SyntaxKind.EndOfFileToken);

    public static ExpressionSyntax ParseExpression(string text, ParseOptions? options = null)
        => ParseExpression(SourceText.From(text), options);

    public static ExpressionSyntax ParseExpression(SourceText sourceText, ParseOptions? options = null, int position = 0)
    {
        var parser = new InternalSyntax.Parser.LanguageParser("file", options ?? new ParseOptions());
        var node = parser.ParseSyntax(typeof(ExpressionSyntax), sourceText, position);

        return node?.CreateRed() as ExpressionSyntax ?? new ExpressionSyntax.Missing();
    }

    public static StatementSyntax ParseStatement(string text, ParseOptions? options = null)
        => ParseStatement(SourceText.From(text), options);

    public static StatementSyntax ParseStatement(SourceText sourceText, ParseOptions? options = null, int position = 0)
    {
        var parser = new InternalSyntax.Parser.LanguageParser("file", options ?? new ParseOptions());
        return (StatementSyntax)parser.ParseStatement(sourceText, position).CreateRed();
    }

    public static TypeSyntax ParseType(string text, ParseOptions? options = null)
        => ParseType(SourceText.From(text), options);

    public static TypeSyntax ParseType(SourceText sourceText, ParseOptions? options = null, int position = 0)
    {
        var parser = new InternalSyntax.Parser.LanguageParser("file", options ?? new ParseOptions());
        var node = parser.ParseSyntax(typeof(TypeSyntax), sourceText, position);

        return node?.CreateRed() as TypeSyntax
            ?? IdentifierName(MissingToken(SyntaxKind.IdentifierToken));
    }

    public static PatternSyntax ParsePattern(string text, ParseOptions? options = null)
        => ParsePattern(SourceText.From(text), options);

    public static PatternSyntax ParsePattern(SourceText sourceText, ParseOptions? options = null, int position = 0)
    {
        var parser = new InternalSyntax.Parser.LanguageParser("file", options ?? new ParseOptions());
        var node = parser.ParseSyntax(typeof(PatternSyntax), sourceText, position);

        return node?.CreateRed() as PatternSyntax
            ?? ConstantPattern(new ExpressionSyntax.Missing());
    }

    public static CompilationUnitSyntax ParseCompilationUnit(string text, ParseOptions? options = null)
        => ParseCompilationUnit(SourceText.From(text), options);

    public static CompilationUnitSyntax ParseCompilationUnit(SourceText sourceText, ParseOptions? options = null)
    {
        var parser = new InternalSyntax.Parser.LanguageParser("file", options ?? new ParseOptions());
        var result = parser.Parse(sourceText);

        return (CompilationUnitSyntax)result.Root.CreateRed();
    }
}
