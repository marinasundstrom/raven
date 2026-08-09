namespace Raven.CodeAnalysis.Syntax;

public partial class SyntaxTree
{
    /// <summary>
    /// Classifies whether this script or interactive syntax tree is ready to compile.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token.</param>
    /// <returns>The submission completeness classification.</returns>
    /// <exception cref="InvalidOperationException">
    /// The syntax tree was parsed as regular source rather than a script or interactive submission.
    /// </exception>
    public SubmissionCompleteness GetSubmissionCompleteness(CancellationToken cancellationToken = default)
    {
        if (Options.Kind is not (SourceCodeKind.Script or SourceCodeKind.Interactive))
            throw new InvalidOperationException("Submission completeness is only available for script or interactive syntax trees.");

        cancellationToken.ThrowIfCancellationRequested();

        var root = GetRoot(cancellationToken);
        var errors = GetDiagnostics(cancellationToken)
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();

        if (HasUnterminatedDirective(errors) ||
            HasUnterminatedLexicalConstruct(root, errors) ||
            HasTrailingMissingSyntax(root, cancellationToken))
        {
            return SubmissionCompleteness.Incomplete;
        }

        return errors.Length == 0
            ? SubmissionCompleteness.Complete
            : SubmissionCompleteness.Invalid;
    }

    private bool HasUnterminatedLexicalConstruct(
        CompilationUnitSyntax root,
        IReadOnlyList<Diagnostic> errors)
    {
        var text = GetText().ToString();
        if (errors.Any(diagnostic => IsUnterminatedAtEndOfInput(diagnostic, text)))
        {
            return true;
        }

        foreach (var trivia in root.DescendantTrivia(descendIntoStructuredTrivia: true))
        {
            if (trivia.Kind == SyntaxKind.MultiLineCommentTrivia &&
                trivia.FullSpan.End == Length &&
                !trivia.Text.EndsWith("*/", StringComparison.Ordinal))
            {
                return true;
            }
        }

        var finalToken = root.DescendantTokens(descendIntoTrivia: true)
            .LastOrDefault(static token =>
                !token.IsMissing &&
                token.Width > 0 &&
                token.Kind is not (SyntaxKind.EndOfFileToken or SyntaxKind.None));

        return finalToken.Kind == SyntaxKind.MultiLineStringLiteralToken &&
            finalToken.Span.End == Length &&
            (finalToken.Text.Length < 6 || !finalToken.Text.EndsWith("\"\"\"", StringComparison.Ordinal));

        static bool IsUnterminatedSingleLineLiteral(Diagnostic diagnostic)
            => diagnostic.Descriptor == CompilerDiagnostics.NewlineInConstant ||
                diagnostic.Descriptor == CompilerDiagnostics.UnterminatedCharacterLiteral;

        static bool IsUnterminatedAtEndOfInput(Diagnostic diagnostic, string text)
        {
            if (!IsUnterminatedSingleLineLiteral(diagnostic))
                return false;

            var start = Math.Clamp(diagnostic.Location.SourceSpan.Start, 0, text.Length);
            return !ContainsLineBreak(text.AsSpan(start));
        }

        static bool ContainsLineBreak(ReadOnlySpan<char> value)
        {
            foreach (var character in value)
            {
                if (character is '\r' or '\n' or '\u0085' or '\u2028' or '\u2029')
                    return true;
            }

            return false;
        }
    }

    private static bool HasUnterminatedDirective(IReadOnlyList<Diagnostic> errors)
        => errors.Any(static diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MissingEndIfDirective);

    private static bool HasTrailingMissingSyntax(
        CompilationUnitSyntax root,
        CancellationToken cancellationToken)
    {
        var tokens = root.DescendantTokens(descendIntoTrivia: true).ToArray();
        var finalWrittenToken = tokens.LastOrDefault(static token =>
            !token.IsMissing &&
            token.Width > 0 &&
            token.Kind is not (SyntaxKind.EndOfFileToken or SyntaxKind.None));

        var writtenEnd = finalWrittenToken.Kind == SyntaxKind.None
            ? 0
            : finalWrittenToken.Span.End;

        foreach (var descendant in root.DescendantNodesAndTokens(descendIntoTrivia: true))
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (descendant.IsToken)
            {
                var token = descendant.AsToken();
                if (token.IsMissing && token.SpanStart >= writtenEnd)
                    return true;
            }
            else if (descendant.AsNode() is { IsMissing: true } node && node.Span.Start >= writtenEnd)
            {
                return true;
            }
        }

        return false;
    }
}
