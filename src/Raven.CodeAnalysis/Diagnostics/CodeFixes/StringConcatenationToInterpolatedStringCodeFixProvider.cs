using System.Collections.Immutable;
using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class StringConcatenationToInterpolatedStringCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds =
        [StringConcatenationAnalyzer.DiagnosticId];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!string.Equals(diagnostic.Id, StringConcatenationAnalyzer.DiagnosticId, StringComparison.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        // 1) Find the concat expression to rewrite
        var root = context.Document.SyntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var span = diagnostic.Location.SourceSpan;

        // Find node that covers the diagnostic span; diagnostic is typically on the '+' token.
        var node = root.FindNode(span);
        if (node is null)
            return;

        // Walk up to a BinaryExpressionSyntax (+) if needed.
        var concat = node as BinaryExpressionSyntax
            ?? node.AncestorsAndSelf().OfType<BinaryExpressionSyntax>().FirstOrDefault();

        if (concat is null || concat.Kind != SyntaxKind.AddExpression)
            return;

        // IMPORTANT: the analyzer should have reported only topmost, but we can still normalize:
        // climb while parent is also string concat.
        concat = GetTopmostConcat(concat);

        // 2) Flatten chain into parts (syntax only; analyzer already ensured string type)
        var parts = new List<ExpressionSyntax>();
        FlattenConcat(concat, parts);

        if (parts.Count < 2)
            return;

        // Optional: if everything is a string literal, skip (constant fold is better).
        if (parts.All(IsStringLiteral))
            return;

        // 3) Build interpolation text
        var replacementText = BuildInterpolatedString(parts);

        // 4) Replace whole expression span
        var change = new TextChange(concat.Span, replacementText);

        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Convert to interpolated string",
                context.Document.Id,
                change));
    }

    private static BinaryExpressionSyntax GetTopmostConcat(BinaryExpressionSyntax expr)
    {
        // If Raven has Parent typed as SyntaxNode, this works.
        var current = expr;

        while (current.Parent is BinaryExpressionSyntax parent &&
               parent.Kind == SyntaxKind.AddExpression)
        {
            current = parent;
        }

        return current;
    }

    private static void FlattenConcat(ExpressionSyntax expr, List<ExpressionSyntax> parts)
    {
        if (expr is BinaryExpressionSyntax add &&
            add.Kind == SyntaxKind.AddExpression)
        {
            FlattenConcat(add.Left, parts);
            FlattenConcat(add.Right, parts);
            return;
        }

        parts.Add(expr);
    }

    private static bool IsStringLiteral(ExpressionSyntax expr)
    {
        return expr is LiteralExpressionSyntax lit &&
               lit.Kind == SyntaxKind.StringLiteralExpression;
    }

    private static string BuildInterpolatedString(List<ExpressionSyntax> parts)
    {
        // This is text-based; later you can upgrade to building InterpolatedStringExpressionSyntax directly.
        var sb = new StringBuilder();
        sb.Append('"');

        foreach (var part in parts)
        {
            if (part is LiteralExpressionSyntax lit && lit.Kind == SyntaxKind.StringLiteralExpression)
            {
                // Assumption: lit.Token.Text includes quotes, like "\"hello\""
                // If Raven exposes ValueText or similar, use that instead.
                var text = GetStringLiteralContents(lit);
                sb.Append(EscapeInterpolatedText(text));
            }
            else
            {
                sb.Append("${");
                sb.Append(part.ToFullString().Trim());
                sb.Append("}");
            }
        }

        sb.Append("\"");
        return sb.ToString();
    }

    private static string GetStringLiteralContents(LiteralExpressionSyntax lit)
    {
        // You likely have something like:
        // - lit.Token.ValueText (Roslyn style)
        // - or lit.Token.Text (includes quotes)
        //
        // This fallback strips surrounding quotes if present.
        var tokenText = lit.Token.Text;

        if (tokenText.Length >= 2 && tokenText[0] == '"' && tokenText[^1] == '"')
            return tokenText.Substring(1, tokenText.Length - 2);

        return tokenText;
    }

    private static string EscapeInterpolatedText(string text)
    {
        // In interpolated strings:
        // - " stays as " because we’re already inside quotes; we must escape it.
        // - { and } must be doubled.
        // - backslashes depend on whether you generate regular or verbatim interpolated strings (we do regular).
        var sb = new StringBuilder(text.Length);

        foreach (var ch in text)
        {
            switch (ch)
            {
                case '{': sb.Append("{{"); break;
                case '}': sb.Append("}}"); break;
                case '"': sb.Append("\\\""); break;
                case '\\': sb.Append("\\\\"); break;
                default: sb.Append(ch); break;
            }
        }

        return sb.ToString();
    }
}
