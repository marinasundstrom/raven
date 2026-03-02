using System.Collections.Immutable;
using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class MergeStringLiteralConcatenationCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds =
        [MergeStringLiteralConcatenationAnalyzer.DiagnosticId];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!string.Equals(diagnostic.Id, MergeStringLiteralConcatenationAnalyzer.DiagnosticId, StringComparison.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        var root = context.Document.SyntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var node = root.FindNode(diagnostic.Location.SourceSpan);
        if (node is null)
            return;

        var concat = node as BinaryExpressionSyntax
            ?? node.AncestorsAndSelf().OfType<BinaryExpressionSyntax>().FirstOrDefault();
        if (concat is null || concat.Kind != SyntaxKind.AddExpression)
            return;

        concat = GetTopmostConcat(concat);

        var parts = new List<ExpressionSyntax>();
        FlattenConcat(concat, parts);
        if (parts.Count < 2 || !parts.All(IsStringLiteral))
            return;

        var merged = string.Concat(parts.Cast<LiteralExpressionSyntax>().Select(static literal => literal.Token.ValueText));
        var replacement = "\"" + EscapeStringLiteral(merged) + "\"";

        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Merge string literals",
                context.Document.Id,
                new TextChange(concat.Span, replacement)));
    }

    private static BinaryExpressionSyntax GetTopmostConcat(BinaryExpressionSyntax expression)
    {
        var current = expression;

        while (current.Parent is BinaryExpressionSyntax parent &&
               parent.Kind == SyntaxKind.AddExpression)
        {
            current = parent;
        }

        return current;
    }

    private static void FlattenConcat(ExpressionSyntax expression, List<ExpressionSyntax> parts)
    {
        if (expression is BinaryExpressionSyntax add &&
            add.Kind == SyntaxKind.AddExpression)
        {
            FlattenConcat(add.Left, parts);
            FlattenConcat(add.Right, parts);
            return;
        }

        parts.Add(expression);
    }

    private static bool IsStringLiteral(ExpressionSyntax expression)
        => expression is LiteralExpressionSyntax literal &&
           literal.Kind == SyntaxKind.StringLiteralExpression;

    private static string EscapeStringLiteral(string text)
    {
        var builder = new StringBuilder(text.Length);

        foreach (var ch in text)
        {
            switch (ch)
            {
                case '\\': builder.Append("\\\\"); break;
                case '"': builder.Append("\\\""); break;
                case '\0': builder.Append("\\0"); break;
                case '\a': builder.Append("\\a"); break;
                case '\b': builder.Append("\\b"); break;
                case '\f': builder.Append("\\f"); break;
                case '\n': builder.Append("\\n"); break;
                case '\r': builder.Append("\\r"); break;
                case '\t': builder.Append("\\t"); break;
                case '\v': builder.Append("\\v"); break;
                default:
                    if (char.IsControl(ch))
                        builder.Append("\\u").Append(((int)ch).ToString("X4"));
                    else
                        builder.Append(ch);
                    break;
            }
        }

        return builder.ToString();
    }
}
