using System.Collections.Immutable;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class PreferValInsteadOfLetCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds =
        [PreferValInsteadOfLetAnalyzer.PreferValInsteadOfLetDiagnosticId];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!string.Equals(diagnostic.Id, PreferValInsteadOfLetAnalyzer.PreferValInsteadOfLetDiagnosticId, StringComparison.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        var change = new TextChange(diagnostic.Location.SourceSpan, "val");
        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Replace 'let' with 'val'",
                context.Document.Id,
                change));
    }
}
