using System.Collections.Immutable;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class NonNullDeclarationsCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds = [NonNullDeclarationsAnalyzer.DiagnosticId];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!string.Equals(diagnostic.Id, NonNullDeclarationsAnalyzer.DiagnosticId, StringComparison.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        var args = diagnostic.GetMessageArgs();
        if (args.Length < 1 || args[0] is not string suggestedType || string.IsNullOrWhiteSpace(suggestedType))
            return;

        var change = new TextChange(diagnostic.Location.SourceSpan, suggestedType);
        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                $"Replace with '{suggestedType}'",
                context.Document.Id,
                change));
    }
}
