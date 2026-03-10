using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class SingleStatementBlockBodyCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds = [SingleStatementBlockBodyAnalyzer.DiagnosticId];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!string.Equals(diagnostic.Id, SingleStatementBlockBodyAnalyzer.DiagnosticId, StringComparison.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        var syntaxTree = context.Document.GetSyntaxTreeAsync(context.CancellationToken).GetAwaiter().GetResult();
        var root = syntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var block = root.FindNode(diagnostic.Location.SourceSpan)?.FirstAncestorOrSelf<BlockStatementSyntax>();
        if (block is null)
            return;

        if (!SingleStatementBlockBodyAnalyzer.TryGetConvertibleExpression(block, out var expression))
            return;

        var change = new TextChange(block.Span, $"=> {expression}");
        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Convert to expression body",
                context.Document.Id,
                change));
    }
}
