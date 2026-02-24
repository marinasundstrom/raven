using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedPropertyCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds = [UnusedPropertyAnalyzer.DiagnosticId];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!string.Equals(diagnostic.Id, UnusedPropertyAnalyzer.DiagnosticId, StringComparison.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        var syntaxTree = context.Document.GetSyntaxTreeAsync(context.CancellationToken).GetAwaiter().GetResult();
        var root = syntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var node = root.FindNode(diagnostic.Location.SourceSpan, getInnermostNodeForTie: true);
        var propertyDecl = node?.FirstAncestorOrSelf<PropertyDeclarationSyntax>();
        if (propertyDecl is null)
            return;

        var span = propertyDecl.FullSpan;
        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Remove unused property",
                context.Document.Id,
                new TextChange(span, string.Empty)));
    }
}
