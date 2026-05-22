using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class RemoveRedundantImportCodeFixProvider : CodeFixProvider
{
    private static readonly ImmutableArray<string> FixableIds =
    [
        CompilerDiagnostics.ImportDirectiveRedundantWithGlobalImport.Id,
        UnusedImportDirectiveAnalyzer.DiagnosticId
    ];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!FixableIds.Contains(diagnostic.Id, StringComparer.OrdinalIgnoreCase))
            return;

        if (!diagnostic.Location.IsInSource)
            return;

        var syntaxTree = context.Document.GetSyntaxTreeAsync(context.CancellationToken).GetAwaiter().GetResult();
        var root = syntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var importDirective = FindSourceImport(root, diagnostic.Location.SourceSpan);
        if (importDirective is null)
            return;

        var sourceText = context.Document.GetTextAsync(context.CancellationToken).GetAwaiter().GetResult().ToString();
        var removalSpan = GetLineRemovalSpan(sourceText, importDirective.Span);
        var allRemovalSpans = GetRedundantImportRemovalSpans(context.Document, root, sourceText, context.CancellationToken)
            .Distinct()
            .OrderByDescending(static span => span.Start)
            .ToArray();
        if (allRemovalSpans.Length > 1)
        {
            context.RegisterCodeFix(
                CodeAction.Create(
                    "Remove all redundant or unused imports",
                    (solution, _) =>
                    {
                        var document = solution.GetDocument(context.Document.Id);
                        if (document is null)
                            return solution;

                        var updatedText = document.Text;
                        foreach (var span in allRemovalSpans)
                            updatedText = updatedText.WithChange(new TextChange(span, string.Empty));

                        return solution.WithDocumentText(context.Document.Id, updatedText);
                    }));
        }

        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Remove redundant import",
                context.Document.Id,
                new TextChange(removalSpan, string.Empty)));
    }

    private static IEnumerable<TextSpan> GetRedundantImportRemovalSpans(
        Document document,
        SyntaxNode root,
        string sourceText,
        CancellationToken cancellationToken)
    {
        var diagnostics = GetRedundantImportDiagnostics(document, cancellationToken);
        var diagnosticSpans = diagnostics
            .Where(diagnostic => diagnostic.Location.IsInSource)
            .Select(diagnostic => diagnostic.Location.SourceSpan)
            .ToArray();

        foreach (var import in EnumerateSourceImports(root))
        {
            if (diagnosticSpans.Any(span => import.Span.Start <= span.Start && import.Span.End >= span.End))
                yield return GetLineRemovalSpan(sourceText, import.Span);
        }
    }

    private static IEnumerable<Diagnostic> GetRedundantImportDiagnostics(
        Document document,
        CancellationToken cancellationToken)
    {
        var workspace = document.Solution.Workspace;
        if (workspace is not null)
        {
            return workspace.GetDocumentDiagnosticsWithAnalyzers(
                    document.Project.Id,
                    document.Id,
                    cancellationToken: cancellationToken)
                .Where(IsFixableImportDiagnostic)
                .ToArray();
        }

        var syntaxTree = document.GetSyntaxTreeAsync(cancellationToken).GetAwaiter().GetResult();
        if (syntaxTree is null)
            return [];

        var semanticModel = document.GetSemanticModelAsync(cancellationToken).GetAwaiter().GetResult();
        if (semanticModel is null)
            return [];

        var compilation = semanticModel.Compilation;
        var compilerDiagnostics = compilation
            .GetDocumentDiagnostics(syntaxTree, cancellationToken: cancellationToken)
            .Where(IsFixableImportDiagnostic);
        var analyzerDiagnostics = new UnusedImportDirectiveAnalyzer()
            .Analyze(compilation, syntaxTree, cancellationToken)
            .Where(IsFixableImportDiagnostic);

        return compilerDiagnostics.Concat(analyzerDiagnostics).ToArray();
    }

    private static bool IsFixableImportDiagnostic(Diagnostic diagnostic)
    {
        foreach (var id in FixableIds)
        {
            if (string.Equals(diagnostic.Id, id, StringComparison.OrdinalIgnoreCase))
                return true;
        }

        return false;
    }

    private static IEnumerable<ImportDirectiveSyntax> EnumerateSourceImports(SyntaxNode root)
    {
        if (root is not CompilationUnitSyntax compilationUnit)
            yield break;

        foreach (var import in compilationUnit.Imports)
            yield return import;

        foreach (var namespaceDeclaration in compilationUnit.Members.OfType<BaseNamespaceDeclarationSyntax>())
        {
            foreach (var import in namespaceDeclaration.Imports)
                yield return import;
        }
    }

    private static ImportDirectiveSyntax? FindSourceImport(SyntaxNode root, TextSpan diagnosticSpan)
    {
        if (root is CompilationUnitSyntax compilationUnit)
        {
            var import = FindImport(compilationUnit.Imports, diagnosticSpan);
            if (import is not null)
                return import;

            foreach (var namespaceDeclaration in compilationUnit.Members.OfType<BaseNamespaceDeclarationSyntax>())
            {
                import = FindImport(namespaceDeclaration.Imports, diagnosticSpan);
                if (import is not null)
                    return import;
            }
        }

        var node = root.FindNode(diagnosticSpan, getInnermostNodeForTie: true);
        return node?.FirstAncestorOrSelf<ImportDirectiveSyntax>();
    }

    private static ImportDirectiveSyntax? FindImport(IEnumerable<ImportDirectiveSyntax> imports, TextSpan diagnosticSpan)
        => imports
            .Where(import => import.Span.Start <= diagnosticSpan.Start && import.Span.End >= diagnosticSpan.End)
            .OrderBy(import => import.Span.Length)
            .FirstOrDefault();

    private static TextSpan GetLineRemovalSpan(string text, TextSpan span)
    {
        var start = span.Start;
        while (start > 0 && text[start - 1] is not '\r' and not '\n')
            start--;

        var end = span.End;
        while (end < text.Length && text[end] is not '\r' and not '\n')
            end++;

        if (end < text.Length)
        {
            if (text[end] == '\r' && end + 1 < text.Length && text[end + 1] == '\n')
                end += 2;
            else
                end++;
        }

        return TextSpan.FromBounds(start, end);
    }
}
