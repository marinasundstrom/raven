using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.Playground.Services;

public sealed class PlaygroundLanguageService
{
    private readonly object _gate = new();
    private readonly AdhocWorkspace _workspace = new();
    private readonly ProjectId _projectId;
    private readonly DocumentId _documentId;

    public PlaygroundLanguageService(PlaygroundFrameworkReferences frameworkReferences)
    {
        var solution = _workspace.CurrentSolution;
        _projectId = ProjectId.CreateNew(solution.Id);
        _documentId = DocumentId.CreateNew(_projectId);
        solution = solution
            .AddProject(
                _projectId,
                "Raven.Playground.Session",
                assemblyName: "RavenPlaygroundSession",
                compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication))
            .AddDocument(
                _documentId,
                "main.rav",
                SourceText.From(string.Empty),
                filePath: "main.rav");

        foreach (var reference in frameworkReferences.GetReferences())
            solution = solution.AddMetadataReference(_projectId, reference);

        _workspace.TryApplyChanges(solution);
    }

    public IReadOnlyList<PlaygroundCompletionItem> GetCompletions(string source, int position)
    {
        ArgumentNullException.ThrowIfNull(source);

        lock (_gate)
        {
            UpdateSource(source);
            var compilation = _workspace.GetCompilation(_projectId);
            var syntaxTree = compilation.SyntaxTrees.Single();
            var clampedPosition = Math.Clamp(position, 0, source.Length);

            return compilation
                .GetCompletions(syntaxTree, clampedPosition)
                .GroupBy(item => (item.DisplayText, item.InsertionText, item.ReplacementSpan))
                .Select(group => group.First())
                .Select(item => new PlaygroundCompletionItem(
                    item.DisplayText,
                    item.InsertionText,
                    item.ReplacementSpan.Start,
                    item.ReplacementSpan.Length,
                    item.CursorOffset,
                    item.Description,
                    GetKind(item)))
                .ToArray();
        }
    }

    public PlaygroundCompilationResult Compile(string source)
    {
        ArgumentNullException.ThrowIfNull(source);

        lock (_gate)
        {
            UpdateSource(source);
            var compilation = _workspace.GetCompilation(_projectId);
            using var assemblyStream = new MemoryStream();
            var emitResult = compilation.Emit(assemblyStream);
            var diagnostics = emitResult.Diagnostics
                .Select(static diagnostic => diagnostic.ToString())
                .ToArray();

            return emitResult.Success
                ? new PlaygroundCompilationResult(true, assemblyStream.ToArray(), diagnostics)
                : new PlaygroundCompilationResult(false, null, diagnostics);
        }
    }

    private void UpdateSource(string source)
    {
        var currentDocument = _workspace.CurrentSolution.GetDocument(_documentId)
            ?? throw new InvalidOperationException("The playground document is unavailable.");
        var currentText = currentDocument.GetTextAsync().GetAwaiter().GetResult();
        if (string.Equals(currentText.ToString(), source, StringComparison.Ordinal))
            return;

        var solution = _workspace.CurrentSolution.WithDocumentText(_documentId, SourceText.From(source));
        _workspace.TryApplyChanges(solution);
    }

    private static string GetKind(CompletionItem item)
    {
        if (item.Symbol is null && SyntaxFacts.TryParseKeyword(item.DisplayText, out _))
            return "keyword";

        return item.Symbol switch
        {
            IMethodSymbol { MethodKind: MethodKind.Constructor } => "constructor",
            IMethodSymbol { IsExtensionMethod: true } => "function",
            IMethodSymbol => "method",
            IPropertySymbol => "property",
            IFieldSymbol => "field",
            IEventSymbol => "event",
            ILocalSymbol or IParameterSymbol => "variable",
            INamespaceSymbol => "module",
            ITypeParameterSymbol => "typeParameter",
            INamedTypeSymbol { TypeKind: TypeKind.Interface } => "interface",
            INamedTypeSymbol { TypeKind: TypeKind.Struct } => "struct",
            INamedTypeSymbol { TypeKind: TypeKind.Enum } => "enum",
            INamedTypeSymbol { TypeKind: TypeKind.Delegate } => "interface",
            INamedTypeSymbol => "class",
            _ => "text",
        };
    }
}

public sealed record PlaygroundCompletionItem(
    string Label,
    string InsertText,
    int Start,
    int Length,
    int? CursorOffset,
    string? Detail,
    string Kind);

public sealed record PlaygroundCompilationResult(
    bool Success,
    byte[]? AssemblyImage,
    IReadOnlyList<string> Diagnostics);
