using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Raven.Editor.Tests;

public sealed class LanguageServerInlayHintTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-inlay-hints-{Guid.NewGuid():N}");

    [Fact]
    public async Task Handle_InferredLocalsAndReturns_ProvidesSourceApplicableTypeHintsAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Add(left: int, right: int) {
    return left + right
}

func Main() -> unit {
    val answer = 1 + 2
    val explicit: int = 3
    val name = "Raven"
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var hints = result.ToArray();
        hints.Select(static hint => hint.Label.String).ShouldContain(" -> int");
        hints.Select(static hint => hint.Label.String).ShouldContain(": int");
        hints.Select(static hint => hint.Label.String).ShouldContain(": string");
        hints.Count(static hint => hint.Label.String == ": int").ShouldBe(1);

        var returnHint = hints.Single(static hint => hint.Label.String == " -> int");
        AssertSourceApplicable(sourceText, returnHint, code.IndexOf(") {", StringComparison.Ordinal) + 1, " -> int");

        var answerHint = hints.Single(static hint => hint.Label.String == ": int");
        AssertSourceApplicable(sourceText, answerHint, code.IndexOf("answer", StringComparison.Ordinal) + "answer".Length, ": int");

        var stringHint = hints.Single(static hint => hint.Label.String == ": string");
        AssertSourceApplicable(sourceText, stringHint, code.IndexOf("name", StringComparison.Ordinal) + "name".Length, ": string");
    }

    [Fact]
    public async Task Handle_RequestedRange_FiltersHintsAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Main() -> unit {
    val first = 1
    val second = "two"
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);
        var secondLineStart = code.IndexOf("    val second", StringComparison.Ordinal);
        secondLineStart.ShouldBeGreaterThan(0);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new TextSpan(secondLineStart, code.Length - secondLineStart))
        }, CancellationToken.None);

        var hints = result.ToArray();
        hints.Length.ShouldBe(1);
        hints[0].Label.String.ShouldBe(": string");
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }

    private static LspRange FullDocumentRange(SourceText sourceText)
        => PositionHelper.ToRange(sourceText, new TextSpan(0, sourceText.Length));

    private static void AssertSourceApplicable(SourceText sourceText, InlayHint hint, int insertionPosition, string expectedText)
    {
        hint.Kind.ShouldBe(InlayHintKind.Type);
        hint.TextEdits.ShouldNotBeNull();

        var edit = hint.TextEdits.Single();
        edit.NewText.ShouldBe(expectedText);

        var expectedRange = PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0));
        edit.Range.Start.Line.ShouldBe(expectedRange.Start.Line);
        edit.Range.Start.Character.ShouldBe(expectedRange.Start.Character);
        edit.Range.End.Line.ShouldBe(expectedRange.End.Line);
        edit.Range.End.Character.ShouldBe(expectedRange.End.Character);
    }
}
