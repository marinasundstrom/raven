using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.LanguageServer;

using RavenWorkspace = Raven.CodeAnalysis.RavenWorkspace;
using SourceText = Raven.CodeAnalysis.Text.SourceText;

namespace Raven.Editor.Tests;

public sealed class LanguageServerDispatcherTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-dispatcher-{Guid.NewGuid():N}");

    [Fact]
    public async Task IsCurrent_CrossFileProjectEditInvalidatesUnchangedDocumentSnapshotAsync()
    {
        var (store, mainUri, utilitiesUri) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);
        var initialContext = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        initialContext.ShouldNotBeNull();
        var initialSnapshot = dispatcher.CreateSnapshot(mainUri, initialContext.Value.Document, documentSession: 1);

        await store.UpsertDocumentAsync(utilitiesUri, SourceText.From("""
namespace Utilities

func Test2() -> IDisposable {
    return default!
}
"""));

        dispatcher.IsCurrent(initialSnapshot).ShouldBeFalse();

        store.TryGetDocument(mainUri, out var currentMainDocument).ShouldBeTrue();
        var currentSnapshot = dispatcher.CreateSnapshot(mainUri, currentMainDocument!, documentSession: 1);
        dispatcher.IsCurrent(currentSnapshot).ShouldBeTrue();
    }

    [Fact]
    public async Task ShouldAcceptDiagnosticsResult_RejectsPreviousProjectSnapshotAsync()
    {
        var (store, mainUri, utilitiesUri) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);
        var initialContext = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        initialContext.ShouldNotBeNull();
        var staleDiagnosticsSnapshot = new DocumentStore.DiagnosticSnapshotKey(
            mainUri.ToString(),
            initialContext.Value.Document.Project.Id,
            initialContext.Value.Document.Version,
            initialContext.Value.Document.Project.Version);

        await store.UpsertDocumentAsync(utilitiesUri, SourceText.From("""
namespace Utilities

func Test2() -> IDisposable {
    return default!
}
"""));

        dispatcher.ShouldAcceptDiagnosticsResult(
                mainUri,
                staleDiagnosticsSnapshot,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                reason: "test",
                editorVersion: 1)
            .ShouldBeFalse();
    }

    [Fact]
    public async Task RecordWorkspaceEvent_IncrementsEpochAndCapturesSnapshotAsync()
    {
        var (store, mainUri, _) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);

        var first = dispatcher.RecordWorkspaceEvent(
            "DocumentOpened",
            mainUri,
            reason: "test",
            editorVersion: 1,
            documentSession: 1);
        var second = dispatcher.RecordWorkspaceEvent(
            "DocumentChanged",
            mainUri,
            reason: "test",
            editorVersion: 2,
            documentSession: 1);

        second.ShouldBe(first + 1);
        dispatcher.TryCaptureSnapshot(mainUri, documentSession: 1, out var snapshot).ShouldBeTrue();
        snapshot.WorkspaceEpoch.ShouldBe(second);
        snapshot.DocumentSession.ShouldBe(1);
    }

    [Fact]
    public async Task GetRecentEvents_ReturnsDispatcherEventsInSequenceOrderAsync()
    {
        var (store, mainUri, _) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);

        dispatcher.RecordWorkspaceEvent(
            "DocumentOpened",
            mainUri,
            reason: "test",
            editorVersion: 1,
            documentSession: 1);
        dispatcher.RecordWorkItemEvent(
            "InlayRefreshRequested",
            mainUri,
            reason: "test");

        var events = dispatcher.GetRecentEvents();

        events.Length.ShouldBe(2);
        events[0].Sequence.ShouldBe(1);
        events[0].Operation.ShouldBe("dispatcherWorkspaceEvent");
        events[0].EventName.ShouldBe("DocumentOpened");
        events[0].Snapshot.ShouldNotBeNull();
        events[1].Sequence.ShouldBe(2);
        events[1].Operation.ShouldBe("dispatcherWorkItemEvent");
        events[1].EventName.ShouldBe("InlayRefreshRequested");
        events[1].WorkspaceEpoch.ShouldBe(events[0].WorkspaceEpoch);
    }

    [Fact]
    public async Task ShouldAcceptDiagnosticsResult_RecordsDiscardDecisionInEventStreamAsync()
    {
        var (store, mainUri, utilitiesUri) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);
        var initialContext = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        initialContext.ShouldNotBeNull();
        var staleDiagnosticsSnapshot = new DocumentStore.DiagnosticSnapshotKey(
            mainUri.ToString(),
            initialContext.Value.Document.Project.Id,
            initialContext.Value.Document.Version,
            initialContext.Value.Document.Project.Version);

        await store.UpsertDocumentAsync(utilitiesUri, SourceText.From("""
namespace Utilities

func Test2() -> IDisposable {
    return default!
}
"""));

        dispatcher.ShouldAcceptDiagnosticsResult(
                mainUri,
                staleDiagnosticsSnapshot,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                reason: "test",
                editorVersion: 1)
            .ShouldBeFalse();

        var dispatcherEvent = dispatcher.GetRecentEvents().ShouldHaveSingleItem();
        dispatcherEvent.Sequence.ShouldBe(1);
        dispatcherEvent.EventName.ShouldBe("DiagnosticsResultDiscarded");
        dispatcherEvent.Detail.ShouldContain("outcome=DiscardedStale");
        dispatcherEvent.Detail.ShouldContain("lane=DocumentCompiler");
    }

    [Fact]
    public async Task AcceptDiagnosticsForPublish_TracksPresentationStateInEventStreamAsync()
    {
        var (store, mainUri, _) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);
        var context = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        context.ShouldNotBeNull();
        var snapshot = new DocumentStore.DiagnosticSnapshotKey(
            mainUri.ToString(),
            context.Value.Document.Project.Id,
            context.Value.Document.Version,
            context.Value.Document.Project.Version);
        var diagnostic = new Diagnostic
        {
            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(new Position(0, 0), new Position(0, 4)),
            Message = "Test diagnostic",
            Code = "RAV9999",
            Source = "raven",
            Severity = DiagnosticSeverity.Warning
        };

        var first = dispatcher.AcceptDiagnosticsForPublish(
            mainUri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [diagnostic],
            editorVersion: 1,
            snapshot);
        var second = dispatcher.AcceptDiagnosticsForPublish(
            mainUri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [diagnostic],
            editorVersion: 1,
            snapshot);

        first.ShouldPublish.ShouldBeTrue();
        second.ShouldPublish.ShouldBeFalse();
        first.Diagnostics.ShouldHaveSingleItem().Message.ShouldBe("Test diagnostic");
        second.Diagnostics.ShouldHaveSingleItem().Message.ShouldBe("Test diagnostic");
        var events = dispatcher.GetRecentEvents();
        events.Select(static dispatcherEvent => dispatcherEvent.EventName)
            .ShouldBe(["DiagnosticsPresentationChanged", "DiagnosticsPresentationUnchanged"]);
        events[0].Detail.ShouldContain("outcome=Changed");
        events[1].Detail.ShouldContain("outcome=Unchanged");
    }

    [Fact]
    public async Task AcceptDiagnosticsForPublish_CompilerLaneCarriesForwardPreviousAnalyzerDiagnosticsAsync()
    {
        var (store, mainUri, _) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);
        var context = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        context.ShouldNotBeNull();
        var snapshot = new DocumentStore.DiagnosticSnapshotKey(
            mainUri.ToString(),
            context.Value.Document.Project.Id,
            context.Value.Document.Version,
            context.Value.Document.Project.Version);
        var analyzerDiagnostic = CreateDiagnostic(
            "RAV9030",
            "Parameter 'name' is never used.",
            5,
            12,
            5,
            16,
            DiagnosticSeverity.Warning);
        var compilerDiagnostic = CreateDiagnostic(
            "RAV0103",
            "'Missing' is not in scope.",
            3,
            4,
            3,
            11,
            DiagnosticSeverity.Error);

        var analyzerPublish = dispatcher.AcceptDiagnosticsForPublish(
            mainUri,
            DocumentStore.DiagnosticLane.DocumentWithAnalyzers,
            [analyzerDiagnostic],
            editorVersion: 1,
            snapshot);
        var compilerPublish = dispatcher.AcceptDiagnosticsForPublish(
            mainUri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [compilerDiagnostic],
            editorVersion: 1,
            snapshot);
        var repeatedCompilerPublish = dispatcher.AcceptDiagnosticsForPublish(
            mainUri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [compilerDiagnostic],
            editorVersion: 1,
            snapshot);

        analyzerPublish.ShouldPublish.ShouldBeTrue();
        analyzerPublish.Diagnostics.Select(static diagnostic => diagnostic.Code?.String)
            .ShouldBe(["RAV9030"]);
        compilerPublish.ShouldPublish.ShouldBeTrue();
        compilerPublish.Diagnostics.Select(static diagnostic => diagnostic.Code?.String)
            .ShouldBe(["RAV0103", "RAV9030"]);
        repeatedCompilerPublish.ShouldPublish.ShouldBeFalse();
        repeatedCompilerPublish.Diagnostics.Select(static diagnostic => diagnostic.Code?.String)
            .ShouldBe(["RAV0103", "RAV9030"]);
    }

    [Fact]
    public async Task CacheInlayHints_RecordsExactHitAndRejectsCrossProjectStaleHintAsync()
    {
        var (store, mainUri, utilitiesUri) = await CreateTwoFileProjectAsync();
        var dispatcher = new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance);
        store.TryGetDocument(mainUri, out var mainDocument).ShouldBeTrue();
        mainDocument.ShouldNotBeNull();
        var sourceText = await mainDocument.GetTextAsync(CancellationToken.None);
        var request = new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(mainUri),
            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(0, 0),
                new Position(sourceText.GetLineCount(), 0))
        };
        var hint = new InlayHint
        {
            Position = new Position(3, 12),
            Label = new StringOrInlayHintLabelParts(": IDisposable?")
        };

        dispatcher.CacheInlayHints(request, mainDocument, sourceText, [hint]);
        dispatcher.TryGetCachedInlayHints(request, sourceText, out var cachedHints).ShouldBeTrue();
        cachedHints.ShouldHaveSingleItem().Label.String.ShouldBe(": IDisposable?");

        await store.UpsertDocumentAsync(utilitiesUri, SourceText.From("""
namespace Utilities

func Test2() -> IDisposable {
    return default!
}
"""));

        dispatcher.TryGetCachedInlayHints(request, sourceText, out _).ShouldBeFalse();
        var events = dispatcher.GetRecentEvents();
        events.Select(static dispatcherEvent => dispatcherEvent.EventName)
            .ShouldBe([
                "InlayPresentationCached",
                "InlayPresentationCacheHit",
                "InlayPresentationCacheMiss"
            ]);
        events[0].Detail.ShouldContain("outcome=Cached");
        events[1].Detail.ShouldContain("outcome=ExactSnapshot");
        events[2].Detail.ShouldContain("outcome=NoReusableSnapshot");
    }

    private async Task<(DocumentStore Store, DocumentUri MainUri, DocumentUri UtilitiesUri)> CreateTwoFileProjectAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var sourceRoot = Path.Combine(_tempRoot, "src");
        Directory.CreateDirectory(sourceRoot);
        await File.WriteAllTextAsync(Path.Combine(_tempRoot, "App.rvnproj"), """
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <TargetFramework>net10.0</TargetFramework>
              </PropertyGroup>
              <ItemGroup>
                <RavenCompile Include="src/**/*.rvn" />
              </ItemGroup>
            </Project>
            """);

        var mainPath = Path.Combine(sourceRoot, "main.rvn");
        var utilitiesPath = Path.Combine(sourceRoot, "test.rvn");
        const string mainCode = """
import Utilities.*

func Main() -> unit {
    val test = Test2()
}
""";
        const string utilitiesCode = """
namespace Utilities

func Test2() -> IDisposable? {
    return null
}
""";

        await File.WriteAllTextAsync(mainPath, mainCode);
        await File.WriteAllTextAsync(utilitiesPath, utilitiesCode);

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
        var mainUri = DocumentUri.FromFileSystemPath(mainPath);
        var utilitiesUri = DocumentUri.FromFileSystemPath(utilitiesPath);
        await store.UpsertDocumentAsync(mainUri, mainCode);
        await store.UpsertDocumentAsync(utilitiesUri, utilitiesCode);
        return (store, mainUri, utilitiesUri);
    }

    private static Diagnostic CreateDiagnostic(
        string code,
        string message,
        int startLine,
        int startCharacter,
        int endLine,
        int endCharacter,
        DiagnosticSeverity severity)
        => new()
        {
            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(startLine, startCharacter),
                new Position(endLine, endCharacter)),
            Message = message,
            Code = code,
            Source = "raven",
            Severity = severity
        };

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
