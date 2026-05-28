using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;

using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using RavenWorkspace = Raven.CodeAnalysis.RavenWorkspace;

namespace Raven.Editor.Tests;

public sealed class RavenTextDocumentSyncHandlerTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-sync-{Guid.NewGuid():N}");

    [Fact]
    public void GetRegistrationOptions_UsesIncrementalSync()
    {
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            languageServer: default!,
            logger: NullLogger<RavenTextDocumentSyncHandler>.Instance);

        var method = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "CreateRegistrationOptions",
            BindingFlags.Instance | BindingFlags.NonPublic);

        method.ShouldNotBeNull();

        var options = method!.Invoke(
            handler,
            [new TextSynchronizationCapability(), new ClientCapabilities()]);

        options.ShouldNotBeNull();
        var changeProperty = options!.GetType().GetProperty("Change");
        changeProperty.ShouldNotBeNull();
        changeProperty!.GetValue(options).ShouldBe(TextDocumentSyncKind.Incremental);
    }

    [Theory]
    [InlineData(true, 1, null, null, null)]
    [InlineData(true, 2, 1, null, null)]
    [InlineData(false, 2, 2, null, null)]
    [InlineData(false, 2, 2, 5, 5)]
    [InlineData(true, 2, 2, 5, 4)]
    public void ShouldSkipDiagnosticRequest_UsesSessionBeforeVersion(
        bool expected,
        long expectedSession,
        long? latestSession,
        int? expectedVersion,
        int? latestVersion)
    {
        RavenTextDocumentSyncHandler.ShouldSkipDiagnosticRequest(
            expectedSession,
            latestSession,
            expectedVersion,
            latestVersion).ShouldBe(expected);
    }

    [Fact]
    public void GetSaveDiagnosticsPolicy_UsesSyntaxOnlyThenDeferredAnalyzerDiagnosticsWithoutWarmup()
    {
        var policy = RavenTextDocumentSyncHandler.GetSaveDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.WarmupDelayMilliseconds.ShouldBe(0);
        policy.InitialMode.ShouldBe(DocumentStore.DiagnosticLane.Syntax);
        policy.FollowUpDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.FollowUpDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(policy.DiagnosticsDelayMilliseconds);
        policy.FollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentCompiler);
        policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(policy.FollowUpDiagnosticsDelayMilliseconds.Value);
        policy.AnalyzerFollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentWithAnalyzers);
        policy.DiagnosticsDelayMilliseconds.ShouldBe(0);
    }

    [Fact]
    public void GetOpenDiagnosticsPolicy_UsesDocumentScopedDiagnosticsForInteractiveOpen()
    {
        var policy = RavenTextDocumentSyncHandler.GetOpenDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.WarmupDelayMilliseconds.ShouldBe(0);
        policy.InitialMode.ShouldBe(DocumentStore.DiagnosticLane.Syntax);
        policy.FollowUpDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.FollowUpDiagnosticsDelayMilliseconds.Value.ShouldBeLessThanOrEqualTo(1_500);
        policy.FollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentCompiler);
        policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(policy.FollowUpDiagnosticsDelayMilliseconds.Value);
        policy.AnalyzerFollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentWithAnalyzers);
        policy.DiagnosticsDelayMilliseconds.ShouldBe(0);
    }

    [Fact]
    public void GetEditDiagnosticsPolicy_UsesSyntaxOnlyThenDeferredCompilerDiagnosticsWhileTyping()
    {
        var policy = RavenTextDocumentSyncHandler.GetEditDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.WarmupDelayMilliseconds.ShouldBe(0);
        policy.InitialMode.ShouldBe(DocumentStore.DiagnosticLane.Syntax);
        policy.FollowUpDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.FollowUpDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(0);
        policy.FollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentCompiler);
        policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(policy.FollowUpDiagnosticsDelayMilliseconds.Value);
        policy.AnalyzerFollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentWithAnalyzers);
        policy.DiagnosticsDelayMilliseconds.ShouldBe(0);
    }

    [Fact]
    public void DocumentCommitDebounce_LeavesTimeToTypeBeforeUpdatingWorkspaceSnapshot()
    {
        RavenTextDocumentSyncHandler.DocumentCommitDebounceMilliseconds.ShouldBe(600);
    }

    [Fact]
    public async Task PendingDocumentChange_IsCommittedWhenSemanticContextIsRequestedAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var filePath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(filePath);
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

        await store.UpsertDocumentWithResultAsync(uri, SourceText.From("func Main() -> int => 1"));
        store.QueuePendingDocumentChange(uri, SourceText.From("func Main() -> int => 2"), deferMacroConsumerRefresh: true);

        store.TryGetPendingDocumentText(uri, out var pendingText).ShouldBeTrue();
        pendingText!.ToString().ShouldContain("2");

        store.TryGetDocument(uri, out var document).ShouldBeTrue();
        var textBeforeFlush = await document!.GetTextAsync();
        textBeforeFlush.ToString().ShouldContain("1");

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);

        context.ShouldNotBeNull();
        context.Value.SourceText.ToString().ShouldContain("2");
        store.TryGetPendingDocumentText(uri, out _).ShouldBeFalse();
    }

    [Fact]
    public void ActiveEditorDiagnosticsPolicies_UseSyntaxFirstThenDeferredFollowUp()
    {
        var policies = new[]
        {
            RavenTextDocumentSyncHandler.GetOpenDiagnosticsPolicy(),
            RavenTextDocumentSyncHandler.GetEditDiagnosticsPolicy(),
            RavenTextDocumentSyncHandler.GetSaveDiagnosticsPolicy()
        };

        foreach (var policy in policies)
        {
            policy.InitialMode.ShouldNotBe(DocumentStore.DiagnosticLane.ProjectCompiler);
            policy.InitialMode.ShouldNotBe(DocumentStore.DiagnosticLane.ProjectWithAnalyzers);
            policy.FollowUpMode.ShouldNotBeNull();
            new[]
            {
                DocumentStore.DiagnosticLane.DocumentCompiler,
                DocumentStore.DiagnosticLane.DocumentWithAnalyzers
            }.Contains(policy.FollowUpMode.Value).ShouldBeTrue();
            policy.FollowUpMode.Value.ShouldNotBe(DocumentStore.DiagnosticLane.ProjectWithAnalyzers);
            policy.AnalyzerFollowUpMode.ShouldBe(DocumentStore.DiagnosticLane.DocumentWithAnalyzers);
        }
    }

    [Fact]
    public void ShouldScheduleStructuralDiagnosticsAfterEdit_ReturnsTrueForDeletedImportLine()
    {
        var previousText = SourceText.From("""
import System.*

func Main() -> unit {}
""");

        var changes = new[]
        {
            new TextDocumentContentChangeEvent
            {
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                    new Position(0, 0),
                    new Position(1, 0)),
                Text = string.Empty
            }
        };

        RavenTextDocumentSyncHandler.ShouldScheduleStructuralDiagnosticsAfterEdit(previousText, changes)
            .ShouldBeTrue();
    }

    [Fact]
    public void ShouldScheduleStructuralDiagnosticsAfterEdit_ReturnsTrueForInsertedImportLine()
    {
        var previousText = SourceText.From("""
func Main() -> unit {}
""");

        var changes = new[]
        {
            new TextDocumentContentChangeEvent
            {
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                    new Position(0, 0),
                    new Position(0, 0)),
                Text = "import System.*\n\n"
            }
        };

        RavenTextDocumentSyncHandler.ShouldScheduleStructuralDiagnosticsAfterEdit(previousText, changes)
            .ShouldBeTrue();
    }

    [Fact]
    public void ShouldScheduleStructuralDiagnosticsAfterEdit_ReturnsFalseForOrdinaryBodyEdit()
    {
        var previousText = SourceText.From("""
func Main() -> unit {
    val number = 1
}
""");

        var changes = new[]
        {
            new TextDocumentContentChangeEvent
            {
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                    new Position(1, 17),
                    new Position(1, 18)),
                Text = "2"
            }
        };

        RavenTextDocumentSyncHandler.ShouldScheduleStructuralDiagnosticsAfterEdit(previousText, changes)
            .ShouldBeFalse();
    }

    [Fact]
    public void CreatePublishedDiagnosticValues_UsesValueSemanticsIndependentOfOrder()
    {
        var first = new List<Diagnostic>
        {
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error),
            CreateDiagnostic("RAV0001", "Something else", 0, 0, 0, 1, DiagnosticSeverity.Warning, DiagnosticTag.Unnecessary)
        };

        var second = new List<Diagnostic>
        {
            CreateDiagnostic("RAV0001", "Something else", 0, 0, 0, 1, DiagnosticSeverity.Warning, DiagnosticTag.Unnecessary),
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error)
        };

        RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(first)
            .SequenceEqual(RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(second))
            .ShouldBeTrue();
    }

    [Fact]
    public void CreatePublishedDiagnosticValues_DetectsMeaningfulValueDifference()
    {
        var first = new List<Diagnostic>
        {
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error)
        };

        var second = new List<Diagnostic>
        {
            CreateDiagnostic("RAV0103", "Name not found here", 1, 4, 1, 6, DiagnosticSeverity.Error)
        };

        RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(first)
            .SequenceEqual(RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(second))
            .ShouldBeFalse();
    }

    [Theory]
    [InlineData(true, 2, 2, false)]
    [InlineData(true, 1, 2, true)]
    [InlineData(true, null, 2, true)]
    [InlineData(true, 1, null, false)]
    [InlineData(false, 1, 1, true)]
    public void ShouldPublishDiagnostics_ReissuesUnchangedDiagnosticsForNewDocumentVersion(
        bool hasLastPublishedDiagnostics,
        int? lastPublishedVersion,
        int? currentVersion,
        bool expected)
    {
        var diagnostics = RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(
        [
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error)
        ]);

        RavenTextDocumentSyncHandler.ShouldPublishDiagnostics(
            hasLastPublishedDiagnostics,
            diagnostics,
            lastPublishedVersion,
            diagnostics,
            currentVersion).ShouldBe(expected);
    }

    [Fact]
    public void ShouldPublishDiagnostics_PublishesWhenDiagnosticValuesChange()
    {
        var previous = RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(
        [
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error)
        ]);
        var current = RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(
        [
            CreateDiagnostic("RAV0168", "Shadowing", 1, 4, 1, 6, DiagnosticSeverity.Warning)
        ]);

        RavenTextDocumentSyncHandler.ShouldPublishDiagnostics(
            hasLastPublishedDiagnostics: true,
            previous,
            lastPublishedVersion: 2,
            current,
            currentVersion: 2).ShouldBeTrue();
    }

    [Fact]
    public void MergePartialDiagnosticsForPublish_CarriesForwardAnalyzerDiagnosticsOnly()
    {
        var analyzerDiagnostic = CreateDiagnostic(
            "RAV9030",
            "Parameter 'title' is never used.",
            26,
            32,
            26,
            37,
            DiagnosticSeverity.Warning);
        var externalAnalyzerDiagnostic = CreateDiagnosticWithSource(
            "CSPELL",
            "\"Avalonia\": Unknown word.",
            5,
            7,
            5,
            15,
            "cSpell",
            DiagnosticSeverity.Information);

        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error),
            analyzerDiagnostic,
            externalAnalyzerDiagnostic);

        var currentCompilerDiagnostic = CreateDiagnostic(
            "RAV0030",
            "Invalid invocation expression.",
            10,
            2,
            10,
            5,
            DiagnosticSeverity.Error);

        var merged = RavenTextDocumentSyncHandler.MergePartialDiagnosticsForPublish(
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [currentCompilerDiagnostic],
            previous);

        merged.Select(static diagnostic => diagnostic.Code?.String).ShouldBe([
            "CSPELL",
            "RAV0030",
            "RAV9030"
        ]);
    }

    [Fact]
    public void MergePartialDiagnosticsForPublish_KeepsAnalyzerDiagnosticsVisibleAcrossDocumentVersions()
    {
        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV9030", "Parameter 'title' is never used.", 26, 32, 26, 37, DiagnosticSeverity.Warning),
            CreateDiagnosticWithSource(
                "CSPELL",
                "\"Avalonia\": Unknown word.",
                5,
                7,
                5,
                15,
                "cSpell",
                DiagnosticSeverity.Information));

        var currentCompilerDiagnostic = CreateDiagnostic(
            "RAV0030",
            "Invalid invocation expression.",
            10,
            2,
            10,
            5,
            DiagnosticSeverity.Error);

        var merged = RavenTextDocumentSyncHandler.MergePartialDiagnosticsForPublish(
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [currentCompilerDiagnostic],
            previous);

        merged.Select(static diagnostic => diagnostic.Code?.String).ShouldBe([
            "CSPELL",
            "RAV0030",
            "RAV9030"
        ]);
    }

    [Fact]
    public void MergePartialDiagnosticsForPublish_OnlyCarriesForwardAnalyzerDiagnostics()
    {
        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV0103", "Name not found", 1, 4, 1, 6, DiagnosticSeverity.Error),
            CreateDiagnostic("RAV9030", "Parameter 'title' is never used.", 26, 32, 26, 37, DiagnosticSeverity.Warning));

        var currentCompilerDiagnostic = CreateDiagnostic(
            "RAV0030",
            "Invalid invocation expression.",
            10,
            2,
            10,
            5,
            DiagnosticSeverity.Error);

        var merged = RavenTextDocumentSyncHandler.MergePartialDiagnosticsForPublish(
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [currentCompilerDiagnostic],
            previous);

        merged.Select(static diagnostic => diagnostic.Code?.String).ShouldBe([
            "RAV0030",
            "RAV9030"
        ]);
    }

    [Fact]
    public void MergePartialDiagnosticsForPublish_DoesNotCarryForwardForAnalyzerLane()
    {
        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV9030", "Parameter 'title' is never used.", 26, 32, 26, 37, DiagnosticSeverity.Warning));

        var merged = RavenTextDocumentSyncHandler.MergePartialDiagnosticsForPublish(
            DocumentStore.DiagnosticLane.DocumentWithAnalyzers,
            [],
            previous);

        merged.ShouldBeEmpty();
    }

    [Fact]
    public void SummarizeDiagnosticsForLog_ReturnsNoneForEmpty()
    {
        RavenTextDocumentSyncHandler.SummarizeDiagnosticsForLog([])
            .ShouldBe("none");
    }

    [Fact]
    public void SummarizeDiagnosticsForLog_UsesDeterministicCodeCounts()
    {
        var diagnostics = new List<Diagnostic>
        {
            CreateDiagnostic("RAV0168", "Shadowing", 2, 1, 2, 5, DiagnosticSeverity.Warning),
            CreateDiagnostic("RAV0103", "Missing", 1, 1, 1, 3, DiagnosticSeverity.Error),
            CreateDiagnostic("RAV0168", "Shadowing again", 3, 1, 3, 5, DiagnosticSeverity.Warning),
            CreateDiagnostic("", "No code", 4, 1, 4, 4, DiagnosticSeverity.Information)
        };

        RavenTextDocumentSyncHandler.SummarizeDiagnosticsForLog(diagnostics)
            .ShouldBe("RAV0168x2, <no-code>x1, RAV0103x1");
    }

    [Theory]
    [InlineData(0, 0, "publishProjectWithAnalyzersDiagnostics")]
    [InlineData(0, 1, "publishProjectWithAnalyzersDiagnosticsSkipped")]
    [InlineData(0, 2, "publishProjectWithAnalyzersDiagnosticsUnchanged")]
    [InlineData(0, 3, "publishProjectWithAnalyzersDiagnosticsVersionMismatch")]
    [InlineData(0, 4, "publishProjectWithAnalyzersDiagnosticsAlreadyCompleted")]
    [InlineData(0, 5, "publishProjectWithAnalyzersDiagnosticsAlreadyPending")]
    [InlineData(1, 0, "publishSyntaxDiagnostics")]
    [InlineData(1, 1, "publishSyntaxDiagnosticsSkipped")]
    [InlineData(1, 2, "publishSyntaxDiagnosticsUnchanged")]
    [InlineData(1, 3, "publishSyntaxDiagnosticsVersionMismatch")]
    [InlineData(1, 4, "publishSyntaxDiagnosticsAlreadyCompleted")]
    [InlineData(1, 5, "publishSyntaxDiagnosticsAlreadyPending")]
    [InlineData(2, 0, "publishDocumentCompilerDiagnostics")]
    [InlineData(2, 1, "publishDocumentCompilerDiagnosticsSkipped")]
    [InlineData(2, 2, "publishDocumentCompilerDiagnosticsUnchanged")]
    [InlineData(2, 3, "publishDocumentCompilerDiagnosticsVersionMismatch")]
    [InlineData(2, 4, "publishDocumentCompilerDiagnosticsAlreadyCompleted")]
    [InlineData(2, 5, "publishDocumentCompilerDiagnosticsAlreadyPending")]
    [InlineData(3, 0, "publishProjectCompilerDiagnostics")]
    [InlineData(3, 1, "publishProjectCompilerDiagnosticsSkipped")]
    [InlineData(3, 2, "publishProjectCompilerDiagnosticsUnchanged")]
    [InlineData(3, 3, "publishProjectCompilerDiagnosticsVersionMismatch")]
    [InlineData(3, 4, "publishProjectCompilerDiagnosticsAlreadyCompleted")]
    [InlineData(3, 5, "publishProjectCompilerDiagnosticsAlreadyPending")]
    [InlineData(4, 0, "publishDocumentWithAnalyzersDiagnostics")]
    [InlineData(4, 1, "publishDocumentWithAnalyzersDiagnosticsSkipped")]
    [InlineData(4, 2, "publishDocumentWithAnalyzersDiagnosticsUnchanged")]
    [InlineData(4, 3, "publishDocumentWithAnalyzersDiagnosticsVersionMismatch")]
    [InlineData(4, 4, "publishDocumentWithAnalyzersDiagnosticsAlreadyCompleted")]
    [InlineData(4, 5, "publishDocumentWithAnalyzersDiagnosticsAlreadyPending")]
    public void GetPublishDiagnosticsOperationName_UsesOutcomeSpecificNames(
        int modeValue,
        int outcomeValue,
        string expected)
    {
        var mode = (DocumentStore.DiagnosticLane)modeValue;
        var outcome = (RavenTextDocumentSyncHandler.PublishDiagnosticsOutcome)outcomeValue;

        RavenTextDocumentSyncHandler.GetPublishDiagnosticsOperationName(mode, outcome)
            .ShouldBe(expected);
    }

    private static Diagnostic CreateDiagnostic(
        string code,
        string message,
        int startLine,
        int startCharacter,
        int endLine,
        int endCharacter,
        DiagnosticSeverity severity,
        params DiagnosticTag[] tags)
        => new()
        {
            Code = code,
            Message = message,
            Severity = severity,
            Source = "raven",
            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(startLine, startCharacter),
                new Position(endLine, endCharacter)),
            Tags = tags.Length == 0 ? null : new Container<DiagnosticTag>(tags)
        };

    private static Diagnostic CreateDiagnosticWithSource(
        string code,
        string message,
        int startLine,
        int startCharacter,
        int endLine,
        int endCharacter,
        string source,
        DiagnosticSeverity severity,
        params DiagnosticTag[] tags)
        => new()
        {
            Code = code,
            Message = message,
            Severity = severity,
            Source = source,
            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(startLine, startCharacter),
                new Position(endLine, endCharacter)),
            Tags = tags.Length == 0 ? null : new Container<DiagnosticTag>(tags)
        };

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
