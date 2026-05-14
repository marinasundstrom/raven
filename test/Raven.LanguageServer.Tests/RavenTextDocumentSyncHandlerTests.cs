using System.Collections.Generic;
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
            hoverHandler: new HoverHandler(default!, NullLogger<HoverHandler>.Instance),
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
    [InlineData(true, 2, 1)]
    [InlineData(false, 2, 2)]
    [InlineData(false, 2, null)]
    [InlineData(false, null, 1)]
    public void ShouldClearStaleDiagnostics_WhenPublishedVersionDiffers(bool expected, int? queuedVersion, int? publishedVersion)
    {
        RavenTextDocumentSyncHandler.ShouldClearStaleDiagnostics(queuedVersion, publishedVersion).ShouldBe(expected);
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
    public void GetSaveDiagnosticsPolicy_UsesSyntaxOnlyThenDeferredFullDiagnosticsWithoutWarmup()
    {
        var policy = RavenTextDocumentSyncHandler.GetSaveDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.WarmupDelayMilliseconds.ShouldBe(0);
        policy.InitialMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.SyntaxOnly);
        policy.FullDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.FullDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(policy.DiagnosticsDelayMilliseconds);
        policy.FollowUpMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.Full);
        policy.DiagnosticsDelayMilliseconds.ShouldBe(0);
    }

    [Fact]
    public void GetOpenDiagnosticsPolicy_UsesSyntaxOnlyThenDeferredDocumentDiagnosticsWithoutWarmup()
    {
        var policy = RavenTextDocumentSyncHandler.GetOpenDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.WarmupDelayMilliseconds.ShouldBe(0);
        policy.InitialMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.SyntaxOnly);
        policy.FullDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.FullDiagnosticsDelayMilliseconds.Value.ShouldBeLessThanOrEqualTo(750);
        policy.FollowUpMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.Document);
        policy.DiagnosticsDelayMilliseconds.ShouldBe(0);
    }

    [Fact]
    public void GetEditDiagnosticsPolicy_UsesDocumentDiagnosticsWhileTyping()
    {
        var policy = RavenTextDocumentSyncHandler.GetEditDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.WarmupDelayMilliseconds.ShouldBe(0);
        policy.InitialMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.Document);
        policy.FullDiagnosticsDelayMilliseconds.ShouldBeNull();
        policy.FollowUpMode.ShouldBeNull();
        policy.DiagnosticsDelayMilliseconds.ShouldBeGreaterThan(0);
    }

    [Fact]
    public void ShouldScheduleFullDiagnosticsAfterEdit_ReturnsTrueForDeletedImportLine()
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

        RavenTextDocumentSyncHandler.ShouldScheduleFullDiagnosticsAfterEdit(previousText, changes)
            .ShouldBeTrue();
    }

    [Fact]
    public void ShouldScheduleFullDiagnosticsAfterEdit_ReturnsTrueForInsertedImportLine()
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

        RavenTextDocumentSyncHandler.ShouldScheduleFullDiagnosticsAfterEdit(previousText, changes)
            .ShouldBeTrue();
    }

    [Fact]
    public void ShouldScheduleFullDiagnosticsAfterEdit_ReturnsFalseForOrdinaryBodyEdit()
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

        RavenTextDocumentSyncHandler.ShouldScheduleFullDiagnosticsAfterEdit(previousText, changes)
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
    [InlineData(0, 0, "publishDiagnostics")]
    [InlineData(0, 1, "publishDiagnosticsSkipped")]
    [InlineData(0, 2, "publishDiagnosticsUnchanged")]
    [InlineData(0, 3, "publishDiagnosticsVersionMismatch")]
    [InlineData(0, 4, "publishDiagnosticsAlreadyCompleted")]
    [InlineData(1, 0, "publishSyntaxDiagnostics")]
    [InlineData(1, 1, "publishSyntaxDiagnosticsSkipped")]
    [InlineData(1, 2, "publishSyntaxDiagnosticsUnchanged")]
    [InlineData(1, 3, "publishSyntaxDiagnosticsVersionMismatch")]
    [InlineData(1, 4, "publishSyntaxDiagnosticsAlreadyCompleted")]
    [InlineData(2, 0, "publishDocumentDiagnostics")]
    [InlineData(2, 1, "publishDocumentDiagnosticsSkipped")]
    [InlineData(2, 2, "publishDocumentDiagnosticsUnchanged")]
    [InlineData(2, 3, "publishDocumentDiagnosticsVersionMismatch")]
    [InlineData(2, 4, "publishDocumentDiagnosticsAlreadyCompleted")]
    public void GetPublishDiagnosticsOperationName_UsesOutcomeSpecificNames(
        int modeValue,
        int outcomeValue,
        string expected)
    {
        var mode = (DocumentStore.DocumentDiagnosticsMode)modeValue;
        var outcome = (RavenTextDocumentSyncHandler.PublishDiagnosticsOutcome)outcomeValue;

        RavenTextDocumentSyncHandler.GetPublishDiagnosticsOperationName(mode, outcome)
            .ShouldBe(expected);
    }

    [Fact]
    public async Task DidChange_InvalidatesHoverCacheForUpdatedDocumentAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "main.rvn"));
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
        var hoverHandler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var syncHandler = new RavenTextDocumentSyncHandler(
            store,
            hoverHandler,
            languageServer: default!,
            logger: NullLogger<RavenTextDocumentSyncHandler>.Instance);

        await syncHandler.Handle(new DidOpenTextDocumentParams
        {
            TextDocument = new TextDocumentItem
            {
                Uri = uri,
                LanguageId = "raven",
                Version = 1,
                Text = """
import System.Console.*

func Main() -> unit {
    val number = 42
    WriteLine(number)
}
"""
            }
        }, CancellationToken.None);

        var hover = await hoverHandler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        GetHoverCacheCount(hoverHandler).ShouldBe(1);

        await syncHandler.Handle(new DidChangeTextDocumentParams
        {
            TextDocument = new OptionalVersionedTextDocumentIdentifier
            {
                Uri = uri,
                Version = 2
            },
            ContentChanges = new Container<TextDocumentContentChangeEvent>(new TextDocumentContentChangeEvent
            {
                Text = """
import System.Console.*

func Main() -> unit {
    val value = 42
    WriteLine(value)
}
"""
            })
        }, CancellationToken.None);

        GetHoverCacheCount(hoverHandler).ShouldBe(0);
    }

    [Fact]
    public async Task DidClose_InvalidatesHoverCacheForClosedDocumentAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "main.rvn"));
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
        var hoverHandler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var syncHandler = new RavenTextDocumentSyncHandler(
            store,
            hoverHandler,
            languageServer: default!,
            logger: NullLogger<RavenTextDocumentSyncHandler>.Instance);

        await syncHandler.Handle(new DidOpenTextDocumentParams
        {
            TextDocument = new TextDocumentItem
            {
                Uri = uri,
                LanguageId = "raven",
                Version = 1,
                Text = """
import System.Console.*

func Main() -> unit {
    val number = 42
    WriteLine(number)
}
"""
            }
        }, CancellationToken.None);

        var hover = await hoverHandler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        GetHoverCacheCount(hoverHandler).ShouldBe(1);

        await syncHandler.Handle(new DidCloseTextDocumentParams
        {
            TextDocument = new TextDocumentIdentifier(uri)
        }, CancellationToken.None);

        GetHoverCacheCount(hoverHandler).ShouldBe(0);
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

    private static int GetHoverCacheCount(HoverHandler handler)
    {
        var cacheField = typeof(HoverHandler).GetField("_hoverCache", BindingFlags.Instance | BindingFlags.NonPublic);
        cacheField.ShouldNotBeNull();
        var cache = cacheField!.GetValue(handler);
        cache.ShouldNotBeNull();
        return (int)cache.GetType().GetProperty("Count")!.GetValue(cache)!;
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
