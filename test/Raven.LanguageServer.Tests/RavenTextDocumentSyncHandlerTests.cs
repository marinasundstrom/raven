using System.Collections.Concurrent;
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

using ProjectId = Raven.CodeAnalysis.ProjectId;
using RavenWorkspace = Raven.CodeAnalysis.RavenWorkspace;
using SolutionId = Raven.CodeAnalysis.SolutionId;
using VersionStamp = Raven.CodeAnalysis.VersionStamp;

namespace Raven.LanguageServer.Tests;

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
        RavenTextDocumentSyncHandler.DocumentCommitDebounceMilliseconds.ShouldBe(1_200);
    }

    [Fact]
    public void PendingSyntaxDiagnosticsDebounce_LeavesTimeToCompleteCommentToken()
    {
        RavenTextDocumentSyncHandler.PendingSyntaxDiagnosticsDebounceMilliseconds.ShouldBe(250);
        RavenTextDocumentSyncHandler.PendingSyntaxDiagnosticsDebounceMilliseconds
            .ShouldBeLessThan(RavenTextDocumentSyncHandler.DocumentCommitDebounceMilliseconds);
    }

    [Fact]
    public void RelatedDocumentCompilerDiagnosticsAfterEdit_UsesInteractiveDelay()
    {
        RavenTextDocumentSyncHandler.RelatedDocumentCompilerDiagnosticsAfterEditDelayMilliseconds
            .ShouldBeLessThan(RavenTextDocumentSyncHandler.RelatedDocumentCompilerDiagnosticsAfterOpenDelayMilliseconds);
    }

    [Fact]
    public void HasAdvertisedInlayHintRefreshSupport_ReadsClientRefreshSupport()
    {
        RavenTextDocumentSyncHandler.HasAdvertisedInlayHintRefreshSupport(null).ShouldBeFalse();
        RavenTextDocumentSyncHandler.HasAdvertisedInlayHintRefreshSupport(new ClientCapabilities()).ShouldBeFalse();
        RavenTextDocumentSyncHandler.HasAdvertisedInlayHintRefreshSupport(new ClientCapabilities
        {
            Workspace = new WorkspaceClientCapabilities
            {
                InlayHint = new InlayHintWorkspaceClientCapabilities
                {
                    RefreshSupport = true
                }
            }
        }).ShouldBeTrue();
    }

    [Fact]
    public async Task RelatedDocumentCompilerDiagnosticsAfterEdit_ReplacesExistingPendingPublishAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var sourceRoot = Path.Combine(_tempRoot, "src");
        Directory.CreateDirectory(sourceRoot);
        var projectPath = Path.Combine(_tempRoot, "App.rvnproj");
        await File.WriteAllTextAsync(projectPath, """
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
        var testPath = Path.Combine(sourceRoot, "test.rvn");
        await File.WriteAllTextAsync(mainPath, """
            import Utilities.*

            func Main() -> unit {
                use test = Test2()
            }
            """);
        await File.WriteAllTextAsync(testPath, """
            namespace Utilities

            func Test2() -> IDisposable? {
                return null
            }
            """);

        var mainUri = DocumentUri.FromFileSystemPath(mainPath);
        var testUri = DocumentUri.FromFileSystemPath(testPath);
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
        _ = await store.UpsertDocumentWithResultAsync(mainUri, SourceText.From(await File.ReadAllTextAsync(mainPath)));
        _ = await store.UpsertDocumentWithResultAsync(testUri, SourceText.From(await File.ReadAllTextAsync(testPath)));
        var handler = new RavenTextDocumentSyncHandler(
            store,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);

        GetDocumentVersions(handler)[mainUri] = 1;
        var scheduleRelatedDiagnostics = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "ScheduleRelatedOpenDocumentCompilerDiagnostics",
            BindingFlags.Instance | BindingFlags.NonPublic);
        scheduleRelatedDiagnostics.ShouldNotBeNull();

        scheduleRelatedDiagnostics!.Invoke(handler, [testUri, 60_000, false, "test"]);
        var pendingDiagnostics = GetPendingDiagnostics(handler);
        pendingDiagnostics.TryGetValue(mainUri, out var first).ShouldBeTrue();
        first!.Id.ShouldBe(1);
        first.Reason.ShouldBe("test");
        first.ExpectedSession.ShouldBe(1);
        first.ExpectedVersion.ShouldBe(1);

        scheduleRelatedDiagnostics.Invoke(handler, [testUri, 60_000, true, "test"]);
        pendingDiagnostics.TryGetValue(mainUri, out var second).ShouldBeTrue();

        second.ShouldNotBeSameAs(first);
        second!.Id.ShouldBe(2);
        second.Reason.ShouldBe("test");
        second.ExpectedSession.ShouldBe(1);
        second.ExpectedVersion.ShouldBe(1);
        first.Cancellation.IsCancellationRequested.ShouldBeTrue();
        second!.Cancellation.Cancel();
    }

    [Fact]
    public async Task RelatedDocumentCompilerDiagnosticsAfterEdit_WhenEditWasAlreadyFlushed_StillSchedulesRelatedDiagnosticsAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var sourceRoot = Path.Combine(_tempRoot, "src");
        Directory.CreateDirectory(sourceRoot);
        var projectPath = Path.Combine(_tempRoot, "App.rvnproj");
        await File.WriteAllTextAsync(projectPath, """
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
        var testPath = Path.Combine(sourceRoot, "test.rvn");
        await File.WriteAllTextAsync(mainPath, """
            import Utilities.*

            func Main() -> unit {
                use test = Test2()
            }
            """);
        await File.WriteAllTextAsync(testPath, """
            namespace Utilities

            func Test2() -> IDisposable? {
                return null
            }
            """);

        var mainUri = DocumentUri.FromFileSystemPath(mainPath);
        var testUri = DocumentUri.FromFileSystemPath(testPath);
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
        _ = await store.UpsertDocumentWithResultAsync(mainUri, SourceText.From(await File.ReadAllTextAsync(mainPath)));
        _ = await store.UpsertDocumentWithResultAsync(testUri, SourceText.From(await File.ReadAllTextAsync(testPath)));

        var updatedTestText = SourceText.From("""
            namespace Utilities

            func Test2() -> IDisposable {
                return default!
            }
            """);
        store.QueuePendingDocumentChange(testUri, updatedTestText, deferMacroConsumerRefresh: true);
        var context = await store.GetAnalysisContextAsync(testUri, CancellationToken.None);
        context.ShouldNotBeNull();
        store.TryGetPendingDocumentText(testUri, out _).ShouldBeFalse();

        var handler = new RavenTextDocumentSyncHandler(
            store,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);

        GetDocumentSessions(handler)[mainUri] = 1;
        GetDocumentSessions(handler)[testUri] = 1;
        GetDocumentVersions(handler)[mainUri] = 1;
        GetDocumentVersions(handler)[testUri] = 2;

        var commitPendingChange = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "CommitPendingDocumentChangeAndScheduleDiagnosticsAsync",
            BindingFlags.Instance | BindingFlags.NonPublic);
        commitPendingChange.ShouldNotBeNull();

        var task = (Task)commitPendingChange!.Invoke(
            handler,
            [testUri, 1L, 2, false, CancellationToken.None])!;
        await task;

        var pendingDiagnostics = GetPendingDiagnostics(handler);
        pendingDiagnostics.TryGetValue(mainUri, out var relatedRequest).ShouldBeTrue();
        relatedRequest!.Reason.ShouldBe("relatedProjectEdit");
        relatedRequest.ExpectedSession.ShouldBe(1);
        relatedRequest.ExpectedVersion.ShouldBe(1);

        foreach (var request in pendingDiagnostics.Values)
            request.Cancellation.Cancel();
    }

    [Fact]
    public async Task PublishDiagnostics_WhenSameVersionLaneIsActive_RequeuesInsteadOfDroppingAsync()
    {
        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "main.rvn"));
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        GetDocumentSessions(handler)[uri] = 1;
        GetDocumentVersions(handler)[uri] = 1;

        var tryStartDiagnosticsPublish = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "TryStartDiagnosticsPublish",
            BindingFlags.Instance | BindingFlags.NonPublic);
        tryStartDiagnosticsPublish.ShouldNotBeNull();
        var publishDiagnostics = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "PublishDiagnosticsAsync",
            BindingFlags.Instance | BindingFlags.NonPublic);
        publishDiagnostics.ShouldNotBeNull();

        ((bool)tryStartDiagnosticsPublish!.Invoke(
            handler,
            [uri, 1, DocumentStore.DiagnosticLane.DocumentCompiler])!).ShouldBeTrue();

        var publishTask = (Task)publishDiagnostics!.Invoke(
            handler,
            [uri, CancellationToken.None, 1L, 1, DocumentStore.DiagnosticLane.DocumentCompiler, 42L])!;
        await publishTask;

        GetPendingDiagnosticsRetryCount(handler).ShouldBe(1);
    }

    [Fact]
    public async Task PublishDiagnostics_ActiveRetryKeepsRetryPendingWhenPublishIsStillActiveAsync()
    {
        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "main.rvn"));
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        GetDocumentSessions(handler)[uri] = 1;
        GetDocumentVersions(handler)[uri] = 1;

        var tryStartDiagnosticsPublish = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "TryStartDiagnosticsPublish",
            BindingFlags.Instance | BindingFlags.NonPublic);
        tryStartDiagnosticsPublish.ShouldNotBeNull();
        var requeueDiagnosticsPublish = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "RequeueDiagnosticsPublish",
            BindingFlags.Instance | BindingFlags.NonPublic);
        requeueDiagnosticsPublish.ShouldNotBeNull();

        ((bool)tryStartDiagnosticsPublish!.Invoke(
            handler,
            [uri, 1, DocumentStore.DiagnosticLane.DocumentCompiler])!).ShouldBeTrue();

        requeueDiagnosticsPublish!.Invoke(
            handler,
            [uri, 1L, 1, DocumentStore.DiagnosticLane.DocumentCompiler, 1, true, 42L]);
        await Task.Delay(100);

        GetPendingDiagnosticsRetryCount(handler).ShouldBe(1);
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
    public async Task DidChange_ValToUseInAspNetMinimalApiSample_KeepsAppInScopeAsync()
    {
        var sampleRoot = Path.Combine(
            GetRepositoryRoot(),
            "samples",
            "projects",
            "aspnet-minimal-api");
        var documentPath = Path.Combine(sampleRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);

        Directory.Exists(sampleRoot).ShouldBeTrue();
        File.Exists(documentPath).ShouldBeTrue();

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "aspnet-minimal-api",
                Uri = DocumentUri.FromFileSystemPath(sampleRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new RavenTextDocumentSyncHandler(
            store,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        var savedCode = await File.ReadAllTextAsync(documentPath);
        var initialCode = savedCode.Replace("use app = builder.Build()", "val app = builder.Build()", StringComparison.Ordinal);
        initialCode.ShouldNotBe(savedCode);

        _ = await store.UpsertDocumentWithResultAsync(uri, SourceText.From(initialCode));
        GetDocumentSessions(handler)[uri] = 1;
        GetDocumentVersions(handler)[uri] = 1;

        await handler.Handle(new DidChangeTextDocumentParams
        {
            TextDocument = new OptionalVersionedTextDocumentIdentifier
            {
                Uri = uri,
                Version = 2
            },
            ContentChanges = new Container<TextDocumentContentChangeEvent>(
                new TextDocumentContentChangeEvent
                {
                    Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                        new Position(12, 0),
                        new Position(12, 3)),
                    Text = "use"
                })
        }, CancellationToken.None);

        store.TryGetPendingDocumentText(uri, out var pendingText).ShouldBeTrue();
        pendingText!.ToString().ShouldContain("use app = builder.Build()");

        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        diagnostics.Any(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                diagnostic.Message.Contains("app", StringComparison.Ordinal))
            .ShouldBeFalse();
        store.TryGetPendingDocumentText(uri, out _).ShouldBeFalse();
    }

    [Fact]
    public async Task DidChange_TypingLineComment_DoesNotPublishSingleSlashSyntaxDiagnosticsAsync()
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
        var published = new List<PublishDiagnosticsParams>();
        var handler = new RavenTextDocumentSyncHandler(
            store,
            new LanguageServerDispatcher(store, NullLogger<LanguageServerDispatcher>.Instance),
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance,
            publishDiagnosticsOverride: published.Add);

        await store.UpsertDocumentWithResultAsync(uri, SourceText.From("""
            [Obsolete("Test")]
            func A(x: string) -> int {
                42
            }
            """));
        GetDocumentSessions(handler)[uri] = 1;
        GetDocumentVersions(handler)[uri] = 1;

        await handler.Handle(new DidChangeTextDocumentParams
        {
            TextDocument = new OptionalVersionedTextDocumentIdentifier
            {
                Uri = uri,
                Version = 2
            },
            ContentChanges = new Container<TextDocumentContentChangeEvent>(
                new TextDocumentContentChangeEvent
                {
                    Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                        new Position(0, 0),
                        new Position(0, 0)),
                    Text = "/"
                })
        }, CancellationToken.None);

        await handler.Handle(new DidChangeTextDocumentParams
        {
            TextDocument = new OptionalVersionedTextDocumentIdentifier
            {
                Uri = uri,
                Version = 3
            },
            ContentChanges = new Container<TextDocumentContentChangeEvent>(
                new TextDocumentContentChangeEvent
                {
                    Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                        new Position(0, 1),
                        new Position(0, 1)),
                    Text = "/"
                })
        }, CancellationToken.None);

        await Task.Delay(RavenTextDocumentSyncHandler.PendingSyntaxDiagnosticsDebounceMilliseconds + 150);

        published.Any(notification => notification.Version == 2).ShouldBeFalse();
        published.ShouldNotBeEmpty();
        var latest = published.Last();
        latest.Version.ShouldBe(3);
        latest.Diagnostics.ShouldNotContain(diagnostic =>
            diagnostic.Message.Contains("Unexpected", StringComparison.OrdinalIgnoreCase) &&
            diagnostic.Message.Contains("/", StringComparison.Ordinal));
    }

    [Fact]
    public void AcceptPendingSyntaxDiagnosticsForPublish_PublishesEmptySetWhenPendingTextIsFixed()
    {
        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "main.rvn"));
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);

        var broken = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            SourceText.From("""
                func Main() -> unit {
                    val value =
                }
                """),
            version: 2);

        broken.ShouldPublish.ShouldBeTrue();
        broken.Diagnostics.ShouldNotBeEmpty();

        var fixedText = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            SourceText.From("""
                func Main() -> unit {
                    val value = 1
                }
                """),
            version: 3);

        fixedText.ShouldPublish.ShouldBeTrue();
        fixedText.Diagnostics.ShouldBeEmpty();
    }

    [Fact]
    public void AcceptPendingSyntaxDiagnosticsForPublish_KeepsPreviousSnapshotDiagnosticsSticky()
    {
        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "main.rvn"));
        var dispatcher = new LanguageServerDispatcher(
            documents: default!,
            NullLogger<LanguageServerDispatcher>.Instance);
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            dispatcher,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        var oldText = SourceText.From("""
func Main() -> unit {
    Missing()
}
""");
        var newText = SourceText.From("""
// typing

func Main() -> unit {
    Missing()
}
""");
        var diagnostic = CreateDiagnostic(
            "RAV0103",
            "The name 'Missing' does not exist in the current context.",
            1,
            4,
            1,
            11,
            DiagnosticSeverity.Error);

        dispatcher.AcceptDiagnosticsForPublish(
                uri,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                [diagnostic],
                editorVersion: 1,
                snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                    uri.ToString(),
                    ProjectId.CreateNew(SolutionId.CreateNew()),
                    VersionStamp.Create(),
                    VersionStamp.Create()),
                sourceText: oldText)
            .ShouldPublish.ShouldBeTrue();

        var pending = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            newText,
            version: 2);

        var stickyDiagnostic = pending.Diagnostics.ShouldHaveSingleItem();
        pending.ShouldPublish.ShouldBeTrue();
        stickyDiagnostic.Code?.String.ShouldBe("RAV0103");
        stickyDiagnostic.Range.Start.Line.ShouldBe(3);
        stickyDiagnostic.Range.End.Line.ShouldBe(3);
    }

    [Fact]
    public void DocumentCompilerPublish_AfterStickyValToUseSyntaxPublish_ClearsStaleAppDiagnostic()
    {
        var samplePath = Path.Combine(GetRepositoryRoot(), "samples", "projects", "aspnet-minimal-api", "src", "main.rvn");
        var newText = SourceText.From(File.ReadAllText(samplePath));
        var oldText = SourceText.From(newText.ToString().Replace(
            "use app = builder.Build()",
            "val app = builder.Build()",
            StringComparison.Ordinal));
        var uri = DocumentUri.FromFileSystemPath(samplePath);
        var dispatcher = new LanguageServerDispatcher(
            documents: default!,
            NullLogger<LanguageServerDispatcher>.Instance);
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            dispatcher,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        var appOffset = oldText.ToString().IndexOf("app.MapPost(\"/submit\"", StringComparison.Ordinal);
        appOffset.ShouldBeGreaterThanOrEqualTo(0);
        var appRange = PositionHelper.ToRange(oldText, new TextSpan(appOffset, "app".Length));
        var staleDiagnostic = CreateDiagnostic(
            "RAV0103",
            "'app' is not in scope.",
            appRange.Start.Line,
            appRange.Start.Character,
            appRange.End.Line,
            appRange.End.Character,
            DiagnosticSeverity.Error);
        var projectId = ProjectId.CreateNew(SolutionId.CreateNew());

        dispatcher.AcceptDiagnosticsForPublish(
                uri,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                [staleDiagnostic],
                editorVersion: 1,
                snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                    uri.ToString(),
                    projectId,
                    VersionStamp.Create(),
                    VersionStamp.Create()),
                sourceText: oldText)
            .ShouldPublish.ShouldBeTrue();

        var stickySyntax = handler.AcceptPendingSyntaxDiagnosticsForPublish(uri, newText, version: 2);
        stickySyntax.ShouldPublish.ShouldBeTrue();
        stickySyntax.Diagnostics.Any(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                diagnostic.Message.Contains("'app' is not in scope", StringComparison.Ordinal))
            .ShouldBeTrue();

        var clearedCompiler = dispatcher.AcceptDiagnosticsForPublish(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            [],
            editorVersion: 2,
            snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                uri.ToString(),
                projectId,
                VersionStamp.Create(),
                VersionStamp.Create()),
            sourceText: newText);

        clearedCompiler.ShouldPublish.ShouldBeTrue();
        clearedCompiler.Diagnostics.ShouldBeEmpty();
    }

    [Fact]
    public void AcceptPendingSyntaxDiagnosticsForPublish_KeepsRav0403StickyWhenUnrelatedEditDoesNotMoveRange()
    {
        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "test.rvn"));
        var dispatcher = new LanguageServerDispatcher(
            documents: default!,
            NullLogger<LanguageServerDispatcher>.Instance);
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            dispatcher,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        var oldText = SourceText.From("""
namespace Utilities

func A(x: string) -> int {
    42
}

func Test2() -> IDisposable {
    return default!
}
""");
        var newText = SourceText.From("""
namespace Utilities

func A(x: string) -> int {
    43
}

func Test2() -> IDisposable {
    return default!
}
""");
        var diagnostic = CreateDiagnostic(
            "RAV0403",
            "The '!' operator treats the operand as non-null",
            7,
            11,
            7,
            19,
            DiagnosticSeverity.Information);

        dispatcher.AcceptDiagnosticsForPublish(
                uri,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                [diagnostic],
                editorVersion: 8,
                snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                    uri.ToString(),
                    ProjectId.CreateNew(SolutionId.CreateNew()),
                    VersionStamp.Create(),
                    VersionStamp.Create()),
                sourceText: oldText)
            .ShouldPublish.ShouldBeTrue();

        var pending = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            newText,
            version: null);

        var stickyDiagnostic = pending.Diagnostics.ShouldHaveSingleItem();
        pending.ShouldPublish.ShouldBeTrue();
        stickyDiagnostic.Code?.String.ShouldBe("RAV0403");
        stickyDiagnostic.Range.Start.Line.ShouldBe(7);
        stickyDiagnostic.Range.Start.Character.ShouldBe(11);
    }

    [Fact]
    public void AcceptPendingSyntaxDiagnosticsForPublish_HelloWorldSampleKeepsRav0403StickyWhenAddingExpression()
    {
        var samplePath = Path.Combine(GetRepositoryRoot(), "samples", "projects", "hello-world", "src", "test.rvn");
        var oldText = SourceText.From(File.ReadAllText(samplePath));
        var oldContent = oldText.ToString();
        var newText = SourceText.From(oldContent.Replace(
            """
[Obsolete("Test")]
func A(x: string) -> int {
    42
}
""",
            """
[Obsolete("Test")]
func A(x: string) -> int {
    42 + 2
}
""",
            StringComparison.Ordinal));
        oldText.ContentEquals(newText).ShouldBeFalse();

        var uri = DocumentUri.FromFileSystemPath(samplePath);
        var dispatcher = new LanguageServerDispatcher(
            documents: default!,
            NullLogger<LanguageServerDispatcher>.Instance);
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            dispatcher,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        var expressionOffset = oldContent.IndexOf("default!", StringComparison.Ordinal);
        expressionOffset.ShouldBeGreaterThanOrEqualTo(0);
        var expressionRange = PositionHelper.ToRange(oldText, new TextSpan(expressionOffset, "default!".Length));
        var diagnostic = CreateDiagnostic(
            "RAV0403",
            "The '!' operator treats the operand as non-null",
            expressionRange.Start.Line,
            expressionRange.Start.Character,
            expressionRange.End.Line,
            expressionRange.End.Character,
            DiagnosticSeverity.Information);

        dispatcher.AcceptDiagnosticsForPublish(
                uri,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                [diagnostic],
                editorVersion: 8,
                snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                    uri.ToString(),
                    ProjectId.CreateNew(SolutionId.CreateNew()),
                    VersionStamp.Create(),
                    VersionStamp.Create()),
                sourceText: oldText)
            .ShouldPublish.ShouldBeTrue();

        var pending = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            newText,
            version: null);

        var stickyDiagnostic = pending.Diagnostics.ShouldHaveSingleItem();
        pending.ShouldPublish.ShouldBeTrue();
        stickyDiagnostic.Code?.String.ShouldBe("RAV0403");
        stickyDiagnostic.Message.ShouldBe("The '!' operator treats the operand as non-null");
        stickyDiagnostic.Range.Start.Line.ShouldBe(expressionRange.Start.Line);
        stickyDiagnostic.Range.Start.Character.ShouldBe(expressionRange.Start.Character);

        var committedSyntax = dispatcher.AcceptDiagnosticsForPublish(
            uri,
            DocumentStore.DiagnosticLane.Syntax,
            [],
            editorVersion: 2,
            snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                uri.ToString(),
                ProjectId.CreateNew(SolutionId.CreateNew()),
                VersionStamp.Create(),
                VersionStamp.Create()),
            sourceText: newText);
        var committedStickyDiagnostic = committedSyntax.Diagnostics.ShouldHaveSingleItem();
        committedStickyDiagnostic.Code?.String.ShouldBe("RAV0403");

        var nextText = SourceText.From(newText.ToString().Replace("42 + 2", "42 + 3", StringComparison.Ordinal));
        var nextPending = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            nextText,
            version: null);
        var nextStickyDiagnostic = nextPending.Diagnostics.ShouldHaveSingleItem();
        nextStickyDiagnostic.Code?.String.ShouldBe("RAV0403");
    }

    [Fact]
    public void AcceptPendingSyntaxDiagnosticsForPublish_HelloWorldSampleDropsRav0403WhenBangIsRemoved()
    {
        var samplePath = Path.Combine(GetRepositoryRoot(), "samples", "projects", "hello-world", "src", "test.rvn");
        var oldText = SourceText.From(File.ReadAllText(samplePath));
        var oldContent = oldText.ToString();
        var newText = SourceText.From(oldContent.Replace("default!", "default", StringComparison.Ordinal));
        oldText.ContentEquals(newText).ShouldBeFalse();

        var uri = DocumentUri.FromFileSystemPath(samplePath);
        var dispatcher = new LanguageServerDispatcher(
            documents: default!,
            NullLogger<LanguageServerDispatcher>.Instance);
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            dispatcher,
            languageServer: default!,
            NullLogger<RavenTextDocumentSyncHandler>.Instance);
        var expressionOffset = oldContent.IndexOf("default!", StringComparison.Ordinal);
        expressionOffset.ShouldBeGreaterThanOrEqualTo(0);
        var expressionRange = PositionHelper.ToRange(oldText, new TextSpan(expressionOffset, "default!".Length));
        var diagnostic = CreateDiagnostic(
            "RAV0403",
            "The '!' operator treats the operand as non-null",
            expressionRange.Start.Line,
            expressionRange.Start.Character,
            expressionRange.End.Line,
            expressionRange.End.Character,
            DiagnosticSeverity.Information);

        dispatcher.AcceptDiagnosticsForPublish(
                uri,
                DocumentStore.DiagnosticLane.DocumentCompiler,
                [diagnostic],
                editorVersion: 8,
                snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                    uri.ToString(),
                    ProjectId.CreateNew(SolutionId.CreateNew()),
                    VersionStamp.Create(),
                    VersionStamp.Create()),
                sourceText: oldText)
            .ShouldPublish.ShouldBeTrue();

        var pending = handler.AcceptPendingSyntaxDiagnosticsForPublish(
            uri,
            newText,
            version: null);

        pending.ShouldPublish.ShouldBeTrue();
        pending.Diagnostics.ShouldBeEmpty();

        var committedSyntax = dispatcher.AcceptDiagnosticsForPublish(
            uri,
            DocumentStore.DiagnosticLane.Syntax,
            [],
            editorVersion: 2,
            snapshotKey: new DocumentStore.DiagnosticSnapshotKey(
                uri.ToString(),
                ProjectId.CreateNew(SolutionId.CreateNew()),
                VersionStamp.Create(),
                VersionStamp.Create()),
            sourceText: newText);
        committedSyntax.Diagnostics.ShouldBeEmpty();
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
    public void GetCarryForwardAnalyzerDiagnosticsForPresentation_KeepsDiagnosticsFromPreviousProjectSnapshot()
    {
        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV9027", "Value 'x' is never used.", 4, 8, 4, 9, DiagnosticSeverity.Warning),
            CreateDiagnostic("RAV9030", "Parameter 'title' is never used.", 26, 32, 26, 37, DiagnosticSeverity.Warning));
        var projectId = ProjectId.CreateNew(SolutionId.CreateNew());
        var documentVersion = VersionStamp.Create();
        var previousSnapshot = new DocumentStore.DiagnosticSnapshotKey(
            "file:///test.rvn",
            projectId,
            documentVersion,
            VersionStamp.Create());
        var currentSnapshot = new DocumentStore.DiagnosticSnapshotKey(
            "file:///test.rvn",
            projectId,
            documentVersion,
            previousSnapshot.ProjectVersion.GetNewerVersion());

        var carried = RavenTextDocumentSyncHandler.GetCarryForwardAnalyzerDiagnosticsForPresentation(
            version: 2,
            currentSnapshot,
            new RavenTextDocumentSyncHandler.VersionedDiagnostics(2, previousSnapshot, previous));

        carried.Select(static diagnostic => diagnostic.Code?.String).ShouldBe([
            "RAV9027",
            "RAV9030"
        ]);
    }

    [Fact]
    public void GetCarryForwardAnalyzerDiagnosticsForPresentation_KeepsDiagnosticsForSameProjectSnapshot()
    {
        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV9027", "Value 'x' is never used.", 4, 8, 4, 9, DiagnosticSeverity.Warning),
            CreateDiagnostic("RAV9030", "Parameter 'title' is never used.", 26, 32, 26, 37, DiagnosticSeverity.Warning));
        var snapshot = new DocumentStore.DiagnosticSnapshotKey(
            "file:///test.rvn",
            ProjectId.CreateNew(SolutionId.CreateNew()),
            VersionStamp.Create(),
            VersionStamp.Create());

        var carried = RavenTextDocumentSyncHandler.GetCarryForwardAnalyzerDiagnosticsForPresentation(
            version: 2,
            snapshot,
            new RavenTextDocumentSyncHandler.VersionedDiagnostics(2, snapshot, previous));

        carried.Select(static diagnostic => diagnostic.Code?.String).ShouldBe([
            "RAV9027",
            "RAV9030"
        ]);
    }

    [Fact]
    public void GetCarryForwardAnalyzerDiagnosticsForPresentation_FallsBackToEditorVersionWhenSnapshotIsUnavailable()
    {
        var previous = ImmutableArray.Create(
            CreateDiagnostic("RAV9030", "Parameter 'title' is never used.", 26, 32, 26, 37, DiagnosticSeverity.Warning));

        var carried = RavenTextDocumentSyncHandler.GetCarryForwardAnalyzerDiagnosticsForPresentation(
            version: 2,
            snapshotKey: null,
            new RavenTextDocumentSyncHandler.VersionedDiagnostics(2, SnapshotKey: null, previous));

        carried.Select(static diagnostic => diagnostic.Code?.String).ShouldBe([
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

    private static ConcurrentDictionary<DocumentUri, int> GetDocumentVersions(RavenTextDocumentSyncHandler handler)
    {
        var field = typeof(RavenTextDocumentSyncHandler).GetField(
            "_documentVersions",
            BindingFlags.Instance | BindingFlags.NonPublic);
        field.ShouldNotBeNull();
        return (ConcurrentDictionary<DocumentUri, int>)field!.GetValue(handler)!;
    }

    private static ConcurrentDictionary<DocumentUri, long> GetDocumentSessions(RavenTextDocumentSyncHandler handler)
    {
        var field = typeof(RavenTextDocumentSyncHandler).GetField(
            "_documentSessions",
            BindingFlags.Instance | BindingFlags.NonPublic);
        field.ShouldNotBeNull();
        return (ConcurrentDictionary<DocumentUri, long>)field!.GetValue(handler)!;
    }

    private static ConcurrentDictionary<DocumentUri, RavenTextDocumentSyncHandler.PendingDiagnosticsRequest> GetPendingDiagnostics(
        RavenTextDocumentSyncHandler handler)
    {
        var field = typeof(RavenTextDocumentSyncHandler).GetField(
            "_pendingDiagnostics",
            BindingFlags.Instance | BindingFlags.NonPublic);
        field.ShouldNotBeNull();
        return (ConcurrentDictionary<DocumentUri, RavenTextDocumentSyncHandler.PendingDiagnosticsRequest>)field!.GetValue(handler)!;
    }

    private static int GetPendingDiagnosticsRetryCount(RavenTextDocumentSyncHandler handler)
    {
        var field = typeof(RavenTextDocumentSyncHandler).GetField(
            "_pendingDiagnosticsRetries",
            BindingFlags.Instance | BindingFlags.NonPublic);
        field.ShouldNotBeNull();
        var countProperty = field!.GetValue(handler)!.GetType().GetProperty("Count");
        countProperty.ShouldNotBeNull();
        return (int)countProperty!.GetValue(field.GetValue(handler)!)!;
    }

    private static string GetRepositoryRoot()
        => Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
