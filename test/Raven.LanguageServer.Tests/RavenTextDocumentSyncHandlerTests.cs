using System.Collections.Generic;
using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;

using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class RavenTextDocumentSyncHandlerTests
{
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
    public void GetSaveDiagnosticsPolicy_UsesDeferredFullDiagnosticsWithoutWarmup()
    {
        var policy = RavenTextDocumentSyncHandler.GetSaveDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeFalse();
        policy.InitialMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.SyntaxOnly);
        policy.FullDiagnosticsDelayMilliseconds.ShouldNotBeNull();
        policy.FullDiagnosticsDelayMilliseconds.Value.ShouldBeGreaterThan(0);
        policy.DiagnosticsDelayMilliseconds.ShouldBe(0);
    }

    [Fact]
    public void GetEditDiagnosticsPolicy_UsesWarmupWithoutDeferredFullDiagnostics()
    {
        var policy = RavenTextDocumentSyncHandler.GetEditDiagnosticsPolicy();

        policy.IncludeWarmup.ShouldBeTrue();
        policy.InitialMode.ShouldBe(DocumentStore.DocumentDiagnosticsMode.SyntaxOnly);
        policy.FullDiagnosticsDelayMilliseconds.ShouldBeNull();
        policy.DiagnosticsDelayMilliseconds.ShouldBeGreaterThan(0);
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
    [InlineData(1, 0, "publishSyntaxDiagnostics")]
    [InlineData(1, 1, "publishSyntaxDiagnosticsSkipped")]
    [InlineData(1, 2, "publishSyntaxDiagnosticsUnchanged")]
    [InlineData(1, 3, "publishSyntaxDiagnosticsVersionMismatch")]
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
}
