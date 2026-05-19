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
    public async Task Handle_DocumentSemanticGateBusyWithoutCachedResult_ReturnsWithoutWaitingAsync()
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
    val answer = 1 + 2
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        using var heldLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");
        var hintsTask = handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var completedTask = await Task.WhenAny(hintsTask, Task.Delay(1000));
        completedTask.ShouldBe(hintsTask);

        var result = await hintsTask;
        result.ShouldBeEmpty();
    }

    [Fact]
    public async Task Handle_DocumentSemanticGateBusyWithCachedResult_ReturnsCachedHintsAsync()
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
    val answer = 1 + 2
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);
        var request = new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        };

        var firstResult = await handler.Handle(request, CancellationToken.None);
        var firstHints = firstResult.ToArray();
        firstHints.ShouldContain(static hint => hint.Label.String == ": int");

        using var heldLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");
        var hintsTask = handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = new LspRange
            {
                Start = new Position(0, 0),
                End = new Position(3, 0)
            }
        }, CancellationToken.None);

        var completedTask = await Task.WhenAny(hintsTask, Task.Delay(1000));
        completedTask.ShouldBe(hintsTask);

        var cachedHints = (await hintsTask).ToArray();
        cachedHints.Select(static hint => hint.Label.String).ShouldContain(": int");
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

    [Fact]
    public async Task Handle_LargeRange_BindsInvocationInitializersAsync()
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
        var padding = string.Join(Environment.NewLine, Enumerable.Range(0, 220).Select(static i => $"// padding {i}"));
        var code = $$"""
func Make() -> int {
    return 42
}

func Main() -> unit {
    val answer = Make()
}

{{padding}}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);
        var answerInsertion = code.IndexOf("answer", StringComparison.Ordinal) + "answer".Length;

        var largeResult = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var largeHints = largeResult.ToArray();
        var answerPosition = PositionHelper.ToRange(sourceText, new TextSpan(answerInsertion, 0)).Start;
        AssertHasHintAtInsertion(sourceText, largeHints, answerInsertion, ": int");

        var smallResult = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new TextSpan(answerInsertion, 0))
        }, CancellationToken.None);

        AssertHasHintAtInsertion(sourceText, smallResult.ToArray(), answerInsertion, ": int");
    }

    [Fact]
    public async Task Handle_InsertApplied_DoesNotReturnAnnotationHintAsync()
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
func Add(left: int, right: int) -> int {
    return left + right
}

func Main() -> unit {
    val answer: int = 1 + 2
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        result.ShouldBeEmpty();
    }

    [Fact]
    public async Task Handle_ForIdentifierTarget_ProvidesSourceApplicableTypeHintAsync()
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
    val numbers = [|1, 2, 3|]
    for number in numbers {
        number
    }

    for explicit: int in numbers {
        explicit
    }
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
        var loopTargetHint = hints.Single(hint =>
            hint.Label.String == ": int" &&
            hint.Position.Line == PositionHelper.ToRange(
                sourceText,
                new TextSpan(code.IndexOf("number in", StringComparison.Ordinal) + "number".Length, 0)).Start.Line);

        AssertSourceApplicable(
            sourceText,
            loopTargetHint,
            code.IndexOf("number in", StringComparison.Ordinal) + "number".Length,
            ": int");

        hints.ShouldNotContain(hint =>
            hint.Position.Line == PositionHelper.ToRange(
                sourceText,
                new TextSpan(code.IndexOf("explicit", StringComparison.Ordinal) + "explicit".Length, 0)).Start.Line &&
            hint.Label.String == ": int");
    }

    [Fact]
    public async Task Handle_UseDeclarationAndFunctionExpressions_ProvidesTypeHintsAsync()
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
class DisposableResource {
    func Dispose() -> unit {
    }
}

func Consume(callback: (int -> int)) -> int {
    return callback(41)
}

func Main() -> unit {
    use resource = DisposableResource()
    val result = Consume(value => value + 1)
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
        hints.Select(static hint => hint.Label.String).ShouldContain(": DisposableResource");
        hints.Select(static hint => hint.Label.String).ShouldContain(" -> int");

        var resourceHint = hints.Single(static hint => hint.Label.String == ": DisposableResource");
        AssertSourceApplicable(
            sourceText,
            resourceHint,
            code.IndexOf("resource", StringComparison.Ordinal) + "resource".Length,
            ": DisposableResource");

        var callbackInsertion = code.IndexOf("value =>", StringComparison.Ordinal) + "value".Length;
        var callbackPosition = PositionHelper.ToRange(sourceText, new TextSpan(callbackInsertion, 0)).Start;
        var callbackReturnHint = hints
            .Where(static hint => hint.Label.String == " -> int")
            .Single(hint => hint.Position.Line == callbackPosition.Line && hint.Position.Character == callbackPosition.Character);
        AssertSourceApplicable(
            sourceText,
            callbackReturnHint,
            callbackInsertion,
            " -> int");
    }

    [Fact]
    public async Task Handle_TopLevelAsyncFunctionExpression_ProvidesBodyLocalTypeHintsAsync()
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
import System.Threading.Tasks.*

class RequestContext {
    public val Text: string = "body"
}

func Accept(handler: func (RequestContext) -> Task<string>) -> unit { }

Accept(async func (context: RequestContext) {
    val content = await Task.FromResult(context.Text)
    return "submitted: $content"
})
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var contentHint = result.Single(static hint => hint.Label.String == ": string");
        AssertSourceApplicable(
            sourceText,
            contentHint,
            code.IndexOf("content", StringComparison.Ordinal) + "content".Length,
            ": string");
    }

    [Fact]
    public async Task Handle_UnimportedMetadataType_UsesSimpleAnnotationAsync()
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
    val json = System.Text.Json.Nodes.JsonObject()
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var hint = result.Single(static hint => hint.Label.String == ": JsonObject");
        AssertSourceApplicable(
            sourceText,
            hint,
            code.IndexOf("json", StringComparison.Ordinal) + "json".Length,
            ": JsonObject");
        AssertTooltipMentionsInsertion(hint, ": JsonObject");
    }

    [Fact]
    public async Task Handle_ImportedMetadataType_UsesSimpleAnnotationAsync()
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
import System.Text.Json.Nodes.*

func Main() -> unit {
    val json = JsonObject()
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var hint = result.Single(static hint => hint.Label.String == ": JsonObject");
        AssertSourceApplicable(
            sourceText,
            hint,
            code.IndexOf("json", StringComparison.Ordinal) + "json".Length,
            ": JsonObject");
        AssertTooltipMentionsInsertion(hint, ": JsonObject");
    }

    [Fact]
    public async Task Handle_LargeDocumentImportedMetadataType_UsesSimpleAnnotationAsync()
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
        var padding = string.Join(Environment.NewLine, Enumerable.Range(0, 220).Select(static i => $"// padding {i}"));
        var code = $$"""
import System.Text.Json.Nodes.*

func Main() -> unit {
    val json = JsonObject()
}

{{padding}}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var hints = result.ToArray();
        var hint = hints.Single(static hint => hint.Label.String == ": JsonObject");
        hints.ShouldNotContain(static hint => hint.Label.String == ": System.Text.Json.Nodes.JsonObject");
        AssertSourceApplicable(
            sourceText,
            hint,
            code.IndexOf("json", StringComparison.Ordinal) + "json".Length,
            ": JsonObject");
    }

    [Fact]
    public async Task Handle_LargeDocumentCurrentNamespaceSourceType_UsesSimpleAnnotationAsync()
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
        var padding = string.Join(Environment.NewLine, Enumerable.Range(0, 220).Select(static i => $"// padding {i}"));
        var code = $$"""
namespace Samples.VehicleCosts

import System.*

class FuelConsumptionRecord {
}

func Main() -> unit {
    val entry = FuelConsumptionRecord()
}

{{padding}}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var hints = result.ToArray();
        var hint = hints.Single(static hint => hint.Label.String == ": FuelConsumptionRecord");
        hints.ShouldNotContain(static hint => hint.Label.String == ": Samples.VehicleCosts.FuelConsumptionRecord");
        AssertSourceApplicable(
            sourceText,
            hint,
            code.IndexOf("entry", StringComparison.Ordinal) + "entry".Length,
            ": FuelConsumptionRecord");
    }

    [Fact]
    public async Task Handle_AmbiguousVisibleTypeName_UsesSimpleAnnotationAsync()
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
namespace First {
    class JsonObject {
    }
}

namespace Second {
    class JsonObject {
    }
}

import First.*
import Second.*

func Main() -> unit {
    val json = First.JsonObject()
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = FullDocumentRange(sourceText)
        }, CancellationToken.None);

        var hint = result.Single(static hint => hint.Label.String == ": JsonObject");
        AssertSourceApplicable(
            sourceText,
            hint,
            code.IndexOf("json", StringComparison.Ordinal) + "json".Length,
            ": JsonObject");
    }

    [Fact]
    public async Task Handle_EfCoreVehicleSample_ProvidesRangeScopedHintsForLocalsAndRouteFunctionExpressionsAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs");
        var filePath = Path.Combine(projectRoot, "src", "Api", "Main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var code = await File.ReadAllTextAsync(filePath);
        var uri = DocumentUri.FromFileSystemPath(filePath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "efcore-vehicle-costs",
                Uri = DocumentUri.FromFileSystemPath(projectRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var builderInsertion = code.IndexOf("builder", StringComparison.Ordinal) + "builder".Length;
        var builderHints = await GetHintsAtInsertionAsync(handler, uri, sourceText, builderInsertion);
        AssertHasHintAtInsertion(sourceText, builderHints, builderInsertion, ": ");

        var vehicleInsertion = code.IndexOf("vehicle =>", StringComparison.Ordinal) + "vehicle".Length;
        var vehicleHints = await GetHintsAtInsertionAsync(handler, uri, sourceText, vehicleInsertion);
        AssertHasHintAtInsertion(sourceText, vehicleHints, vehicleInsertion, " -> ");

        const string routeHandler = "async func (context: VehicleDbContext, cancellationToken: CancellationToken)";
        var routeHandlerInsertion = code.IndexOf(routeHandler, StringComparison.Ordinal) + routeHandler.Length;
        var routeHandlerHints = await GetHintsAtInsertionAsync(handler, uri, sourceText, routeHandlerInsertion);
        AssertHasHintAtInsertion(sourceText, routeHandlerHints, routeHandlerInsertion, " -> ");
    }

    [Fact]
    public async Task Handle_EfCoreExpressionTreesSample_ProvidesParameterHintForPipeExtensionLambdaAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees");
        var filePath = Path.Combine(projectRoot, "src", "main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var code = await File.ReadAllTextAsync(filePath);
        var uri = DocumentUri.FromFileSystemPath(filePath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "efcore-expression-trees",
                Uri = DocumentUri.FromFileSystemPath(projectRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        const string selectPipe = "|> Select(user => user.Name)";
        var selectPipeStart = code.IndexOf(selectPipe, StringComparison.Ordinal);
        selectPipeStart.ShouldBeGreaterThanOrEqualTo(0);
        var parameterInsertion = selectPipeStart + selectPipe.IndexOf("user", StringComparison.Ordinal) + "user".Length;

        var hints = await GetHintsAtInsertionAsync(handler, uri, sourceText, parameterInsertion);

        AssertHasHintAtInsertion(sourceText, hints, parameterInsertion, ": User");
    }

    [Fact]
    public async Task Handle_LargeViewportRange_PrioritizesCompleteHintPlacementOverTooltipsAsync()
    {
        var sampleRoot = Path.Combine(
            FindRepositoryRoot(),
            "samples",
            "projects",
            "test-case");
        var documentPath = Path.Combine(sampleRoot, "src", "main.rvn");

        Directory.Exists(sampleRoot).ShouldBeTrue();
        File.Exists(documentPath).ShouldBeTrue();

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "test-case",
                Uri = DocumentUri.FromFileSystemPath(sampleRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        var code = await File.ReadAllTextAsync(documentPath);
        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = new LspRange
            {
                Start = new Position(133, 0),
                End = new Position(250, 0)
            }
        }, CancellationToken.None);

        var hints = result.ToArray();
        var doubledInsertion = code.IndexOf("doubled", StringComparison.Ordinal) + "doubled".Length;
        var describeStringsTextInsertion =
            code.LastIndexOf("text = \"[\"", StringComparison.Ordinal) + "text".Length;

        AssertHasHintAtInsertion(sourceText, hints, doubledInsertion, ": ");
        AssertHasHintAtInsertion(sourceText, hints, describeStringsTextInsertion, ": string");
        hints.All(static hint => hint.Tooltip is null).ShouldBeTrue();
    }

    [Fact]
    public async Task Handle_TestCasePatternDesignations_ProvidesTypeHintsAsync()
    {
        var sampleRoot = Path.Combine(
            FindRepositoryRoot(),
            "samples",
            "projects",
            "test-case");
        var documentPath = Path.Combine(sampleRoot, "src", "main.rvn");

        Directory.Exists(sampleRoot).ShouldBeTrue();
        File.Exists(documentPath).ShouldBeTrue();

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "test-case",
                Uri = DocumentUri.FromFileSystemPath(sampleRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        var code = await File.ReadAllTextAsync(documentPath);
        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);

        var nameInsertion = code.IndexOf("(2, name)", StringComparison.Ordinal) + "(2, name".Length;
        var keyInsertion = code.IndexOf("(key, value) in doubled", StringComparison.Ordinal) + "(key".Length;
        var valueInsertion = code.IndexOf("(key, value) in doubled", StringComparison.Ordinal) + "(key, value".Length;

        var nameHints = await GetHintsAtInsertionAsync(handler, uri, sourceText, nameInsertion);
        var keyHints = await GetHintsAtInsertionAsync(handler, uri, sourceText, keyInsertion);
        var valueHints = await GetHintsAtInsertionAsync(handler, uri, sourceText, valueInsertion);

        AssertHasHintAtInsertion(sourceText, nameHints, nameInsertion, ": string");
        AssertHasHintAtInsertion(sourceText, keyHints, keyInsertion, ": string");
        AssertHasHintAtInsertion(sourceText, valueHints, valueInsertion, ": int");
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }

    private static LspRange FullDocumentRange(SourceText sourceText)
        => PositionHelper.ToRange(sourceText, new TextSpan(0, sourceText.Length));

    private static async Task<InlayHint[]> GetHintsAtInsertionAsync(
        InlayHintHandler handler,
        DocumentUri uri,
        SourceText sourceText,
        int insertionPosition)
    {
        var result = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0))
        }, CancellationToken.None);

        return result.ToArray();
    }

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

    private static void AssertHasHintAtInsertion(SourceText sourceText, InlayHint[] hints, int insertionPosition, string labelPrefix)
    {
        var expectedPosition = PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0)).Start;
        hints.ShouldContain(hint =>
            hint.Label.String.StartsWith(labelPrefix, StringComparison.Ordinal) &&
            hint.Position.Line == expectedPosition.Line &&
            hint.Position.Character == expectedPosition.Character);
    }

    private static void AssertTooltipMentionsInsertion(InlayHint hint, string insertionText)
    {
        hint.Tooltip.ShouldNotBeNull();
        var tooltip = hint.Tooltip.MarkupContent;
        tooltip.ShouldNotBeNull();
        tooltip.Kind.ShouldBe(MarkupKind.Markdown);
        tooltip.Value.ShouldContain("Inferred type");
        tooltip.Value.ShouldContain(insertionText);
    }

    private static string FindRepositoryRoot()
    {
        var directory = new DirectoryInfo(AppContext.BaseDirectory);
        while (directory is not null)
        {
            if (File.Exists(Path.Combine(directory.FullName, "Raven.sln")))
                return directory.FullName;

            directory = directory.Parent;
        }

        throw new DirectoryNotFoundException("Could not locate Raven.sln from test output directory.");
    }
}
