using System.Reflection;
using System.Text.RegularExpressions;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class LanguageServerSnapshotConsistencyTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-snapshot-{Guid.NewGuid():N}");

    [Fact]
    public async Task HoverHandler_ClearedDocument_DoesNotReuseStaleStateAsync()
    {
        var (store, _, uri) = CreateWorkspace("val number = 42");
        store.UpsertDocument(uri, string.Empty);

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(0, 0)
        }, CancellationToken.None);

        hover.ShouldBeNull();
    }

    [Fact]
    public async Task HoverHandler_UpdatedDocument_DoesNotReuseCachedHoverFromPreviousVersionAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.Console.*

func Main() -> unit {
    val number = 42
    WriteLine(number)
}
""");
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var firstHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, CancellationToken.None);

        firstHover.ShouldNotBeNull();
        firstHover.Contents.ShouldNotBeNull();

        store.UpsertDocument(uri, """
import System.Console.*

func Main() -> unit {
    val value = 42
    WriteLine(value)
}
""");

        var secondHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, CancellationToken.None);

        secondHover.ShouldNotBeNull();
        secondHover.Contents.ShouldNotBeNull();
        secondHover.Range.ShouldNotBeNull();
        secondHover.Range.Start.Character.ShouldBe(14);
        secondHover.Range.End.Character.ShouldBe(19);
    }

    [Fact]
    public async Task HoverHandler_InvalidateDocument_ClearsCachedEntriesForReopenedFileAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.Console.*

func Main() -> unit {
    val query = 42
    WriteLine(query)
}
""");
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var firstHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, CancellationToken.None);

        firstHover.ShouldNotBeNull();

        var cacheField = typeof(HoverHandler).GetField("_hoverCache", BindingFlags.Instance | BindingFlags.NonPublic);
        cacheField.ShouldNotBeNull();
        var cache = cacheField!.GetValue(handler);
        cache.ShouldNotBeNull();
        cache.GetType().GetProperty("Count")!.GetValue(cache).ShouldBe(1);

        handler.InvalidateDocument(uri);

        cache.GetType().GetProperty("Count")!.GetValue(cache).ShouldBe(0);
    }

    [Fact]
    public async Task HoverHandler_CanceledRequest_ReturnsNullAndDoesNotPopulateCacheAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.Console.*

func Main() -> unit {
    val number = 42
    WriteLine(number)
}
""");
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        using var cancellation = new CancellationTokenSource();
        await cancellation.CancelAsync();

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, cancellation.Token);

        hover.ShouldBeNull();

        var cacheField = typeof(HoverHandler).GetField("_hoverCache", BindingFlags.Instance | BindingFlags.NonPublic);
        cacheField.ShouldNotBeNull();
        var cache = cacheField!.GetValue(handler);
        cache.ShouldNotBeNull();
        cache.GetType().GetProperty("Count")!.GetValue(cache).ShouldBe(0);
    }

    [Fact]
    public async Task HoverHandler_BrokenNearbyCode_StillShowsUnchangedLocalSymbolAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.Console.*

func Main() -> unit {
    val number = 42
    val broken =
    WriteLine(number)
}
""");
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(5, 14)
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("number");
        hover.Contents.MarkupContent!.Value.ShouldNotContain("Error");
        hover.Range.ShouldNotBeNull();
        hover.Range.Start.Line.ShouldBe(5);
        hover.Range.Start.Character.ShouldBe(14);
    }

    [Fact]
    public async Task HoverHandler_MalformedEarlierRecord_DoesNotFailLaterHoverAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.Collections.Generic.*

record Foo(
    val Name: string
    val Count: int
)

union JsonValue(string | double | bool | JsonObject)
record JsonObject(Properties: IDictionary<string, JsonValue>)

val x = JsonObject([
    "name": 42
])

x.Properties["name"].HasValue
""");
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(10, 14)
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("JsonObject");
    }

    [Fact]
    public async Task HoverHandler_MalformedRecordBeforeUnion_DoesNotFailTypeHoverAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.*
import System.Console.*

val foo = Foo(
    Name: "Foo",
    Status: .OnMaintenance(.UtcNow, "Test"),
)

record Foo(
    val Name: string,
    val Status: Status
    val Test: int | bool
)

[RavenUnionJsonConverter("kind")]
union Status {
    case Active(Date: DateTimeOffset)
    case OnMaintenance(Date: DateTimeOffset, Reason: string)
}
""");
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(5, 7)
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("Status");
    }

    [Fact]
    public async Task HoverHandler_LocalDeclarationRange_DoesNotCoverPipeInitializerInvocationAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.*

class Runner {
    static func Where(value: Int32, predicate: (Int32) -> bool) -> Int32 {
        return value
    }

    static func Main() -> unit {
        val query = 5
            |> Where(x => x > 1)

        query
    }
}
""");

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var queryHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(8, 12)
        }, CancellationToken.None);

        queryHover.ShouldNotBeNull();
        queryHover!.Range.ShouldNotBeNull();
        queryHover.Range.Start.Line.ShouldBe(8);
        queryHover.Range.End.Line.ShouldBe(8);
        queryHover.Range.End.Character.ShouldBeGreaterThan(queryHover.Range.Start.Character);

        var whereHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(9, 16)
        }, CancellationToken.None);

        whereHover.ShouldNotBeNull();
        whereHover!.Contents.ShouldNotBeNull();
        whereHover.Range.ShouldNotBeNull();
        whereHover.Range.Start.Line.ShouldBe(9);
    }

    [Fact]
    public async Task HoverHandler_EditCycle_MemberReceiverAndInvocationTargetStayDistinctAsync()
    {
        var initialText = """
import System.Console.*

class Counter {
    val Count: int => 1

    func Next() -> int {
        Count + 1
    }
}

func Main() -> unit {
    val counter = Counter()
    WriteLine(counter.Next())
}
""";
        var updatedText = """
import System.Console.*

class Counter {
    val Count: int => 1

    func Next() -> int {
        Count + 1
    }
}

func Main() -> unit {
    val padding = 0
    val counter = Counter()
    WriteLine(counter.Next())
}
""";

        var (store, _, uri) = CreateWorkspace(initialText);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        await AssertMemberAccessHoverAsync(store, handler, uri, initialText, expectedLine: 12);

        store.UpsertDocument(uri, updatedText);

        await AssertMemberAccessHoverAsync(store, handler, uri, updatedText, expectedLine: 13);
    }

    [Fact]
    public async Task HoverHandler_EditCycle_PropertyRemovedAndReadded_ResolvesPropertyHoverAsync()
    {
        var initialText = """
class Foo {
    val Test: string => ""
}

func Main() -> unit {
    val foo = Foo()
    foo.Test
}
""";
        var withoutPropertyText = """
class Foo {
}

func Main() -> unit {
    val foo = Foo()
    foo.Test
}
""";
        var restoredText = initialText;

        var (store, _, uri) = CreateWorkspace(initialText);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        await AssertPropertyMemberHoverAsync(store, handler, uri, initialText);

        store.UpsertDocument(uri, withoutPropertyText);
        var missingHover = await GetPropertyMemberHoverAsync(store, handler, uri, withoutPropertyText);
        missingHover?.Contents.MarkupContent?.Value.ShouldNotContain("val Test: string");

        store.UpsertDocument(uri, restoredText);

        await AssertPropertyMemberHoverAsync(store, handler, uri, restoredText);
    }

    [Fact]
    public async Task HoverHandler_EditCycle_BodyDeclarationEdit_ResolvesUpdatedLocalHoverAsync()
    {
        var initialText = """
class Runner {
    func Main(value: int) -> int {
        val result = value
        return result
    }
}
""";
        var updatedText = """
class Runner {
    func Main(value: int) -> int {
        val renamed = value
        return renamed
    }
}
""";

        var (store, _, uri) = CreateWorkspace(initialText);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        await AssertLocalHoverAsync(store, handler, uri, initialText, "return result", "result", "val result: int");

        store.UpsertDocument(uri, updatedText);

        await AssertLocalHoverAsync(store, handler, uri, updatedText, "return renamed", "renamed", "val renamed: int");
    }

    [Fact]
    public async Task HoverHandler_EditCycle_AddedOverload_ResolvesNewSignatureHoverAsync()
    {
        var initialText = """
class Runner {
    static func Pick(value: int) -> int {
        return value
    }

    static func Main() -> unit {
        Pick(1)
    }
}
""";
        var updatedText = """
class Runner {
    static func Pick(value: int) -> int {
        return value
    }

    static func Pick(text: string) -> string {
        return text
    }

    static func Main() -> unit {
        Pick("text")
    }
}
""";

        var (store, _, uri) = CreateWorkspace(initialText);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        store.UpsertDocument(uri, updatedText);

        await AssertMethodDeclarationHoverAsync(
            store,
            handler,
            uri,
            updatedText,
            "Pick(text: string)",
            "func Pick(text: string) -> string");
    }

    [Fact]
    public async Task HoverHandler_EditCycle_PipeInvocationTargetStaysMethodAsync()
    {
        var initialText = """
import System.*

class Runner {
    static func Where(value: Int32, predicate: (Int32) -> bool) -> Int32 {
        return value
    }

    static func Main() -> unit {
        val query = 5
            |> Where(x => x > 1)

        query
    }
}
""";
        var updatedText = """
import System.*

class Runner {
    static func Where(value: Int32, predicate: (Int32) -> bool) -> Int32 {
        return value
    }

    static func Main() -> unit {
        val padding = 0
        val query = 5
            |> Where(x => x > 1)

        query
    }
}
""";

        var (store, _, uri) = CreateWorkspace(initialText);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        await AssertPipeWhereHoverAsync(store, handler, uri, initialText, expectedLine: 9);

        store.UpsertDocument(uri, updatedText);

        await AssertPipeWhereHoverAsync(store, handler, uri, updatedText, expectedLine: 10);
    }

    [Fact]
    public async Task CompletionHandler_ClearedDocument_ReturnsWithoutOutOfBoundsFailureAsync()
    {
        var (store, _, uri) = CreateWorkspace("val number = 42");
        store.UpsertDocument(uri, string.Empty);

        var handler = new CompletionHandler(store, NullLogger<CompletionHandler>.Instance);
        var completions = await handler.Handle(new CompletionParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(0, 0)
        }, CancellationToken.None);

        completions.ShouldNotBeNull();
        completions.Items.ShouldNotBeNull();
    }

    [Fact]
    public async Task GetAnalysisContextAsync_ClearedDocument_ReturnsCompilationOwnedSyntaxTreeAsync()
    {
        var (store, _, uri) = CreateWorkspace("val number = 42");
        store.UpsertDocument(uri, string.Empty);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);

        context.ShouldNotBeNull();
        context.Value.SourceText.ToString().ShouldBe(string.Empty);
        context.Value.Compilation.SyntaxTrees.ShouldContain(context.Value.SyntaxTree);
        Should.NotThrow(() => context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree));
    }

    [Fact]
    public async Task GetAnalysisContextAsync_RapidSuccessiveUpdates_StaysSnapshotConsistentAsync()
    {
        var (store, _, uri) = CreateWorkspace("val number = 42");

        store.UpsertDocument(uri, "val number = 100");
        store.UpsertDocument(uri, """
record Payment(amount: int)

val payment = Payment(42)
""");

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);

        context.ShouldNotBeNull();
        context.Value.SourceText.ToString().ShouldContain("record Payment");
        context.Value.Compilation.SyntaxTrees.ShouldContain(context.Value.SyntaxTree);
        Should.NotThrow(() => context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree));
    }

    [Fact]
    public async Task GetSemanticModelAsync_ReturnsCurrentDocumentSemanticModel_AndInvalidatesOnUpdateAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
func Main() -> () {
    val number = 42
}
""");

        var firstContext = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        var firstModel = await store.GetSemanticModelAsync(uri, CancellationToken.None);
        var secondContext = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        var secondModel = await store.GetSemanticModelAsync(uri, CancellationToken.None);

        firstContext.ShouldNotBeNull();
        secondContext.ShouldNotBeNull();
        firstModel.ShouldNotBeNull();
        secondModel.ShouldNotBeNull();
        ReferenceEquals(firstContext.Value.SyntaxTree, secondContext.Value.SyntaxTree).ShouldBeTrue();
        Should.NotThrow(() => firstModel.GetDiagnostics());
        Should.NotThrow(() => secondModel.GetDiagnostics());

        store.UpsertDocument(uri, """
func Main() -> () {
    val value = 100
}
""");

        var updatedContext = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        var updatedModel = await store.GetSemanticModelAsync(uri, CancellationToken.None);

        updatedContext.ShouldNotBeNull();
        updatedModel.ShouldNotBeNull();
        updatedContext.Value.SourceText.ToString().ShouldContain("value = 100");
        ReferenceEquals(firstContext.Value.SyntaxTree, updatedContext.Value.SyntaxTree).ShouldBeFalse();
        Should.NotThrow(() => updatedModel.GetDiagnostics());
    }

    [Fact]
    public async Task GetAnalysisContextAsync_ProjectBackedDocumentFullReplacement_DoesNotRetainStaleGenericScopeDiagnosticsAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.*

func Main() -> () {
    val value = 42
    Console.WriteLine(value)
}
""");

        store.UpsertDocument(uri, """
import System.*
import System.Console.*

func Main() -> () {
    val r = Parse<int>("42")
    WriteLine(r)
}

func Parse<T>(str: string) -> T
    where T: IParsable<T>
    => T.Parse(str, null)
""");

        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        diagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal)).ShouldBeFalse();

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();
        context.Value.Compilation.SyntaxTrees.ShouldContain(context.Value.SyntaxTree);
        Should.NotThrow(() => context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree));
    }

    [Fact]
    public async Task GetAnalysisContextAsync_WorkspaceReload_DoesNotReuseStaleCachedAnalysisAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var projectPath = Path.Combine(_tempRoot, "App.rvnproj");
        File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var filePath = Path.Combine(_tempRoot, "src", "main.rvn");
        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);
        File.WriteAllText(filePath, """
import System.*

func Main() -> () {
    val value = 42
    Console.WriteLine(value)
}
""");

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
        var uri = DocumentUri.FromFileSystemPath(filePath);

        var firstContext = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        firstContext.ShouldNotBeNull();

        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var secondContext = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        secondContext.ShouldNotBeNull();
        secondContext.Value.Document.Id.ShouldNotBe(firstContext.Value.Document.Id);
        secondContext.Value.Document.Project.Id.ShouldNotBe(firstContext.Value.Document.Project.Id);
        secondContext.Value.Compilation.SyntaxTrees.ShouldContain(secondContext.Value.SyntaxTree);
        Should.NotThrow(() => secondContext.Value.Compilation.GetSemanticModel(secondContext.Value.SyntaxTree));
    }

    [Fact]
    public async Task GetSemanticModelAsync_ProjectVersionChangeWithoutDocumentEdit_InvalidatesCachedAnalysisAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var projectPath = Path.Combine(_tempRoot, "App.rvnproj");
        File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var mainFilePath = Path.Combine(_tempRoot, "src", "main.rvn");
        var helperFilePath = Path.Combine(_tempRoot, "src", "helper.rvn");
        Directory.CreateDirectory(Path.GetDirectoryName(mainFilePath)!);
        File.WriteAllText(mainFilePath, """
func Main() -> () {
    Helper()
}
""");
        File.WriteAllText(helperFilePath, """
func Helper() -> () { }
""");

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
        var mainUri = DocumentUri.FromFileSystemPath(mainFilePath);
        var helperUri = DocumentUri.FromFileSystemPath(helperFilePath);

        var firstContext = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        var firstModel = await store.GetSemanticModelAsync(mainUri, CancellationToken.None);

        firstContext.ShouldNotBeNull();
        firstModel.ShouldNotBeNull();

        store.UpsertDocument(helperUri, """
func Helper() -> () {
    val answer = 42
}
""");

        var secondContext = await store.GetAnalysisContextAsync(mainUri, CancellationToken.None);
        var secondModel = await store.GetSemanticModelAsync(mainUri, CancellationToken.None);

        secondContext.ShouldNotBeNull();
        secondModel.ShouldNotBeNull();
        secondContext.Value.Document.Version.ShouldBe(firstContext.Value.Document.Version);
        secondContext.Value.Document.Project.Version.ShouldNotBe(firstContext.Value.Document.Project.Version);
        ReferenceEquals(firstContext.Value.Compilation, secondContext.Value.Compilation).ShouldBeFalse();
        secondContext.Value.Compilation.SyntaxTrees.ShouldContain(secondContext.Value.SyntaxTree);
    }

    [Fact]
    public async Task WarmAnalysisAsync_CanPopulateSemanticModelWhileDocumentSemanticGateIsHeldAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
func Main() -> () {
    val number = 42
}
""");

        _ = await store.GetAnalysisContextAsync(uri, CancellationToken.None);

        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");
        await store.WarmAnalysisAsync(uri, shouldSkipWork: null, CancellationToken.None);

        var cacheField = typeof(DocumentStore).GetField("_documentAnalysisCache", BindingFlags.Instance | BindingFlags.NonPublic);
        cacheField.ShouldNotBeNull();

        var cache = cacheField!.GetValue(store);
        cache.ShouldNotBeNull();

        var entriesProperty = cache!.GetType().GetProperty("Values");
        entriesProperty.ShouldNotBeNull();

        var entries = ((System.Collections.IEnumerable)entriesProperty!.GetValue(cache)!).Cast<object>().ToArray();
        entries.Length.ShouldBe(1);

        var semanticModelField = entries[0].GetType().GetField("_semanticModel", BindingFlags.Instance | BindingFlags.NonPublic);
        semanticModelField.ShouldNotBeNull();

        var lazySemanticModel = semanticModelField!.GetValue(entries[0]);
        lazySemanticModel.ShouldNotBeNull();

        var isValueCreatedProperty = lazySemanticModel!.GetType().GetProperty("IsValueCreated");
        isValueCreatedProperty.ShouldNotBeNull();
        isValueCreatedProperty!.GetValue(lazySemanticModel).ShouldBe(true);

        var valueProperty = lazySemanticModel.GetType().GetProperty("Value");
        valueProperty.ShouldNotBeNull();
        var warmedModel = valueProperty!.GetValue(lazySemanticModel);
        var returnedModel = await store.GetSemanticModelAsync(uri, CancellationToken.None);

        ReferenceEquals(warmedModel, returnedModel).ShouldBeTrue();
    }

    [Fact]
    public async Task WarmAnalysisAsync_WaitsForCompilerGateInsteadOfSkippingAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
func Main() -> () {
    val number = 42
}
""");

        using var compilerLease = await store.EnterCompilerAccessAsync(CancellationToken.None, "test", uri);
        var warmupTask = store.WarmAnalysisAsync(uri, shouldSkipWork: null, CancellationToken.None);

        await Task.Delay(100);
        warmupTask.IsCompleted.ShouldBeFalse();

        compilerLease.Dispose();
        await warmupTask;

        var returnedModel = await store.GetSemanticModelAsync(uri, CancellationToken.None);
        returnedModel.ShouldNotBeNull();
    }

    [Fact]
    public async Task HoverHandler_WaitsForDocumentSemanticGateInsteadOfReturningNullAsync()
    {
        var (store, _, uri) = CreateWorkspace("""
import System.Console.*

func Main() -> unit {
    val number = 42
    WriteLine(number)
}
""");

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");

        var hoverTask = handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = new Position(4, 14)
        }, CancellationToken.None);

        await Task.Delay(100);
        hoverTask.IsCompleted.ShouldBeFalse();

        semanticLease.Dispose();
        var hover = await hoverTask;

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("number");
    }

    [Fact]
    public async Task HoverHandler_TypeDeclarationIdentifier_UsesSyntaxHoverWithoutSemanticGateAsync()
    {
        var text = """
record Foo(
    val Name: string,
    val Status: Status
    val Test: int | bool
)

[RavenUnionJsonConverter("kind")]
union Status {
    case Active
}
""";
        var (store, _, uri) = CreateWorkspace(text);
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var statusOffset = text.IndexOf("Status {", StringComparison.Ordinal);
        statusOffset.ShouldBeGreaterThanOrEqualTo(0);
        var position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(statusOffset, 0)).Start;
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");

        var hoverTask = handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = position
        }, CancellationToken.None);

        var completed = await Task.WhenAny(hoverTask, Task.Delay(1000));
        completed.ShouldBe(hoverTask);

        var hover = await hoverTask;
        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("union Status");
        hover.Contents.MarkupContent!.Value.ShouldContain("Union");
    }

    [Fact]
    public async Task HoverHandler_PrimaryConstructorParameterDeclaration_UsesSyntaxHoverWithoutSemanticGateAsync()
    {
        var text = """
record Foo(
    val Name: string,
    val Status: Status
)

union Status {
    case Active
}
""";
        var (store, _, uri) = CreateWorkspace(text);
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var nameOffset = text.IndexOf("Name: string", StringComparison.Ordinal);
        nameOffset.ShouldBeGreaterThanOrEqualTo(0);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");

        var hoverTask = handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(nameOffset, 0)).Start
        }, CancellationToken.None);

        var completed = await Task.WhenAny(hoverTask, Task.Delay(1000));
        completed.ShouldBe(hoverTask);

        var hover = await hoverTask;
        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("val Name: string");
        hover.Contents.MarkupContent!.Value.ShouldContain("Property in `Foo`");
    }

    [Fact]
    public async Task HoverHandler_MethodDeclarationIdentifier_UsesSyntaxHoverWithoutSemanticGateAsync()
    {
        var text = """
class Runner {
    static func Pick(text: string) -> string {
        return text
    }
}
""";
        var (store, _, uri) = CreateWorkspace(text);
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var pickOffset = text.IndexOf("Pick(text: string)", StringComparison.Ordinal);
        pickOffset.ShouldBeGreaterThanOrEqualTo(0);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");

        var hoverTask = handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(pickOffset + 1, 0)).Start
        }, CancellationToken.None);

        var completed = await Task.WhenAny(hoverTask, Task.Delay(1000));
        completed.ShouldBe(hoverTask);

        var hover = await hoverTask;
        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("static func Pick(text: string) -> string");
        hover.Contents.MarkupContent!.Value.ShouldContain("Method in `class Runner`");
    }

    [Fact]
    public async Task HoverHandler_MethodParameterDeclaration_UsesSyntaxHoverWithoutSemanticGateAsync()
    {
        var text = """
class Runner {
    static async func Main(args: string[]) -> Task {
        return Task.CompletedTask
    }
}
""";
        var (store, _, uri) = CreateWorkspace(text);
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var argsOffset = text.IndexOf("args: string[]", StringComparison.Ordinal);
        argsOffset.ShouldBeGreaterThanOrEqualTo(0);
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");

        var hoverTask = handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(argsOffset + 2, 0)).Start
        }, CancellationToken.None);

        var completed = await Task.WhenAny(hoverTask, Task.Delay(1000));
        completed.ShouldBe(hoverTask);

        var hover = await hoverTask;
        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("args: string[]");
        hover.Contents.MarkupContent!.Value.ShouldContain("Parameter in `static async func Main(args: string[]) -> Task`");
    }

    [Fact]
    public async Task HoverHandler_ProjectBackedExplicitTypeIdentifiers_ShowNamedTypeSignaturesAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var projectPath = Path.Combine(_tempRoot, "AspNetMinimalApi.rvnproj");
        File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>AspNetMinimalApi</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var filePath = Path.Combine(_tempRoot, "src", "main.rvn");
        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);
        var text = """
func ping(name: string) -> PingResult {
    PingResult("pong $name")
}

func ping1(name: string) -> Result<PingResult, CustomError> {
    match name {
        "Bob" | "bob" => Ok(PingResult("pong $name"))
        _ => Error(CustomError("Invalid name"))
    }
}

record PingResult(val Message: string)
record CustomError(val Message: string)
""";
        File.WriteAllText(filePath, text);

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
        var uri = DocumentUri.FromFileSystemPath(filePath);
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
        var root = context.Value.SyntaxTree.GetRoot();

        var sourceText = context.Value.SourceText;
        var pingResultOffset = text.IndexOf("-> PingResult", StringComparison.Ordinal);
        pingResultOffset.ShouldBeGreaterThanOrEqualTo(0);
        pingResultOffset += "-> ".Length + 2;

        var ping1ResultOffset = text.IndexOf("Result<PingResult, CustomError>", StringComparison.Ordinal);
        ping1ResultOffset.ShouldBeGreaterThanOrEqualTo(0);
        var customErrorOffset = ping1ResultOffset + "Result<PingResult, ".Length + 2;

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var pingResultHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(pingResultOffset, 0)).Start
        }, CancellationToken.None);

        pingResultHover.ShouldNotBeNull();
        pingResultHover.Contents.MarkupContent.ShouldNotBeNull();
        pingResultHover.Contents.MarkupContent!.Value.ShouldContain("PingResult");
        pingResultHover.Contents.MarkupContent!.Value.ShouldNotContain("```raven\n()\n```");

        var customErrorHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(customErrorOffset, 0)).Start
        }, CancellationToken.None);

        customErrorHover.ShouldNotBeNull();
        customErrorHover.Contents.MarkupContent.ShouldNotBeNull();
        customErrorHover.Contents.MarkupContent!.Value.ShouldContain("CustomError");
        customErrorHover.Contents.MarkupContent!.Value.ShouldNotContain("```raven\n()\n```");
    }

    [Fact]
    public async Task HoverHandler_RepoAspNetSample_ExplicitTypeIdentifiers_ShowNamedTypeSignaturesAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "aspnet-minimal-api");
        var filePath = Path.Combine(projectRoot, "src", "main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var text = File.ReadAllText(filePath);
        var uri = DocumentUri.FromFileSystemPath(filePath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "aspnet-minimal-api",
                Uri = DocumentUri.FromFileSystemPath(projectRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();
        var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
        var root = context.Value.SyntaxTree.GetRoot();

        var sourceText = context.Value.SourceText;
        var pingResultOffset = text.IndexOf("-> PingResult", StringComparison.Ordinal);
        pingResultOffset.ShouldBeGreaterThanOrEqualTo(0);
        pingResultOffset += "-> ".Length + 2;

        var customErrorOffset = text.IndexOf("CustomError>", StringComparison.Ordinal);
        customErrorOffset.ShouldBeGreaterThanOrEqualTo(0);
        customErrorOffset += 2;

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        foreach (var (offset, expectedName) in new[]
                 {
                     (pingResultOffset, "PingResult"),
                     (customErrorOffset, "CustomError")
                 })
        {
            var hover = await handler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = PositionHelper.ToRange(sourceText, new TextSpan(offset, 0)).Start
            }, CancellationToken.None);

            hover.ShouldNotBeNull();
            hover.Contents.MarkupContent.ShouldNotBeNull();
            hover.Contents.MarkupContent!.Value.ShouldContain(expectedName);
            hover.Contents.MarkupContent!.Value.ShouldNotContain("```raven\n()\n```");
        }
    }

    [Fact]
    public async Task HoverHandler_RepoAspNetSample_UnionCaseIdentifiers_DoNotFallbackToUnitAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "aspnet-minimal-api");
        var filePath = Path.Combine(projectRoot, "src", "main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var text = File.ReadAllText(filePath);
        var uri = DocumentUri.FromFileSystemPath(filePath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "aspnet-minimal-api",
                Uri = DocumentUri.FromFileSystemPath(projectRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();
        var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
        var root = context.Value.SyntaxTree.GetRoot();

        var sourceText = context.Value.SourceText;
        var okOffset = text.IndexOf("Ok(PingResult", StringComparison.Ordinal);
        okOffset.ShouldBeGreaterThanOrEqualTo(0);
        okOffset += 1;

        var errorOffset = text.IndexOf("Error(CustomError", StringComparison.Ordinal);
        errorOffset.ShouldBeGreaterThanOrEqualTo(0);
        errorOffset += 1;

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        foreach (var (offset, expectedName) in new[]
                 {
                     (okOffset, "Ok"),
                     (errorOffset, "Error")
                 })
        {
            var hover = await handler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = PositionHelper.ToRange(sourceText, new TextSpan(offset, 0)).Start
            }, CancellationToken.None);

            hover.ShouldNotBeNull();
            hover.Contents.MarkupContent.ShouldNotBeNull();
            hover.Contents.MarkupContent!.Value.ShouldContain(expectedName);
            hover.Contents.MarkupContent!.Value.ShouldNotContain("```raven\n()\n```");
        }
    }

    [Fact]
    public async Task HoverHandler_RepoEfCoreSample_PipeInvocationTarget_DoesNotReuseLocalHoverAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees");
        var filePath = Path.Combine(projectRoot, "src", "main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var text = File.ReadAllText(filePath);
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
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var sourceText = context.Value.SourceText;
        var queryOffset = text.IndexOf("val query =", StringComparison.Ordinal);
        queryOffset.ShouldBeGreaterThanOrEqualTo(0);
        queryOffset += "val ".Length + 2;

        var whereLineText = "        |> Where(onlyActiveAdults)";
        var whereLineOffset = text.IndexOf(whereLineText, StringComparison.Ordinal);
        whereLineOffset.ShouldBeGreaterThanOrEqualTo(0);
        var whereColumn = whereLineText.IndexOf("Where", StringComparison.Ordinal);
        whereColumn.ShouldBeGreaterThanOrEqualTo(0);

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        var queryHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(queryOffset, 0)).Start
        }, CancellationToken.None);

        queryHover.ShouldNotBeNull();
        queryHover!.Range.ShouldNotBeNull();
        queryHover.Range.Start.Line.ShouldBe(queryHover.Range.End.Line);

        foreach (var delta in new[] { 0, 2, 4 })
        {
            var whereOffset = whereLineOffset + whereColumn + delta;
            var whereHover = await handler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = PositionHelper.ToRange(sourceText, new TextSpan(whereOffset, 0)).Start
            }, CancellationToken.None);

            whereHover.ShouldNotBeNull();
            whereHover.Contents.MarkupContent.ShouldNotBeNull();
            whereHover.Contents.MarkupContent!.Value.ShouldContain("Where");
            whereHover.Contents.MarkupContent!.Value.ShouldNotContain("val query");
            whereHover.Range.ShouldNotBeNull();
            whereHover.Range.Start.Line.ShouldBe(whereHover.Range.End.Line);
        }
    }

    [Fact]
    public async Task HoverHandler_RepoEfCoreSample_LambdaParameter_ShowsLambdaContainingSignatureAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees");
        var filePath = Path.Combine(projectRoot, "src", "main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var text = File.ReadAllText(filePath);
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
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var sourceText = context.Value.SourceText;
        var lambdaOffset = text.IndexOf("OrderBy(user => user.Name)", StringComparison.Ordinal);
        lambdaOffset.ShouldBeGreaterThanOrEqualTo(0);
        lambdaOffset += "OrderBy(".Length + 1;

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(lambdaOffset, 0)).Start
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("user: User");
        hover.Contents.MarkupContent!.Value.ShouldContain("Parameter in `func (user: User) -> string`");
    }

    [Fact]
    public async Task HoverHandler_InlineExtensionLambdaReplay_ResolvesOrchestratedTargetsWithoutBindingFallbackAsync()
    {
        const string text = """
class ServiceCollection {
}

class DbContextOptionsBuilder {
}

class VehicleDbContext {
}

class ConnectionFactory {
    static func GetConnectionString() -> string {
        return "Host=localhost"
    }
}

class Runner {
    func Configure(services: ServiceCollection) -> ServiceCollection {
        services.AddDbContext<VehicleDbContext>(func (options: DbContextOptionsBuilder) {
            options.UseProvider(ConnectionFactory.GetConnectionString())
        })
    }
}

extension ServiceCollectionExtensions for ServiceCollection {
    func AddDbContext<T>(configure: (DbContextOptionsBuilder -> ())?) -> ServiceCollection {
        return ServiceCollection()
    }
}

extension DbContextOptionsBuilderExtensions for DbContextOptionsBuilder {
    func UseProvider(connectionString: string) -> DbContextOptionsBuilder {
        return DbContextOptionsBuilder()
    }
}
""";

        var results = await ReplayInlineHoversAsync(
            text,
            new HoverReplayTarget("AddDbContext", "AddDbContext<", 2, "AddDbContext"),
            new HoverReplayTarget("options", "options.UseProvider", 2, "options: DbContextOptionsBuilder"),
            new HoverReplayTarget("UseProvider", "UseProvider(", 2, "UseProvider"),
            new HoverReplayTarget("ConnectionFactory", "ConnectionFactory.GetConnectionString", 2, "ConnectionFactory"),
            new HoverReplayTarget("UseProvider again", "UseProvider(", 2, "UseProvider"));

        foreach (var result in results)
        {
            result.SemanticDelta.SymbolInfoBinderFallbacks.ShouldBe(0);
            result.SemanticDelta.TypeInfoBoundFallbacks.ShouldBe(0);
        }
    }

    [Fact]
    public async Task HoverHandler_RepoEfCoreSample_ReplaysRecordedHoverPositionsWithoutBindingFallbackAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "efcore-vehicle-costs");
        var filePath = Path.Combine(projectRoot, "src", "Api", "Main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var text = File.ReadAllText(filePath);
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
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var targets = new[]
        {
            new HoverPositionTarget("UseNpgsql", 15, 29, "UseNpgsql"),
            new HoverPositionTarget("Task", 10, 49, "class Task"),
            new HoverPositionTarget("CreateBuilder", 11, 42, "CreateBuilder"),
            new HoverPositionTarget("builder", 11, 16, "builder: WebApplicationBuilder"),
            new HoverPositionTarget("VehicleAppServices", 15, 41, "VehicleAppServices")
        };

        foreach (var target in targets)
        {
            var before = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
            var hover = await handler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = new Position(target.Line, target.Character)
            }, CancellationToken.None);
            var after = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
            var delta = SemanticQueryInstrumentation.Subtract(after, before);

            hover.ShouldNotBeNull();
            hover!.Contents.MarkupContent.ShouldNotBeNull();
            hover.Contents.MarkupContent!.Value.ShouldContain(target.ExpectedText);
            delta.SymbolInfoBinderFallbacks.ShouldBe(0);
            delta.TypeInfoBoundFallbacks.ShouldBe(0);
            delta.BoundNodeBindFallbacks.ShouldBe(0);
        }
    }

    [Fact]
    public async Task GetAnalysisContextAsync_RepoEfCoreSample_MinAgeEdit_DoesNotPoisonDiagnosticsOrHoverAsync()
    {
        var repoRoot = FindRepositoryRoot();
        var projectRoot = Path.Combine(repoRoot, "samples", "projects", "efcore-expression-trees");
        var filePath = Path.Combine(projectRoot, "src", "main.rvn");
        File.Exists(filePath).ShouldBeTrue();

        var originalText = File.ReadAllText(filePath);
        var minAgeMatch = Regex.Match(originalText, @"val minAge = (?<value>\d+)");
        minAgeMatch.Success.ShouldBeTrue();
        var currentMinAge = int.Parse(minAgeMatch.Groups["value"].Value);
        var updatedText = string.Concat(
            originalText.AsSpan(0, minAgeMatch.Index),
            $"val minAge = {currentMinAge + 1}",
            originalText.AsSpan(minAgeMatch.Index + minAgeMatch.Length));
        updatedText.ShouldNotBe(originalText);

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
        _ = store.UpsertDocument(uri, originalText);
        _ = await store.GetAnalysisContextAsync(uri, CancellationToken.None);

        store.UpsertDocument(uri, updatedText);

        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        diagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal)).ShouldBeFalse();
        diagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, "RAV0168", StringComparison.Ordinal)).ShouldBeFalse();

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();
        var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
        var root = context.Value.SyntaxTree.GetRoot();

        var sourceText = context.Value.SourceText;
        var whereLineText = "        |> Where(onlyActiveAdults)";
        var whereLineOffset = updatedText.IndexOf(whereLineText, StringComparison.Ordinal);
        whereLineOffset.ShouldBeGreaterThanOrEqualTo(0);
        var whereColumn = whereLineText.IndexOf("Where", StringComparison.Ordinal);
        whereColumn.ShouldBeGreaterThanOrEqualTo(0);
        var whereOffset = whereLineOffset + whereColumn;
        var whereIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "Where");
        var whereSymbolInfo = semanticModel.GetSymbolInfo(whereIdentifier);
        var whereSymbol = whereSymbolInfo.Symbol ?? whereSymbolInfo.CandidateSymbols.FirstOrDefault();
        var retrySemanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
        var retryWhereSymbolInfo = retrySemanticModel.GetSymbolInfo(whereIdentifier);
        var retryWhereSymbol = retryWhereSymbolInfo.Symbol ?? retryWhereSymbolInfo.CandidateSymbols.FirstOrDefault();
        Assert.True(whereSymbol is IMethodSymbol || retryWhereSymbol is IMethodSymbol);

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);

        foreach (var delta in new[] { 0, 2, 4 })
        {
            whereOffset = whereLineOffset + whereColumn + delta;
            var hover = await handler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = PositionHelper.ToRange(sourceText, new TextSpan(whereOffset, 0)).Start
            }, CancellationToken.None);

            hover.ShouldNotBeNull();
            hover.Contents.MarkupContent.ShouldNotBeNull();
            hover.Contents.MarkupContent!.Value.ShouldContain("Where");
            hover.Contents.MarkupContent!.Value.ShouldNotContain("val query: Error");
        }
    }

    [Fact]
    public async Task HoverHandler_NamedFunctionExpressionDeclaration_UsesSyntaxSignatureWithoutBroadBindingAsync()
    {
        const string text = """
class C {
    func Run() -> int {
        val seed = 1
        val compute = func Step(n: int) -> int {
            if n < 1
                seed
            else
                Step(n - 1)
        }

        compute(3)
    }
}
""";

        var uri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "named-lambda-hover.rav"));
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
        _ = store.UpsertDocument(uri, text);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var sourceText = context.Value.SourceText;
        var offset = text.IndexOf("Step(n: int)", StringComparison.Ordinal);
        offset.ShouldBeGreaterThanOrEqualTo(0);
        offset += 2;

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(offset, 0)).Start
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("func Step(n: int) -> int");
        hover.Contents.MarkupContent!.Value.ShouldContain("Function in `C`");
    }

    private static async Task AssertMemberAccessHoverAsync(
        DocumentStore store,
        HoverHandler handler,
        DocumentUri uri,
        string text,
        int expectedLine)
    {
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var sourceText = context.Value.SourceText;
        var receiverOffset = text.IndexOf("counter.Next()", StringComparison.Ordinal);
        receiverOffset.ShouldBeGreaterThanOrEqualTo(0);
        var targetOffset = receiverOffset + "counter.".Length + 1;

        var receiverHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(receiverOffset + 2, 0)).Start
        }, CancellationToken.None);

        receiverHover.ShouldNotBeNull();
        receiverHover!.Contents.MarkupContent.ShouldNotBeNull();
        receiverHover.Contents.MarkupContent!.Value.ShouldContain("counter");
        receiverHover.Contents.MarkupContent!.Value.ShouldNotContain("func Next");
        receiverHover.Range.ShouldNotBeNull();
        receiverHover.Range.Start.Line.ShouldBe(expectedLine);

        var targetHover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(targetOffset, 0)).Start
        }, CancellationToken.None);

        targetHover.ShouldNotBeNull();
        targetHover!.Contents.MarkupContent.ShouldNotBeNull();
        targetHover.Contents.MarkupContent!.Value.ShouldContain("Next");
        targetHover.Contents.MarkupContent!.Value.ShouldNotContain("val counter");
        targetHover.Range.ShouldNotBeNull();
        targetHover.Range.Start.Line.ShouldBe(expectedLine);
    }

    private static async Task AssertPropertyMemberHoverAsync(
        DocumentStore store,
        HoverHandler handler,
        DocumentUri uri,
        string text)
    {
        var hover = await GetPropertyMemberHoverAsync(store, handler, uri, text);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("val Test: string");
        hover.Contents.MarkupContent!.Value.ShouldContain("Property in `class Foo`");
    }

    private static async Task<Hover?> GetPropertyMemberHoverAsync(
        DocumentStore store,
        HoverHandler handler,
        DocumentUri uri,
        string text)
    {
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var testOffset = text.IndexOf("foo.Test", StringComparison.Ordinal);
        testOffset.ShouldBeGreaterThanOrEqualTo(0);
        testOffset += "foo.".Length + 1;

        return await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(testOffset, 0)).Start
        }, CancellationToken.None);
    }

    private static async Task AssertPipeWhereHoverAsync(
        DocumentStore store,
        HoverHandler handler,
        DocumentUri uri,
        string text,
        int expectedLine)
    {
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var sourceText = context.Value.SourceText;
        var whereOffset = text.IndexOf("|> Where", StringComparison.Ordinal);
        whereOffset.ShouldBeGreaterThanOrEqualTo(0);
        whereOffset += "|> ".Length + 1;

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new TextSpan(whereOffset, 0)).Start
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain("Where");
        hover.Contents.MarkupContent!.Value.ShouldNotContain("val query");
        hover.Range.ShouldNotBeNull();
        hover.Range.Start.Line.ShouldBe(expectedLine);
    }

    private static async Task AssertLocalHoverAsync(
        DocumentStore store,
        HoverHandler handler,
        DocumentUri uri,
        string text,
        string marker,
        string symbolName,
        string expectedSignature)
    {
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var markerOffset = text.IndexOf(marker, StringComparison.Ordinal);
        markerOffset.ShouldBeGreaterThanOrEqualTo(0);
        var symbolOffset = text.IndexOf(symbolName, markerOffset, StringComparison.Ordinal);
        symbolOffset.ShouldBeGreaterThanOrEqualTo(0);

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(symbolOffset + 1, 0)).Start
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain(expectedSignature);
        hover.Range.ShouldNotBeNull();
    }

    private static async Task AssertMethodDeclarationHoverAsync(
        DocumentStore store,
        HoverHandler handler,
        DocumentUri uri,
        string text,
        string marker,
        string expectedSignature)
    {
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var markerOffset = text.IndexOf(marker, StringComparison.Ordinal);
        markerOffset.ShouldBeGreaterThanOrEqualTo(0);

        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(markerOffset + 1, 0)).Start
        }, CancellationToken.None);

        hover.ShouldNotBeNull();
        hover!.Contents.MarkupContent.ShouldNotBeNull();
        hover.Contents.MarkupContent!.Value.ShouldContain(expectedSignature);
        hover.Contents.MarkupContent!.Value.ShouldContain("Method in `class Runner`");
        hover.Range.ShouldNotBeNull();
    }

    private async Task<IReadOnlyList<HoverReplayResult>> ReplayInlineHoversAsync(
        string text,
        params HoverReplayTarget[] targets)
    {
        var (store, _, uri) = CreateWorkspace(text);
        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var results = new List<HoverReplayResult>(targets.Length);

        foreach (var target in targets)
        {
            var targetOffset = IndexOfOccurrence(text, target.SearchText, target.Occurrence);
            targetOffset.ShouldBeGreaterThanOrEqualTo(0);

            var hoverPosition = PositionHelper.ToRange(
                context.Value.SourceText,
                new TextSpan(targetOffset + target.CharacterOffset, 0)).Start;
            var before = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
            var hover = await handler.Handle(new HoverParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = hoverPosition
            }, CancellationToken.None);
            var after = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
            var delta = SemanticQueryInstrumentation.Subtract(after, before);

            hover.ShouldNotBeNull();
            hover!.Contents.MarkupContent.ShouldNotBeNull();
            hover.Contents.MarkupContent!.Value.ShouldContain(target.ExpectedText);

            results.Add(new HoverReplayResult(target.Label, hoverPosition, delta));
        }

        return results;
    }

    private static int IndexOfOccurrence(string text, string searchText, int occurrence)
    {
        var index = -1;
        for (var i = 0; i < occurrence; i++)
        {
            index = text.IndexOf(searchText, index + 1, StringComparison.Ordinal);
            if (index < 0)
                return -1;
        }

        return index;
    }

    private (DocumentStore store, WorkspaceManager manager, DocumentUri uri) CreateWorkspace(string text)
    {
        Directory.CreateDirectory(_tempRoot);

        var projectPath = Path.Combine(_tempRoot, "App.rvnproj");
        File.WriteAllText(projectPath, """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var filePath = Path.Combine(_tempRoot, "src", "main.rvn");
        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);
        File.WriteAllText(filePath, text);

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
        var uri = DocumentUri.FromFileSystemPath(filePath);
        store.UpsertDocument(uri, text);
        return (store, manager, uri);
    }

    private static string FindRepositoryRoot()
    {
        var current = new DirectoryInfo(AppContext.BaseDirectory);
        while (current is not null)
        {
            if (File.Exists(Path.Combine(current.FullName, "Raven.sln")))
                return current.FullName;

            current = current.Parent;
        }

        throw new DirectoryNotFoundException("Could not locate Raven.sln from test base directory.");
    }

    private sealed record HoverReplayTarget(
        string Label,
        string SearchText,
        int CharacterOffset,
        string ExpectedText,
        int Occurrence = 1);

    private sealed record HoverPositionTarget(
        string Label,
        int Line,
        int Character,
        string ExpectedText);

    private sealed record HoverReplayResult(
        string Label,
        Position Position,
        SemanticQueryInstrumentation.Snapshot SemanticDelta);

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
