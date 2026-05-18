using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

using CodeDiagnostic = Raven.CodeAnalysis.Diagnostic;
using CodeDiagnosticSeverity = Raven.CodeAnalysis.DiagnosticSeverity;
using CodeLocation = Raven.CodeAnalysis.Location;
using CodeTextSpan = Raven.CodeAnalysis.Text.TextSpan;
using LspDiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity;

namespace Raven.Editor.Tests;

public sealed class LanguageServerDiagnosticsTests : IDisposable
{
    private const string ThisValueIsNotMutableDiagnosticId = "RAV0027";
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-diags-{Guid.NewGuid():N}");

    [Fact]
    public void ShouldReport_MatchesEquivalentSyntaxTreesByPath()
    {
        const string path = "/workspace/test.rvn";
        const string code = """
class Flag {
    static func explicit(value: Flag) -> bool { return true }
}

func Main() -> () {
    val flag = Flag()
    if flag {
    }
}
""";

        var diagnosticTree = SyntaxTree.ParseText(code, path: path);
        var documentTree = SyntaxTree.ParseText(code, path: path);
        var location = CodeLocation.Create(diagnosticTree, new CodeTextSpan(code.IndexOf("flag {", StringComparison.Ordinal), 4));
        var descriptor = DiagnosticDescriptor.Create(
            "RAVTEST001",
            "Compiler info diagnostic",
            null,
            string.Empty,
            "Compiler info diagnostic",
            "compiler",
            CodeDiagnosticSeverity.Info);
        var diagnostic = CodeDiagnostic.Create(descriptor, location);

        DocumentStore.ShouldReport(diagnostic, documentTree).ShouldBeTrue();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_IncludesCompilerInfoDiagnosticsAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
class Flag {
    static func explicit(value: Flag) -> bool { return true }
}

func Main() -> () {
    val flag = Flag()
    if flag {
    }
}
""";

        store.UpsertDocument(uri, code);
        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        diagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, "RAV1507", StringComparison.Ordinal)).ShouldBeTrue();
        diagnostics.ShouldContain(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Information);
    }

    [Fact]
    public async Task GetDiagnosticsAsync_TagsUnusedLocalAsUnnecessaryAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
    }
}
""";

        store.UpsertDocument(uri, code);
        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        var diagnostic = diagnostics.Single(d => string.Equals(
            d.Code?.String,
            Raven.CodeAnalysis.Diagnostics.UnusedVariableAnalyzer.DiagnosticId,
            StringComparison.Ordinal));

        diagnostic.Tags.ShouldNotBeNull();
        diagnostic.Tags!.ShouldContain(DiagnosticTag.Unnecessary);
        diagnostic.Severity.ShouldBe(LspDiagnosticSeverity.Warning);
    }

    [Fact]
    public async Task GetDiagnosticsAsync_TagsUnusedLocalFunctionAsUnnecessaryAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
val c = C()
c.M()

class C {
    func M() -> () {
        func Helper() -> () { }
    }
}
""";

        store.UpsertDocument(uri, code);
        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        diagnostics.Count(d => string.Equals(
            d.Code?.String,
            Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.DiagnosticId,
            StringComparison.Ordinal)).ShouldBe(1);

        var diagnostic = diagnostics.Single(d =>
            string.Equals(d.Code?.String, Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.DiagnosticId, StringComparison.Ordinal));

        diagnostic.Tags.ShouldNotBeNull();
        diagnostic.Tags!.ShouldContain(DiagnosticTag.Unnecessary);
        diagnostic.Severity.ShouldBe(LspDiagnosticSeverity.Warning);
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_ProjectWithAnalyzersLane_IncludesBuiltInAnalyzerDiagnosticsAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Main() -> unit {
    let count = 0
    count
}
""";

        store.UpsertDocument(uri, code);
        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.ProjectWithAnalyzers,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.Any(diagnostic => string.Equals(
            diagnostic.Code?.String,
            Raven.CodeAnalysis.Diagnostics.PreferValInsteadOfLetAnalyzer.PreferValInsteadOfLetDiagnosticId,
            StringComparison.Ordinal)).ShouldBeTrue();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_DocumentCompilerLane_AfterInlayHints_DoesNotPublishStalePipeLocalDiagnosticAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var inlayHandler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*
import System.Linq.Expressions.*

class User(var Name: string, var Age: int, var IsActive: bool)

func Main(users: IQueryable<User>) {
    val minAge = 21
    val onlyActiveAdults: Expression<System.Func<User, bool>> =
        user => user.IsActive && user.Age >= minAge

    val query = users
        |> Where(onlyActiveAdults)
        |> OrderBy(user => user.Name)
        |> Select(user => user.Name)
}
""";

        store.UpsertDocument(uri, code);
        var beforeInlayResult = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);
        var beforeInlayErrors = beforeInlayResult.Diagnostics
            .Where(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error)
            .Select(diagnostic => $"{diagnostic.Code?.String}: {diagnostic.Message}")
            .ToArray();
        beforeInlayErrors.ShouldBeEmpty(string.Join(Environment.NewLine, beforeInlayErrors));

        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        _ = await inlayHandler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new CodeTextSpan(0, sourceText.Length))
        }, CancellationToken.None);

        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.ShouldNotContain(diagnostic =>
            diagnostic.Severity == LspDiagnosticSeverity.Error &&
            diagnostic.Message.Contains("'onlyActiveAdults' is not in scope", StringComparison.Ordinal));
        var errorDiagnostics = result.Diagnostics
            .Where(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error)
            .Select(diagnostic => $"{diagnostic.Code?.String}: {diagnostic.Message}")
            .ToArray();
        errorDiagnostics.ShouldBeEmpty(string.Join(Environment.NewLine, errorDiagnostics));
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_DocumentCompilerLane_AfterInlayHints_DoesNotPublishStaleAsyncLambdaBodyDiagnosticsAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var inlayHandler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
import System.*
import System.Threading.Tasks.*

class RequestContext {
    public val Text: string = "body"
}

func Main() -> unit {
    Accept(async func (context: RequestContext) {
        val content = await Task.FromResult(context.Text)
        return "submitted: $content"
    })
}

func Accept(handler: func (RequestContext) -> Task<string>) -> unit { }
""";

        store.UpsertDocument(uri, code);
        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        _ = await inlayHandler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new CodeTextSpan(0, sourceText.Length))
        }, CancellationToken.None);

        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.ShouldNotContain(diagnostic =>
            diagnostic.Severity == LspDiagnosticSeverity.Error &&
            diagnostic.Message.Contains("'context' is not in scope", StringComparison.Ordinal));
        result.Diagnostics.Any(diagnostic =>
            diagnostic.Severity == LspDiagnosticSeverity.Error &&
            (string.Equals(diagnostic.Code?.String, "RAV2700", StringComparison.Ordinal) ||
             string.Equals(diagnostic.Code?.String, "RAV1900", StringComparison.Ordinal) ||
             string.Equals(diagnostic.Code?.String, "RAV2705", StringComparison.Ordinal))).ShouldBeFalse();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_DocumentCompilerLane_ReportsTypedDeconstructionMismatchOnAnnotationSpanAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
import System.Collections.Immutable.*

val doubled: ImmutableDictionary<string, int> = ["two": 4]

for val (key: string, value: string) in doubled {
    _ = key
    _ = value
}
""";

        store.UpsertDocument(uri, code);
        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeFalse();
        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        var valueTypeStart = code.IndexOf("value: string", StringComparison.Ordinal) + "value: ".Length;
        var expectedRange = PositionHelper.ToRange(sourceText, new CodeTextSpan(valueTypeStart, "string".Length));

        result.Diagnostics.Any(diagnostic =>
            string.Equals(diagnostic.Code?.String, "RAV1503", StringComparison.Ordinal) &&
            diagnostic.Range.Start == expectedRange.Start &&
            diagnostic.Range.End == expectedRange.End).ShouldBeTrue();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_TopLevelGenericWhereClauseSelfConstraint_DoesNotReportTypeParameterOutOfScopeAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
import System.*
import System.Console.*

func Main() -> () {
    val r = Parse<int>("42")
    WriteLine(r)
}

func Parse<T>(str: string) -> T
    where T: IParsable<T>
    => T.Parse(str, null)
""";

        store.UpsertDocument(uri, code);
        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        diagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal)).ShouldBeFalse();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_ProjectBackedNamespaceMembers_AfterHover_DoesNotReportMissingImportedMemberAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var sourceDirectory = Path.Combine(_tempRoot, "src");
        Directory.CreateDirectory(sourceDirectory);
        File.WriteAllText(Path.Combine(sourceDirectory, "members.rvn"), """
namespace Utilities

public const Answer: int = 41

public func AddOne(value: int) -> int => value + 1
""");

        var documentPath = Path.Combine(sourceDirectory, "main.rvn");
        const string code = """
import Utilities.*

func Run() -> int {
    return AddOne(Answer)
}
""";
        File.WriteAllText(documentPath, code);

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
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        store.UpsertDocument(uri, code);

        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        var addOneOffset = code.IndexOf("AddOne", StringComparison.Ordinal);
        var hover = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new CodeTextSpan(addOneOffset + 1, 0)).Start
        }, CancellationToken.None);
        hover.ShouldNotBeNull();

        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        diagnostics
            .Where(diagnostic => diagnostic.Severity == LspDiagnosticSeverity.Error)
            .Select(diagnostic => $"{diagnostic.Code?.String}: {diagnostic.Message}")
            .ToArray()
            .ShouldBeEmpty();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_ProjectBackedNamespaceMemberEdit_InvalidatesDependentDocumentAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var sourceDirectory = Path.Combine(_tempRoot, "src");
        Directory.CreateDirectory(sourceDirectory);
        var membersPath = Path.Combine(sourceDirectory, "members.rvn");
        const string initialMembers = """
namespace Utilities

public const Prefix: string = "ready"
""";
        File.WriteAllText(membersPath, initialMembers);

        var documentPath = Path.Combine(sourceDirectory, "main.rvn");
        const string code = """
import Utilities.*

func Run() -> string {
    return Format()
}
""";
        File.WriteAllText(documentPath, code);

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
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        var membersUri = DocumentUri.FromFileSystemPath(membersPath);
        store.UpsertDocument(uri, code);

        var initialResult = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);
        initialResult.Diagnostics.Any(diagnostic =>
            string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
            diagnostic.Message.Contains("Format", StringComparison.Ordinal)).ShouldBeTrue();

        store.UpsertDocument(membersUri, """
namespace Utilities

public const Prefix: string = "ready"

public func Format() -> string => Prefix
""");

        var updatedResult = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);
        updatedResult.WasSkipped.ShouldBeFalse();
        updatedResult.Diagnostics.Any(diagnostic =>
            string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
            diagnostic.Message.Contains("Format", StringComparison.Ordinal)).ShouldBeFalse();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_DocumentCompilerLane_TopLevelFileprivateConstAfterHover_ReportsInaccessibleAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var sourceDirectory = Path.Combine(_tempRoot, "src");
        Directory.CreateDirectory(sourceDirectory);
        File.WriteAllText(Path.Combine(sourceDirectory, "members.rvn"), """
namespace Hidden

fileprivate const Secret: int = 1

public func SameFile() -> int {
    return Secret
}
""");

        var documentPath = Path.Combine(sourceDirectory, "main.rvn");
        const string code = """
namespace Hidden

public func OtherFile() -> int {
    return Secret
}
""";
        File.WriteAllText(documentPath, code);

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
        var handler = new HoverHandler(store, NullLogger<HoverHandler>.Instance);
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        store.UpsertDocument(uri, code);

        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        var secretOffset = code.IndexOf("Secret", StringComparison.Ordinal);
        _ = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new CodeTextSpan(secretOffset + 1, 0)).Start
        }, CancellationToken.None);

        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.Any(diagnostic =>
            string.Equals(diagnostic.Code?.String, "RAV0500", StringComparison.Ordinal)).ShouldBeTrue();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_ProjectBackedDocument_DoesNotReportNamespaceSegmentOutOfScopeForImportedFrameworkNamespacesAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
import System.*
import System.Console.*
import System.Linq.*
import System.Collections.Generic.*

func Main() {
    val test = MyResult(List<string>())
}

union MyResult<T>(List<T> | int)
""";

        store.UpsertDocument(uri, code);
        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        diagnostics.Any(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                diagnostic.Message.Contains("Linq", StringComparison.Ordinal))
            .ShouldBeFalse();
        diagnostics.Any(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                diagnostic.Message.Contains("Generic", StringComparison.Ordinal))
            .ShouldBeFalse();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_AspNetMinimalApiSample_DoesNotReportBogusOutOfScopeDiagnosticsAsync()
    {
        var sampleRoot = Path.Combine(
            GetRepositoryRoot(),
            "samples",
            "projects",
            "aspnet-minimal-api");
        var documentPath = Path.Combine(sampleRoot, "src", "main.rvn");

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
        var inlayHandler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        var code = await File.ReadAllTextAsync(documentPath);
        store.UpsertDocument(uri, code);

        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        _ = await inlayHandler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new CodeTextSpan(0, sourceText.Length))
        }, CancellationToken.None);

        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        foreach (var name in new[]
                 {
                     "Generic",
                     "AspNetCore",
                     "Builder",
                     "Http",
                     "Mvc",
                     "Tasks",
                     "encoding",
                     "detectEncodingFromByteOrderMarks",
                     "bufferSize",
                     "leaveOpen"
                 })
        {
            diagnostics.Any(diagnostic =>
                    string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                    diagnostic.Message.Contains(name, StringComparison.Ordinal))
                .ShouldBeFalse();
        }

        diagnostics.Any(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                diagnostic.Message.Contains("'context' is not in scope", StringComparison.Ordinal))
            .ShouldBeFalse();
        diagnostics.Any(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV2700", StringComparison.Ordinal) ||
                string.Equals(diagnostic.Code?.String, "RAV1900", StringComparison.Ordinal))
            .ShouldBeFalse();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_TestCaseSample_AfterInlayHints_DoesNotReportSelfShadowingDiagnosticsAsync()
    {
        var sampleRoot = Path.Combine(
            GetRepositoryRoot(),
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
        var inlayHandler = new InlayHintHandler(store, NullLogger<InlayHintHandler>.Instance);
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        var code = await File.ReadAllTextAsync(documentPath);
        store.UpsertDocument(uri, code);

        var sourceText = Raven.CodeAnalysis.Text.SourceText.From(code);
        _ = await inlayHandler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Range = PositionHelper.ToRange(sourceText, new CodeTextSpan(0, sourceText.Length))
        }, CancellationToken.None);

        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.DocumentCompiler,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeFalse();
        var shadowDiagnostics = result.Diagnostics
            .Where(diagnostic => string.Equals(diagnostic.Code?.String, "RAV0168", StringComparison.Ordinal))
            .Select(diagnostic => $"{diagnostic.Message} {diagnostic.Range.Start.Line + 1}:{diagnostic.Range.Start.Character + 1}")
            .ToArray();
        shadowDiagnostics.ShouldBeEmpty();

        var missingPatternLocalDiagnostics = result.Diagnostics
            .Where(diagnostic =>
                string.Equals(diagnostic.Code?.String, "RAV0103", StringComparison.Ordinal) &&
                (diagnostic.Message.Contains("'entry' is not in scope", StringComparison.Ordinal) ||
                 diagnostic.Message.Contains("'id' is not in scope", StringComparison.Ordinal) ||
                 diagnostic.Message.Contains("'name' is not in scope", StringComparison.Ordinal)))
            .Select(diagnostic => $"{diagnostic.Message} {diagnostic.Range.Start.Line + 1}:{diagnostic.Range.Start.Character + 1}")
            .ToArray();
        missingPatternLocalDiagnostics.ShouldBeEmpty();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_ProjectBackedDocument_VarValToggle_RecomputesReadOnlyAssignmentDiagnosticAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);

        var mutableSource = """
import System.Collections.Immutable.*

var people = [
    Person("Alice", 25, [])
]

people = people.Add(Person("Test", 30, []))

record Person(
    val Name: string
    val Age: int
    val Items: string[]
)
""";

        var readOnlySource = mutableSource.Replace("var people", "val people", StringComparison.Ordinal);

        store.UpsertDocument(uri, mutableSource);
        var mutableDiagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        mutableDiagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, ThisValueIsNotMutableDiagnosticId, StringComparison.Ordinal)).ShouldBeFalse();

        store.UpsertDocument(uri, readOnlySource);
        var readOnlyDiagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        readOnlyDiagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, ThisValueIsNotMutableDiagnosticId, StringComparison.Ordinal)).ShouldBeTrue();

        store.UpsertDocument(uri, mutableSource);
        var mutableAgainDiagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        mutableAgainDiagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, ThisValueIsNotMutableDiagnosticId, StringComparison.Ordinal)).ShouldBeFalse();

        store.UpsertDocument(uri, readOnlySource);
        var readOnlyAgainDiagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        readOnlyAgainDiagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, ThisValueIsNotMutableDiagnosticId, StringComparison.Ordinal)).ShouldBeTrue();
    }

    [Fact]
    public async Task GetDiagnosticsAsync_ProjectBackedDocument_RecoversAfterTransientParseFailureAndReportsReadOnlyAssignmentAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
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
        var documentPath = Path.Combine(_tempRoot, "src", "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);

        var invalidSource = """
val flag = true and
""";

        store.UpsertDocument(uri, invalidSource);
        var invalidDiagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        invalidDiagnostics.ShouldNotBeNull();

        var readOnlySource = """
import System.Collections.Immutable.*

val people = [
    Person("Alice", 25, [])
]

people = people.Add(Person("Test", 30, []))

record Person(
    val Name: string
    val Age: int
    val Items: string[]
)
""";

        store.UpsertDocument(uri, readOnlySource);
        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);

        diagnostics.Any(diagnostic => string.Equals(diagnostic.Code?.String, ThisValueIsNotMutableDiagnosticId, StringComparison.Ordinal)).ShouldBeTrue();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_ProjectWithAnalyzersLane_DoesNotRequireDocumentSemanticGateAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Main() -> () {
    fb
}
""";

        store.UpsertDocument(uri, code);

        using var heldLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");
        var diagnosticsTask = store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.ProjectWithAnalyzers,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        var completedTask = await Task.WhenAny(diagnosticsTask, Task.Delay(1000));
        completedTask.ShouldBe(diagnosticsTask);

        var result = await diagnosticsTask;
        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.ShouldNotBeEmpty();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_ProjectWithAnalyzersLane_SkipsWhenCompilerGateIsBusyAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Main() -> () {
    fb
}
""";

        store.UpsertDocument(uri, code);

        using var heldLease = await store.EnterCompilerAccessAsync(CancellationToken.None, "test", uri);
        var result = await store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.ProjectWithAnalyzers,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        result.WasSkipped.ShouldBeTrue();
        result.Diagnostics.ShouldBeEmpty();
    }

    [Fact]
    public async Task TryGetDiagnosticsAsync_SyntaxLane_DoesNotRequireCompilerGateAsync()
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
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Main( -> () {
}
""";

        store.UpsertDocument(uri, code);

        using var heldLease = await store.EnterCompilerAccessAsync(CancellationToken.None, "test", uri);
        var diagnosticsTask = store.TryGetDiagnosticsAsync(
            uri,
            DocumentStore.DiagnosticLane.Syntax,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        var completedTask = await Task.WhenAny(diagnosticsTask, Task.Delay(1000));
        completedTask.ShouldBe(diagnosticsTask);

        var result = await diagnosticsTask;
        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.ShouldNotBeEmpty();
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }

    private static string WriteProject(string directory, string name, string contents)
    {
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, $"{name}.rvnproj");
        File.WriteAllText(path, contents);
        return path;
    }

    private static string GetRepositoryRoot()
        => Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
}
