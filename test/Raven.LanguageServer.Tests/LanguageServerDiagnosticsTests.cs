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
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        store.UpsertDocument(uri, await File.ReadAllTextAsync(documentPath));

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
    public async Task TryGetDiagnosticsAsync_FullMode_WaitsForDocumentSemanticGateInsteadOfSkippingAsync()
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
            DocumentStore.DocumentDiagnosticsMode.Full,
            shouldSkipWork: null,
            cancellationToken: CancellationToken.None);

        await Task.Delay(100);
        diagnosticsTask.IsCompleted.ShouldBeFalse();

        heldLease.Dispose();

        var result = await diagnosticsTask;
        result.WasSkipped.ShouldBeFalse();
        result.Diagnostics.Any(d => string.Equals(d.Code?.String, "RAV0103", StringComparison.Ordinal)).ShouldBeTrue();
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
