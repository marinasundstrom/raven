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
    public async Task GetDiagnosticsAsync_IncludesCompilerInfoDiagnostics()
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
    public async Task GetDiagnosticsAsync_TagsUnusedLocalAsUnnecessary()
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
    public async Task GetDiagnosticsAsync_TagsUnusedLocalFunctionAsUnnecessary()
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

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
