using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
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

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
