using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

using CodeFixAction = Raven.CodeAnalysis.CodeAction;

namespace Raven.Editor.Tests;

public sealed class WorkspaceManagerTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-{Guid.NewGuid():N}");

    [Fact]
    public void FindWorkspaceProjectFiles_RecursesIntoNestedProjects()
    {
        Directory.CreateDirectory(_tempRoot);
        var rootProjectPath = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <RavenMacro Include="macros/ObservableMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        var nestedProjectPath = WriteProject(Path.Combine(_tempRoot, "macros"), "ObservableMacros", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");

        var projectSystem = new MsBuildProjectSystemService();

        var candidates = WorkspaceManager.FindWorkspaceProjectFiles(_tempRoot, projectSystem);
        var primary = WorkspaceManager.SelectPrimaryProjectPath(_tempRoot, candidates);

        candidates.ShouldContain(rootProjectPath);
        candidates.ShouldContain(nestedProjectPath);
        primary.ShouldBe(rootProjectPath);
    }

    [Fact]
    public void Initialize_OpensNestedProjectsWithoutSolutionFile()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <RavenMacro Include="macros/ObservableMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(_tempRoot, "src", "main.rvn"), """
func Main() -> () { }
""");
        _ = WriteProject(Path.Combine(_tempRoot, "macros"), "ObservableMacros", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(_tempRoot, "macros", "src", "main.rvn"), """
class MacroPlugin { }
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

        manager.GetProjectsSnapshot().Count.ShouldBe(2);
    }

    [Fact]
    public async Task TryGetDocument_ResolvesSiblingProjectDocumentByUriAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        WriteMacroObservableLayout(_tempRoot);

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

        var macroUri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "macros", "main.rvn"));

        manager.TryGetDocument(macroUri, out var document).ShouldBeTrue();
        document.ShouldNotBeNull();
        manager.TryGetCompilation(macroUri, out var compilation).ShouldBeTrue();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();
        Should.NotThrow(() => compilation.GetSemanticModel(syntaxTree!));
    }

    [Fact]
    public async Task RemoveDocument_DoesNotRemoveProjectBackedSiblingProjectDocumentAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        WriteMacroObservableLayout(_tempRoot);

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

        var macroPath = Path.Combine(_tempRoot, "macros", "main.rvn");
        var macroUri = DocumentUri.FromFileSystemPath(macroPath);
        var originalText = File.ReadAllText(macroPath);

        _ = manager.UpsertDocument(macroUri, originalText);
        manager.RemoveDocument(macroUri).ShouldBeTrue();

        manager.TryGetDocument(macroUri, out var document).ShouldBeTrue();
        document.ShouldNotBeNull();
        manager.TryGetCompilation(macroUri, out var compilation).ShouldBeTrue();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();
        Should.NotThrow(() => compilation.GetSemanticModel(syntaxTree!));
    }

    [Fact]
    public void TryGetCodeFixes_StaleOwnedDocumentWithExistingDocumentIdButDeadProjectId_RebindsToCurrentProject()
    {
        Directory.CreateDirectory(_tempRoot);
        var projectPath = WriteProject(_tempRoot, "App", """
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
        WriteRavenFile(filePath, """
val x = 1
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

        var uri = DocumentUri.FromFileSystemPath(filePath);
        _ = manager.UpsertDocument(uri, File.ReadAllText(filePath));

        manager.TryGetDocument(uri, out var originalDocument).ShouldBeTrue();
        originalDocument.ShouldNotBeNull();

        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var newProjectId = manager.GetProjectsSnapshot().Single().Id;
        var staleProjectId = ProjectId.CreateNew(workspace.CurrentSolution.Id);

        var documentsField = typeof(WorkspaceManager).GetField("_documents", BindingFlags.Instance | BindingFlags.NonPublic)!;
        var documents = documentsField.GetValue(manager)!;
        var itemProperty = documents.GetType().GetProperty("Item")!;
        itemProperty.SetValue(
            documents,
            Activator.CreateInstance(
                itemProperty.PropertyType,
                originalDocument!.Id,
                staleProjectId,
                originalDocument.Version,
                true),
            [uri]);

        Should.NotThrow(() => manager.TryGetCodeFixes(uri, out _));
        manager.TryGetDocument(uri, out var reboundDocument).ShouldBeTrue();
        reboundDocument.ShouldNotBeNull();
        reboundDocument.Project.Id.ShouldBe(newProjectId);
    }

    [Fact]
    public async Task SwitchingBetweenSiblingProjectDocuments_DoesNotDetachAppDocumentFromCompilationAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        WriteMacroFreestandingLayout(_tempRoot);

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

        var appPath = Path.Combine(_tempRoot, "app", "src", "main.rvn");
        var macroPath = Path.Combine(_tempRoot, "macros", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);
        var macroUri = DocumentUri.FromFileSystemPath(macroPath);

        _ = manager.UpsertDocument(appUri, File.ReadAllText(appPath));
        _ = manager.UpsertDocument(macroUri, File.ReadAllText(macroPath));
        manager.RemoveDocument(macroUri).ShouldBeTrue();
        _ = manager.UpsertDocument(appUri, File.ReadAllText(appPath));

        manager.TryGetDocument(appUri, out var document).ShouldBeTrue();
        document.ShouldNotBeNull();
        manager.TryGetCompilation(appUri, out var compilation).ShouldBeTrue();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();
        Should.NotThrow(() => compilation.GetSemanticModel(syntaxTree!));
    }

    [Fact]
    public async Task TryGetDocumentContext_ReturnsMatchingDocumentAndCompilationAfterSiblingSwitchAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        WriteMacroFreestandingLayout(_tempRoot);

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
        var appPath = Path.Combine(_tempRoot, "app", "src", "main.rvn");
        var macroPath = Path.Combine(_tempRoot, "macros", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);
        var macroUri = DocumentUri.FromFileSystemPath(macroPath);

        _ = store.UpsertDocument(appUri, File.ReadAllText(appPath));
        _ = store.UpsertDocument(macroUri, File.ReadAllText(macroPath));
        store.RemoveDocument(macroUri).ShouldBeTrue();

        store.TryGetDocumentContext(appUri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();
        Should.NotThrow(() => compilation.GetSemanticModel(syntaxTree!));
    }

    [Fact]
    public async Task Initialize_ProjectOpenFailureInSiblingProject_DoesNotPreventDiagnosticsForHealthyProjectAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var appProjectPath = WriteProject(Path.Combine(_tempRoot, "app"), "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(_tempRoot, "app", "src", "main.rvn"), """
func Main() -> unit {
    WriteLine(test)
}
""");

        _ = WriteProject(Path.Combine(_tempRoot, "broken"), "Broken", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(_tempRoot, "broken", "src", "main.rvn"), """
func Main() -> unit { }
""");

        var projectSystem = new ThrowingProjectSystemService(
            new RavenProjectSystemService(),
            failingProjectPath: Path.Combine(_tempRoot, "broken", "Broken.rvnproj"));
        var workspace = RavenWorkspace.Create(targetFramework: "net10.0", projectSystemService: projectSystem);
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
        var appUri = DocumentUri.FromFileSystemPath(Path.Combine(_tempRoot, "app", "src", "main.rvn"));
        _ = store.UpsertDocument(appUri, File.ReadAllText(Path.Combine(_tempRoot, "app", "src", "main.rvn")));

        var diagnostics = await store.GetDiagnosticsAsync(appUri, CancellationToken.None);
        diagnostics.Any(d => d.Code?.String == "RAV0103").ShouldBeTrue();
    }

    [Fact]
    public async Task UpdatingMacroProjectDocument_RefreshesConsumingProjectMacroExpansionAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        WriteFreestandingMacroExpansionLayout(_tempRoot, "1");

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

        var appPath = Path.Combine(_tempRoot, "app", "src", "main.rvn");
        var macroPath = Path.Combine(_tempRoot, "macros", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);
        var macroUri = DocumentUri.FromFileSystemPath(macroPath);

        _ = manager.UpsertDocument(appUri, File.ReadAllText(appPath));
        _ = manager.UpsertDocument(macroUri, File.ReadAllText(macroPath));

        manager.TryGetDocumentContext(appUri, out var initialDocument, out _).ShouldBeTrue();
        var initialMacroReferenceInfo = initialDocument!.Project.MacroReferences.Single();
        initialMacroReferenceInfo.SourceProjectFilePath.ShouldBe(Path.Combine(_tempRoot, "macros", "FreestandingMacros.rvnproj"));
        var initialMacroReference = initialMacroReferenceInfo.Display;

        var initialExpansion = await GetFreestandingMacroExpansionTextAsync(manager, appUri);
        initialExpansion.ShouldBe("1");

        var updatedMacroSource = CreateFreestandingMacroExpansionSource("2");
        _ = manager.UpsertDocument(macroUri, updatedMacroSource);

        manager.TryGetDocumentContext(macroUri, out var refreshedMacroDocument, out _).ShouldBeTrue();
        refreshedMacroDocument!.Project.FilePath.ShouldBe(Path.Combine(_tempRoot, "macros", "FreestandingMacros.rvnproj"));

        manager.TryGetDocumentContext(appUri, out var refreshedDocument, out _).ShouldBeTrue();
        var refreshedMacroReference = refreshedDocument!.Project.MacroReferences.Single().Display;
        refreshedMacroReference.ShouldNotBe(initialMacroReference);

        var refreshedExpansion = await GetFreestandingMacroExpansionTextAsync(manager, appUri);
        refreshedExpansion.ShouldBe("2");
    }

    [Fact]
    public async Task WatchedMacroProjectDocumentChange_RefreshesConsumingProjectMacroExpansionAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        WriteFreestandingMacroExpansionLayout(_tempRoot, "1");

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

        var appPath = Path.Combine(_tempRoot, "app", "src", "main.rvn");
        var macroPath = Path.Combine(_tempRoot, "macros", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);

        _ = manager.UpsertDocument(appUri, File.ReadAllText(appPath));

        var initialExpansion = await GetFreestandingMacroExpansionTextAsync(manager, appUri);
        initialExpansion.ShouldBe("1");

        File.WriteAllText(macroPath, CreateFreestandingMacroExpansionSource("2"));
        manager.ReloadForWatchedFiles([
            new FileEvent
            {
                Uri = DocumentUri.FromFileSystemPath(macroPath),
                Type = FileChangeType.Changed
            }
        ]);

        var refreshedExpansion = await GetFreestandingMacroExpansionTextAsync(manager, appUri);
        refreshedExpansion.ShouldBe("2");
    }

    [Fact]
    public async Task Initialize_SampleMacroObservableRoot_ResolvesMacrosForAppDocumentAsync()
    {
        var repoRoot = GetRepositoryRoot();
        var sampleRoot = Path.Combine(repoRoot, "samples", "projects", "macro-observable");
        var appPath = Path.Combine(sampleRoot, "app", "src", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "macro-observable",
                Uri = DocumentUri.FromFileSystemPath(sampleRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(appUri, File.ReadAllText(appPath));

        var diagnostics = await store.GetDiagnosticsAsync(appUri, CancellationToken.None);
        diagnostics.Any(diagnostic => diagnostic.Code?.String == "RAVM010").ShouldBeFalse();

        store.TryGetDocumentContext(appUri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();

        var semanticModel = compilation.GetSemanticModel(syntaxTree!);
        var attribute = syntaxTree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        var expansion = semanticModel.GetMacroExpansion(attribute);

        expansion.ShouldNotBeNull();
        expansion!.ReplacementDeclaration.ShouldNotBeNull();
        expansion.IntroducedMembers.Length.ShouldBeGreaterThan(0);
    }

    [Fact]
    public async Task Initialize_SampleMacroReactiveRoot_ResolvesMacrosForAppDocumentAsync()
    {
        var repoRoot = GetRepositoryRoot();
        var sampleRoot = Path.Combine(repoRoot, "samples", "projects", "macro-reactive");
        var appPath = Path.Combine(sampleRoot, "app", "src", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "macro-reactive",
                Uri = DocumentUri.FromFileSystemPath(sampleRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(appUri, File.ReadAllText(appPath));

        var diagnostics = await store.GetDiagnosticsAsync(appUri, CancellationToken.None);
        diagnostics.Any(diagnostic => diagnostic.Code?.String == "RAVM010").ShouldBeFalse();

        store.TryGetDocumentContext(appUri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();

        var semanticModel = compilation.GetSemanticModel(syntaxTree!);
        var root = syntaxTree.GetRoot();
        var attribute = root.DescendantNodes()
            .OfType<AttributeSyntax>()
            .Single(candidate => candidate.Name.ToString() == "Observable");
        var freestanding = root.DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();

        semanticModel.GetMacroExpansion(attribute).ShouldNotBeNull();
        semanticModel.GetMacroExpansion(freestanding).ShouldNotBeNull();
    }

    [Fact]
    public async Task Initialize_RepositoryRoot_ResolvesMacrosForMacroReactiveSampleAsync()
    {
        var repoRoot = GetRepositoryRoot();
        var appPath = Path.Combine(repoRoot, "samples", "projects", "macro-reactive", "app", "src", "main.rvn");
        var appUri = DocumentUri.FromFileSystemPath(appPath);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "Raven",
                Uri = DocumentUri.FromFileSystemPath(repoRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(appUri, File.ReadAllText(appPath));

        var diagnostics = await store.GetDiagnosticsAsync(appUri, CancellationToken.None);
        diagnostics.Any(diagnostic => diagnostic.Code?.String == "RAVM010").ShouldBeFalse();

        store.TryGetDocumentContext(appUri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();

        var semanticModel = compilation.GetSemanticModel(syntaxTree!);
        var root = syntaxTree.GetRoot();
        var attribute = root.DescendantNodes()
            .OfType<AttributeSyntax>()
            .Single(candidate => candidate.Name.ToString() == "Observable");
        var freestanding = root.DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();

        semanticModel.GetMacroExpansion(attribute).ShouldNotBeNull();
        semanticModel.GetMacroExpansion(freestanding).ShouldNotBeNull();
    }

    [Fact]
    public void TryGetRefactorings_ReturnsContextActionsForOpenDocument()
    {
        Directory.CreateDirectory(_tempRoot);
        var filePath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(filePath);
        File.WriteAllText(filePath, "func Main() -> () { }");

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(
            workspace,
            NullLogger<WorkspaceManager>.Instance,
            [],
            [new TestRefactoringProvider()]);

        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        _ = manager.UpsertDocument(uri, File.ReadAllText(filePath));

        manager.TryGetRefactorings(uri, new TextSpan(0, 4), out var refactorings).ShouldBeTrue();
        refactorings.Length.ShouldBe(1);
        refactorings[0].Action.Title.ShouldBe("Test refactoring");
    }

    [Fact]
    public async Task TopLevelExeProject_DocumentContext_BindsWithoutMainFunctionAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "AspNetMinimalApi", """
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
        WriteRavenFile(filePath, """
val first = args.Length
val second = first + 1

record Data(val Value: int)
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

        var uri = DocumentUri.FromFileSystemPath(filePath);
        _ = manager.UpsertDocument(uri, File.ReadAllText(filePath));

        manager.TryGetDocumentContext(uri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();

        var semanticModel = compilation.GetSemanticModel(syntaxTree!);
        var diagnostics = compilation.GetDiagnostics();
        diagnostics.Any(diagnostic => diagnostic.Descriptor.Id is "RAV1012" or "RAV1014").ShouldBeFalse();

        var root = syntaxTree!.GetRoot();
        var argsIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.ValueText == "args");
        var argsSymbol = semanticModel.GetSymbolInfo(argsIdentifier).Symbol;

        argsSymbol.ShouldNotBeNull();
        argsSymbol.Name.ShouldBe("args");
    }

    [Fact]
    public async Task TopLevelExeProject_MainToTopLevelTransition_UpdatesCompilationAndOutlineAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "AspNetMinimalApi", """
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
        WriteRavenFile(filePath, """
func Main() -> int {
    func Parse() -> int => 1
    Parse()
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
        _ = store.UpsertDocument(uri, File.ReadAllText(filePath));

        var transitionedText = """
val first = args.Length

if first >= 0 {
    func Parse() -> int => first
    Parse()
}

record Data(val Value: int)
""";

        _ = store.UpsertDocument(uri, transitionedText);

        store.TryGetDocumentContext(uri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();

        var diagnostics = await store.GetDiagnosticsAsync(uri, CancellationToken.None);
        diagnostics.Any(diagnostic => diagnostic.Code?.String is "RAV1012" or "RAV1014").ShouldBeFalse();

        var semanticModel = compilation.GetSemanticModel(syntaxTree!);
        var root = syntaxTree!.GetRoot();
        var argsIdentifier = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.ValueText == "args");
        semanticModel.GetSymbolInfo(argsIdentifier).Symbol.ShouldNotBeNull();

        var buildMemberSymbols = typeof(DocumentSymbolHandler)
            .GetMethod("BuildMemberSymbols", BindingFlags.NonPublic | BindingFlags.Static)!;
        var text = await document.GetTextAsync();
        var symbols = ((IEnumerable<DocumentSymbol>)buildMemberSymbols.Invoke(null, [root.Members, text])!)
            .ToArray();

        symbols.Any(symbol => symbol.Name == "Main").ShouldBeFalse();
        var topLevelCode = symbols.Single(symbol => symbol.Name == "<top-level code>");
        topLevelCode.Children.ShouldNotBeNull();
        topLevelCode.Children.Any(symbol => symbol.Name == "Parse").ShouldBeTrue();
        symbols.Single(symbol => symbol.Name == "Data").Kind.ShouldBe(OmniSharp.Extensions.LanguageServer.Protocol.Models.SymbolKind.Struct);
    }

    [Fact]
    public void TryGetCodeFixes_StaleOwnedDocument_RebindsInsteadOfThrowing()
    {
        Directory.CreateDirectory(_tempRoot);
        _ = WriteProject(_tempRoot, "App", """
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
        WriteRavenFile(filePath, """
func Main() -> unit {
    val text: string? = null
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

        var uri = DocumentUri.FromFileSystemPath(filePath);
        _ = manager.UpsertDocument(uri, File.ReadAllText(filePath));

        var documentsField = typeof(WorkspaceManager).GetField("_documents", BindingFlags.Instance | BindingFlags.NonPublic)!;
        var documents = documentsField.GetValue(manager)!;
        var tryGetValue = documents.GetType().GetMethod("TryGetValue")!;
        var tryGetArgs = new object?[] { uri, null };
        ((bool)tryGetValue.Invoke(documents, tryGetArgs)!).ShouldBeTrue();
        var staleOwnedDocument = tryGetArgs[1];

        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        documents = documentsField.GetValue(manager)!;
        var itemProperty = documents.GetType().GetProperty("Item")!;
        itemProperty.SetValue(documents, staleOwnedDocument, [uri]);

        Should.NotThrow(() => manager.TryGetCodeFixes(uri, out _));
    }

    [Fact]
    public void MacroShadowOutputDirectory_DoesNotChangeAcrossProjectVersions()
    {
        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var projectId = workspace.AddProject("Macros", targetFramework: "net10.0");
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var getShadowMacroOutputDirectory = typeof(WorkspaceManager)
            .GetMethod("GetShadowMacroOutputDirectory", BindingFlags.Static | BindingFlags.NonPublic)!;

        var firstPath = (string)getShadowMacroOutputDirectory.Invoke(null, [project])!;

        var documentId = DocumentId.CreateNew(projectId);
        var updatedSolution = workspace.CurrentSolution.AddDocument(
            documentId,
            "macro.rvn",
            SourceText.From("func Main() -> unit { }"),
            Path.Combine(_tempRoot, "macro.rvn"));
        workspace.TryApplyChanges(updatedSolution);
        var updatedProject = workspace.CurrentSolution.GetProject(projectId)!;

        var secondPath = (string)getShadowMacroOutputDirectory.Invoke(null, [updatedProject])!;

        secondPath.ShouldBe(firstPath);
    }

    [Fact]
    public void MacroShadowOutputPath_ChangesWhenContentHashChanges()
    {
        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var projectId = workspace.AddProject("Macros", targetFramework: "net10.0");
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var getShadowMacroOutputPath = typeof(WorkspaceManager)
            .GetMethod("GetShadowMacroOutputPath", BindingFlags.Static | BindingFlags.NonPublic)!;

        var firstPath = (string)getShadowMacroOutputPath.Invoke(null, [project, "Macros", "aaaaaaaaaaaaaaaa"])!;
        var secondPath = (string)getShadowMacroOutputPath.Invoke(null, [project, "Macros", "bbbbbbbbbbbbbbbb"])!;

        firstPath.ShouldNotBe(secondPath);
        Path.GetDirectoryName(firstPath).ShouldBe(Path.GetDirectoryName(secondPath));
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

    private static void WriteRavenFile(string path, string contents)
    {
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, contents);
    }

    private static string GetRepositoryRoot()
        => Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));

    private static void WriteMacroObservableLayout(string root)
    {
        var ravenCodeAnalysisPath = typeof(RavenWorkspace).Assembly.Location;

        _ = WriteProject(Path.Combine(root, "app"), "MacroObservable", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>MacroObservable</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <RavenMacro Include="../macros/ObservableMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(root, "app", "src", "main.rvn"), """
func Main() -> () { }
""");

        _ = WriteProject(Path.Combine(root, "macros"), "ObservableMacros", $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>ObservableMacros</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(root, "macros", "main.rvn"), """
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*

class ObservableMacroPlugin: IRavenMacroPlugin {
    val Name: string => "SampleMacros.Observable"

    func GetMacros() -> ImmutableArray<IMacroDefinition> {
        [ObservableMacro()]
    }
}

class ObservableMacro: IAttachedDeclarationMacro {
    val Name: string => "Observable"
    val Kind: MacroKind => MacroKind.AttachedDeclaration
    val Targets: MacroTarget => MacroTarget.Property

    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
        MacroExpansionResult.Empty
    }
}
""");
    }

    private static void WriteMacroFreestandingLayout(string root)
    {
        var ravenCodeAnalysisPath = typeof(RavenWorkspace).Assembly.Location;

        _ = WriteProject(Path.Combine(root, "app"), "MacroFreestanding", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>MacroFreestanding</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <RavenMacro Include="../macros/FreestandingMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(root, "app", "src", "main.rvn"), """
func Main() -> int => #answer()
""");

        _ = WriteProject(Path.Combine(root, "macros"), "FreestandingMacros", $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>FreestandingMacros</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(root, "macros", "main.rvn"), """
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*

class FreestandingMacroPlugin: IRavenMacroPlugin {
    val Name: string => "SampleMacros.Answer"

    func GetMacros() -> ImmutableArray<IMacroDefinition> {
        [AnswerMacro()]
    }
}

class AnswerMacro: IFreestandingExpressionMacro {
    val Name: string => "answer"
    val Kind: MacroKind => MacroKind.FreestandingExpression
    val Targets: MacroTarget => MacroTarget.None

    func Expand(context: FreestandingMacroContext) -> FreestandingMacroExpansionResult {
        FreestandingMacroExpansionResult.Empty
    }
}
""");
    }

    private static void WriteFreestandingMacroExpansionLayout(string root, string expansionText)
    {
        var ravenCodeAnalysisPath = typeof(RavenWorkspace).Assembly.Location;

        _ = WriteProject(Path.Combine(root, "app"), "MacroFreestanding", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>MacroFreestanding</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <RavenMacro Include="../macros/FreestandingMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(root, "app", "src", "main.rvn"), """
func Main() -> int => #answer()
""");

        _ = WriteProject(Path.Combine(root, "macros"), "FreestandingMacros", $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>FreestandingMacros</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        WriteRavenFile(Path.Combine(root, "macros", "main.rvn"), CreateFreestandingMacroExpansionSource(expansionText));
    }

    private static string CreateFreestandingMacroExpansionSource(string expansionText)
    {
        return $$"""
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

class FreestandingMacroPlugin: IRavenMacroPlugin {
    val Name: string => "SampleMacros.Answer"

    func GetMacros() -> ImmutableArray<IMacroDefinition> {
        [AnswerMacro()]
    }
}

class AnswerMacro: IFreestandingExpressionMacro {
    val Name: string => "answer"
    val Kind: MacroKind => MacroKind.FreestandingExpression
    val Targets: MacroTarget => MacroTarget.None

    func Expand(context: FreestandingMacroContext) -> FreestandingMacroExpansionResult {
        FreestandingMacroExpansionResult {
            Expression = ParseExpression("{{expansionText}}")
        }
    }
}
""";
    }

    private static async Task<string?> GetFreestandingMacroExpansionTextAsync(WorkspaceManager manager, DocumentUri appUri)
    {
        manager.TryGetDocumentContext(appUri, out var document, out var compilation).ShouldBeTrue();
        document.ShouldNotBeNull();
        compilation.ShouldNotBeNull();

        var syntaxTree = await document.GetSyntaxTreeAsync();
        syntaxTree.ShouldNotBeNull();

        var semanticModel = compilation.GetSemanticModel(syntaxTree!);
        var expression = syntaxTree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = semanticModel.GetMacroExpansion(expression);
        return expansion?.Expression?.ToFullString().Trim();
    }

    private sealed class TestRefactoringProvider : CodeRefactoringProvider
    {
        public override void RegisterRefactorings(CodeRefactoringContext context)
        {
            context.RegisterRefactoring(CodeFixAction.Create("Test refactoring", static (solution, _) => solution));
        }
    }

    private sealed class ThrowingProjectSystemService : IProjectSystemService
    {
        private readonly IProjectSystemService _inner;
        private readonly string _failingProjectPath;

        public ThrowingProjectSystemService(IProjectSystemService inner, string failingProjectPath)
        {
            _inner = inner;
            _failingProjectPath = Path.GetFullPath(failingProjectPath);
        }

        public bool CanOpenProject(string projectFilePath)
            => _inner.CanOpenProject(projectFilePath);

        public IReadOnlyList<string> GetProjectReferencePaths(string projectFilePath)
            => _inner.GetProjectReferencePaths(projectFilePath);

        public ProjectId OpenProject(Workspace workspace, string projectFilePath)
        {
            if (string.Equals(Path.GetFullPath(projectFilePath), _failingProjectPath, StringComparison.OrdinalIgnoreCase))
                throw new InvalidOperationException("Synthetic project open failure.");

            return _inner.OpenProject(workspace, projectFilePath);
        }

        public void SaveProject(Project project, string filePath)
            => _inner.SaveProject(project, filePath);
    }
}
