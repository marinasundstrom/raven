using Microsoft.Extensions.Logging.Abstractions;
using System.Reflection;

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

    private sealed class TestRefactoringProvider : CodeRefactoringProvider
    {
        public override void RegisterRefactorings(CodeRefactoringContext context)
        {
            context.RegisterRefactoring(CodeFixAction.Create("Test refactoring", static (solution, _) => solution));
        }
    }
}
