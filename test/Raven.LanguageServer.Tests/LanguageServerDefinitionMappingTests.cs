using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

using Microsoft.Extensions.Logging.Abstractions;

namespace Raven.LanguageServer.Tests;

public class LanguageServerDefinitionMappingTests
{
    [Fact]
    public void BuildLocationLinks_ForMethodSymbol_ProvidesOriginAndTargetRanges()
    {
        const string code = """
class Counter {
    func Increment() -> unit { }
    func Run() -> unit {
        self.Increment()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var invocation = root.DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var methodSymbol = semanticModel.GetSymbolInfo(invocation).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        var links = DefinitionLocationMapper
            .BuildLocationLinks(methodSymbol, syntaxTree.GetText(), invocation.Expression.Span)
            .ToArray();

        links.Length.ShouldBeGreaterThan(0);

        var first = links[0];
        first.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath("/workspace/test.rav"));
        first.OriginSelectionRange.Start.Line.ShouldBe(3);
        first.TargetSelectionRange.Start.Line.ShouldBe(1);
    }

    [Fact]
    public void MacroTokenDefinition_UsesOrdinaryRavenSymbolTarget()
    {
        const string code = """
import Raven.LanguageServer.Tests.*

class Greeting { }

func Main() {
    let value = symbolToken! { Greeting }
}
""";
        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create(
                "test",
                [syntaxTree],
                [.. LanguageServerTestReferences.Default],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddMacroReferences(new MacroReference(new SymbolTokenMacro()));
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var offset = code.LastIndexOf("Greeting", StringComparison.Ordinal) + 1;

        var tokenInfo = DefinitionHandler.TryResolveMacroTokenDefinition(
            semanticModel,
            root,
            offset,
            CancellationToken.None);
        var links = DefinitionLocationMapper.BuildLocationLinks(
                tokenInfo!.Symbol!,
                syntaxTree.GetText(),
                tokenInfo.Span)
            .ToArray();

        var link = Assert.Single(links);
        link.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath("/workspace/test.rav"));
        link.TargetSelectionRange.Start.Line.ShouldBe(2);
        link.OriginSelectionRange.Start.Line.ShouldBe(5);
    }

    [Fact]
    public async Task DefinitionHandler_ProjectReferenceSymbol_ResolvesToSiblingProjectSource()
    {
        using var fixture = new DefinitionWorkspaceFixture();

        fixture.WriteProject(Path.Combine(fixture.Root, "lib"), "Lib", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
""");
        fixture.WriteRavenFile(Path.Combine(fixture.Root, "lib", "src", "Greeter.rvn"), """
public class Greeter {
    public func Say() -> string => "Hi"
}
""");

        fixture.WriteProject(Path.Combine(fixture.Root, "app"), "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="src/**/*.rvn" />
    <ProjectReference Include="../lib/Lib.rvnproj" />
  </ItemGroup>
</Project>
""");
        var appPath = Path.Combine(fixture.Root, "app", "src", "main.rvn");
        fixture.WriteRavenFile(appPath, """
func Main() -> () {
    let greeter = Greeter()
    greeter.Say()
}
""");

        var result = await fixture.GetDefinitionAsync(appPath, "Say");

        result.ShouldNotBeNull();
        var links = result!.ToArray();
        links.ShouldNotBeEmpty();
        links[0].LocationLink.ShouldNotBeNull();
        links[0].LocationLink!.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath(Path.Combine(fixture.Root, "lib", "src", "Greeter.rvn")));
    }

    [Fact]
    public async Task DefinitionHandler_MacroAttribute_ResolvesToMacroDeclarationProject()
    {
        using var fixture = new DefinitionWorkspaceFixture();
        var ravenCodeAnalysisPath = typeof(RavenWorkspace).Assembly.Location;

        fixture.WriteProject(Path.Combine(fixture.Root, "macros"), "ObservableMacros", $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>ObservableMacros</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        var macroPath = Path.Combine(fixture.Root, "macros", "main.rvn");
        fixture.WriteRavenFile(macroPath, """
import Raven.CodeAnalysis.Macros.*

[assembly: RavenCompilerPlugin(typeof(ObservableMacro))]

class ObservableMacro : IAttachedDeclarationMacro {
    val Name: string => "Observable"
    val Targets: MacroTarget => MacroTarget.Property

    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
        MacroExpansionResult.Empty
    }
}
""");

        fixture.WriteProject(Path.Combine(fixture.Root, "app"), "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="src/**/*.rvn" />
    <ProjectReference Include="../macros/ObservableMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        var appPath = Path.Combine(fixture.Root, "app", "src", "main.rvn");
        fixture.WriteRavenFile(appPath, """
class MyViewModel {
    #[Observable]
    var Title: string = ""
}
""");

        var result = await fixture.GetDefinitionAsync(appPath, "Observable");

        result.ShouldNotBeNull();
        var links = result!.ToArray();
        links.ShouldNotBeEmpty();
        links[0].LocationLink.ShouldNotBeNull();
        links[0].LocationLink!.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath(macroPath));
    }

    [Fact]
    public async Task DefinitionHandler_InvocableMacro_ResolvesToMacroDeclarationProject()
    {
        using var fixture = new DefinitionWorkspaceFixture();
        var ravenCodeAnalysisPath = typeof(RavenWorkspace).Assembly.Location;

        fixture.WriteProject(Path.Combine(fixture.Root, "macros"), "AnswerMacros", $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>AnswerMacros</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        var macroPath = Path.Combine(fixture.Root, "macros", "main.rvn");
        fixture.WriteRavenFile(macroPath, """
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*

[assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

class AnswerMacro : IInvocableMacro {
    val Name: string => "answer"

    func Expand(context: FreestandingMacroContext) -> FreestandingMacroExpansionResult {
        FreestandingMacroExpansionResult.Empty
    }
}
""");

        fixture.WriteProject(Path.Combine(fixture.Root, "app"), "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="src/**/*.rvn" />
    <ProjectReference Include="../macros/AnswerMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        var appPath = Path.Combine(fixture.Root, "app", "src", "main.rvn");
        fixture.WriteRavenFile(appPath, """
func Main() -> int => answer!()
""");

        var result = await fixture.GetDefinitionAsync(appPath, "answer");

        result.ShouldNotBeNull();
        var links = result!.ToArray();
        links.ShouldNotBeEmpty();
        links[0].LocationLink.ShouldNotBeNull();
        links[0].LocationLink!.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath(macroPath));
    }

    [Fact]
    public async Task DefinitionHandler_MacroFragmentMember_UsesOrdinaryRavenDefinition()
    {
        using var fixture = new DefinitionWorkspaceFixture();
        var ravenCodeAnalysisPath = typeof(RavenWorkspace).Assembly.Location;

        fixture.WriteProject(Path.Combine(fixture.Root, "macros"), "FragmentMacros", $$"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>FragmentMacros</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="main.rvn" />
    <Reference Include="Raven.CodeAnalysis">
      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
""");
        fixture.WriteRavenFile(Path.Combine(fixture.Root, "macros", "main.rvn"), """
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Text.*

[assembly: RavenCompilerPlugin(typeof(FragmentMacro))]

class FragmentMacro : ITokenTreeMacro, IMacroFragmentProvider {
    val Name: string => "fragment"

    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
        FreestandingMacroExpansionResult.Empty
    }

    func GetFragmentRegions(context: TokenTreeMacroContext) -> ImmutableArray<MacroFragmentRegion> {
        [context.CreateFragmentRegion(
            MacroFragmentKind.Expression,
            TextSpan(0, context.BodySpan.Length))]
    }
}
""");

        fixture.WriteProject(Path.Combine(fixture.Root, "app"), "App", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="src/**/*.rvn" />
    <ProjectReference Include="../macros/FragmentMacros.rvnproj" />
  </ItemGroup>
</Project>
""");
        var appPath = Path.Combine(fixture.Root, "app", "src", "main.rvn");
        fixture.WriteRavenFile(appPath, """
class Customer {
    val Name: string => "Ada"
}

func Main() {
    let customer = Customer()
    let result = fragment! { customer.Name }
}
""");

        var result = await fixture.GetDefinitionAsync(appPath, "Name", useLastOccurrence: true);

        result.ShouldNotBeNull();
        var link = result!.Single().LocationLink;
        link.ShouldNotBeNull();
        link!.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath(appPath));
        link.TargetSelectionRange.Start.Line.ShouldBe(1);
    }

    private sealed class DefinitionWorkspaceFixture : IDisposable
    {
        public string Root { get; } = Path.Combine(Path.GetTempPath(), $"raven-def-ls-{Guid.NewGuid():N}");

        public DefinitionWorkspaceFixture()
        {
            Directory.CreateDirectory(Root);
        }

        public string WriteProject(string directory, string name, string contents)
        {
            Directory.CreateDirectory(directory);
            var path = Path.Combine(directory, $"{name}.rvnproj");
            File.WriteAllText(path, contents);
            return path;
        }

        public void WriteRavenFile(string path, string contents)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, contents);
        }

        public async Task<LocationOrLocationLinks?> GetDefinitionAsync(
            string filePath,
            string marker,
            bool useLastOccurrence = false)
        {
            var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(Root)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var handler = new DefinitionHandler(store, NullLogger<DefinitionHandler>.Instance);
            var uri = DocumentUri.FromFileSystemPath(filePath);
            var text = await File.ReadAllTextAsync(filePath);
            var offset = useLastOccurrence
                ? text.LastIndexOf(marker, StringComparison.Ordinal)
                : text.IndexOf(marker, StringComparison.Ordinal);
            offset.ShouldBeGreaterThanOrEqualTo(0);
            await store.UpsertDocumentAsync(uri, text);
            var sourceText = Raven.CodeAnalysis.Text.SourceText.From(text);

            return await handler.Handle(new DefinitionParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Position = PositionHelper.ToRange(sourceText, new TextSpan(offset, 0)).Start
            }, CancellationToken.None);
        }
        public void Dispose()
        {
            if (Directory.Exists(Root))
                Directory.Delete(Root, recursive: true);
        }
    }

    private sealed class SymbolTokenMacro : ITokenTreeMacro, IMacroTokenSymbolProvider
    {
        public string Name => "symbolToken";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ISymbol? GetTokenSymbol(TokenTreeMacroContext context, SyntaxToken token)
            => context.Compilation.GetTypeByMetadataName(token.ValueText);
    }
}
