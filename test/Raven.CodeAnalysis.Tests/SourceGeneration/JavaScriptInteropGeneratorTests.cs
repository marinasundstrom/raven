using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.SourceGeneration;

public class JavaScriptInteropGeneratorTests
{
    [Fact]
    public void Generator_ImplementsStringImportWithStringCallback()
    {
        var compilation = CreateCompilation("""
            import System.*
            import System.Runtime.InteropServices.JavaScript.*

            partial class BrowserInterop {
                [JSImport("setGreeting", "raven")]
                static partial func SetGreeting(
                    message: string,
                    [JSMarshalAs<JSType.Function<JSType.String>>] onRendered: Action<string>
                );
            }
            """);
        var declaration = compilation.SyntaxTrees.Single().GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(declaration.SyntaxTree).GetDeclaredSymbol(declaration)!;

        method.GetAttributes().Select(static attribute => attribute.AttributeClass.ToFullyQualifiedMetadataName()).ShouldContain(
            "System.Runtime.InteropServices.JavaScript.JSImportAttribute");

        var driver = GeneratorDriver.Create(new JavaScriptInteropGenerator())
            .RunGeneratorsAndUpdateCompilation(compilation, out var outputCompilation, out var diagnostics);

        diagnostics.ShouldBeEmpty();
        var generatedSource = driver.GetRunResult().GeneratedSources.Single().SourceText.ToString();
        generatedSource.ShouldContain("JSFunctionBinding.BindJSFunction");
        generatedSource.Split("JSMarshalerType.Discard", StringSplitOptions.None).Length.ShouldBe(2);
        generatedSource.ShouldContain("JSMarshalerType.Action(JSMarshalerType.String)");
        generatedSource.ShouldContain("messageArgument.ToJS(message)");
        generatedSource.ShouldContain("onRenderedArgument.ToJS(onRendered, __ReadString_SetGreeting_onRendered)");
        var errors = outputCompilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();
        errors.ShouldBeEmpty(string.Join(Environment.NewLine, errors.Select(static diagnostic => diagnostic.ToString())));
    }

    [Fact]
    public void Generator_ReportsUnsupportedParameterType()
    {
        var compilation = CreateCompilation("""
            import System.*
            import System.Runtime.InteropServices.JavaScript.*

            partial class BrowserInterop {
                [JSImport("setCount", "raven")]
                static partial func SetCount(count: int);
            }
            """);

        _ = GeneratorDriver.Create(new JavaScriptInteropGenerator())
            .RunGeneratorsAndUpdateCompilation(compilation, out _, out var diagnostics);

        var diagnostic = diagnostics.Single();
        diagnostic.Id.ShouldBe("RVNJS001");
        diagnostic.GetMessage().ShouldContain("parameter 'count'");
    }

    [Fact]
    public void Workspace_RunsJavaScriptInteropGeneratorWithoutExplicitGeneratorReference()
    {
        var workspace = new AdhocWorkspace();
        var projectId = ProjectId.CreateNew(workspace.CurrentSolution.Id);
        var solution = workspace.CurrentSolution
            .AddProject(projectId, "BrowserInteropProject")
            .WithCompilationOptions(projectId, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        foreach (var reference in TestMetadataReferences.Default)
            solution = solution.AddMetadataReference(projectId, reference);

        solution = solution.AddDocument(
            DocumentId.CreateNew(projectId),
            "Main.rvn",
            SourceText.From("""
                import System.*
                import System.Runtime.InteropServices.JavaScript.*

                partial class BrowserInterop {
                    [JSImport("setGreeting", "raven")]
                    static partial func SetGreeting(message: string);
                }
                """));

        workspace.TryApplyChanges(solution).ShouldBeTrue();

        var compilation = workspace.GetCompilation(projectId);

        compilation.SyntaxTrees.ShouldContain(static tree =>
            tree.FilePath.Contains("JavaScriptInterop", StringComparison.Ordinal));
        var errors = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();
        errors.ShouldBeEmpty(string.Join(Environment.NewLine, errors.Select(static diagnostic => diagnostic.ToString())));
    }

    private static Compilation CreateCompilation(string source)
        => Compilation.Create(
                "javascript-interop-generator-test",
                [SyntaxTree.ParseText(source)],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
}
