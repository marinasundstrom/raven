using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class SingleFileWorkspaceCompilationTests
{
    [Fact]
    public void WorkspaceCompilation_MarkedMacroFile_IsAutomaticallyPartitioned()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "macros.rvn",
            SourceText.From(
                """
                import System.Collections.Immutable.*
                import Raven.CodeAnalysis.Macros.*

                [LocalMacroPlugin]
                class LocalMacroPlugin : IRavenMacroPlugin {
                    val Name: string => "Local"

                    func GetMacros() -> ImmutableArray<IMacroDefinition>
                        => [LocalAnswerMacro()]
                }

                class LocalAnswerMacro : ITokenTreeExpressionMacro {
                    val Name: string => "localAnswer"
                    val Kind: MacroKind => MacroKind.FreestandingExpression
                    val Targets: MacroTarget => MacroTarget.None

                    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                        FreestandingMacroExpansionResult {
                            Expression = #quote { 42 }
                        }
                    }
                }
                """),
            "/tmp/macros.rvn").Project;
        project = project.AddDocument(
            "main.rvn",
            SourceText.From("func Main() -> int => #localAnswer { }"),
            "/tmp/main.rvn").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var macroTree = Assert.Single(compilation.MacroSyntaxTrees);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(
            compilation.GetSemanticModel(macroTree).GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void WorkspaceCompilation_SingleTopLevelInterface_DoesNotDuplicateDeclaration()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "test.rav",
            SourceText.From(
                """
                interface IError { }
                """),
            "/tmp/test.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Single(compilation.SyntaxTrees);
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.TypeAlreadyDefined.Id);
    }
}
