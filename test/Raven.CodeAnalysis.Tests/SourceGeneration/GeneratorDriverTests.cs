using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis.Tests.SourceGeneration;

public class GeneratorDriverTests
{
    [Fact]
    public void RunGeneratorsAndUpdateCompilation_AddsGeneratedSyntaxTree()
    {
        var compilation = Compilation.Create("generator-test", [SyntaxTree.ParseText("class Input {}")]);

        var driver = GeneratorDriver.Create(new TestGenerator())
            .RunGeneratorsAndUpdateCompilation(compilation, out var outputCompilation, out var diagnostics);

        diagnostics.ShouldBeEmpty();
        outputCompilation.SyntaxTrees.Length.ShouldBe(2);
        outputCompilation.SyntaxTrees[1].FilePath.ShouldEndWith("Generated.rvn");
        outputCompilation.SyntaxTrees[1].GetText()!.ToString().ShouldContain("class Generated");
        driver.GetRunResult().GeneratedSources.Length.ShouldBe(1);
    }

    [Fact]
    public void RunGeneratorsAndUpdateCompilation_ReportsGeneratorFailures()
    {
        var compilation = Compilation.Create("generator-test");

        _ = GeneratorDriver.Create(new ThrowingGenerator())
            .RunGeneratorsAndUpdateCompilation(compilation, out var outputCompilation, out var diagnostics);

        diagnostics.Single().Id.ShouldBe("RVNGEN001");
        outputCompilation.GetDiagnostics().ShouldContain(diagnostic => diagnostic.Id == "RVNGEN001");
    }

    [Fact]
    public void Workspace_RunsGeneratorsFromGeneratorReferencesBeforeReturningCompilation()
    {
        var workspace = new AdhocWorkspace();
        var projectId = ProjectId.CreateNew(workspace.CurrentSolution.Id);
        var solution = workspace.CurrentSolution
            .AddProject(projectId, "GeneratorProject")
            .AddGeneratorReference(projectId, new GeneratorReference(new TestGenerator()));

        workspace.TryApplyChanges(solution).ShouldBeTrue();

        var compilation = workspace.GetCompilation(projectId);

        compilation.SyntaxTrees.ShouldContain(tree => tree.FilePath.EndsWith("Generated.rvn", StringComparison.Ordinal));
    }

    [Fact]
    public void Workspace_AnalyzersObserveGeneratedSyntaxTrees()
    {
        var workspace = new AdhocWorkspace();
        var projectId = ProjectId.CreateNew(workspace.CurrentSolution.Id);
        var solution = workspace.CurrentSolution
            .AddProject(projectId, "GeneratorProject")
            .AddGeneratorReference(projectId, new GeneratorReference(new TestGenerator()))
            .AddAnalyzerReference(projectId, new AnalyzerReference(new GeneratedTreeAnalyzer()));

        workspace.TryApplyChanges(solution).ShouldBeTrue();

        var diagnostics = workspace.GetDiagnostics(projectId);

        diagnostics.ShouldContain(diagnostic => diagnostic.Id == "TESTGEN001");
    }

    [Fact]
    public void Workspace_ChangingGeneratorReferencesInvalidatesCompilation()
    {
        var workspace = new AdhocWorkspace();
        var projectId = ProjectId.CreateNew(workspace.CurrentSolution.Id);
        var solution = workspace.CurrentSolution
            .AddProject(projectId, "GeneratorProject")
            .AddGeneratorReference(projectId, new GeneratorReference(new NamedGenerator("First")));
        workspace.TryApplyChanges(solution).ShouldBeTrue();

        var firstCompilation = workspace.GetCompilation(projectId);

        solution = workspace.CurrentSolution.WithGeneratorReferences(
            projectId,
            [new GeneratorReference(new NamedGenerator("Second"))]);
        workspace.TryApplyChanges(solution).ShouldBeTrue();
        var secondCompilation = workspace.GetCompilation(projectId);

        ReferenceEquals(firstCompilation, secondCompilation).ShouldBeFalse();
        secondCompilation.SyntaxTrees.ShouldContain(tree => tree.FilePath.EndsWith("Second.rvn", StringComparison.Ordinal));
        secondCompilation.SyntaxTrees.ShouldNotContain(tree => tree.FilePath.EndsWith("First.rvn", StringComparison.Ordinal));
    }

    private sealed class TestGenerator : ISourceGenerator
    {
        public void Initialize(GeneratorInitializationContext context)
        {
        }

        public void Execute(GeneratorExecutionContext context)
        {
            context.AddSource("Generated", "class Generated {}");
        }
    }

    private sealed class ThrowingGenerator : ISourceGenerator
    {
        public void Initialize(GeneratorInitializationContext context)
        {
        }

        public void Execute(GeneratorExecutionContext context)
        {
            throw new InvalidOperationException("boom");
        }
    }

    private sealed class NamedGenerator(string name) : ISourceGenerator
    {
        public void Initialize(GeneratorInitializationContext context)
        {
        }

        public void Execute(GeneratorExecutionContext context)
        {
            context.AddSource(name, $"class {name} {{}}");
        }
    }

    private sealed class GeneratedTreeAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor s_descriptor = DiagnosticDescriptor.Create(
            "TESTGEN001",
            "Generated source observed",
            null,
            string.Empty,
            "The analyzer observed generated source.",
            "Tests",
            DiagnosticSeverity.Info);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterCompilationAction(context =>
            {
                if (context.Compilation.SyntaxTrees.Any(
                    tree => tree.FilePath.EndsWith("Generated.rvn", StringComparison.Ordinal)))
                {
                    context.ReportDiagnostic(Diagnostic.Create(s_descriptor, Location.None));
                }
            });
        }

        public override System.Collections.Immutable.ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => [s_descriptor];
    }
}
