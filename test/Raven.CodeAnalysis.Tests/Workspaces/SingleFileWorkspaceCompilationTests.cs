using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class SingleFileWorkspaceCompilationTests
{
    [Fact]
    public void WorkspaceCompilation_MacroFunctionEditsInvalidateOnlyTheLocalMacroPartition()
    {
        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                performanceInstrumentation: instrumentation),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.DefaultWithRavenMacros)
            project = project.AddMetadataReference(reference);

        var document = project.AddDocument(
            "main.rvn",
            SourceText.From(CreateMixedMacroFunctionSource(42, 0)),
            "/tmp/main.rvn");
        workspace.TryApplyChanges(document.Project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        AssertNoErrors(initialCompilation);
        Assert.Equal("42", GetLocalAnswerExpansion(initialCompilation));
        Assert.Equal(1, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(0, instrumentation.Macros.LocalPartitionReuses);

        workspace.TryApplyChanges(
            workspace.CurrentSolution.WithDocumentText(
                document.Id,
                SourceText.From(CreateMixedMacroFunctionSource(42, 1))));

        var consumerCompilation = workspace.GetCompilation(projectId);
        AssertNoErrors(consumerCompilation);
        Assert.Equal("42", GetLocalAnswerExpansion(consumerCompilation));
        Assert.Equal(1, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(1, instrumentation.Macros.LocalPartitionReuses);

        workspace.TryApplyChanges(
            workspace.CurrentSolution.WithDocumentText(
                document.Id,
                SourceText.From(CreateMixedMacroFunctionSource(43, 1))));

        var macroCompilation = workspace.GetCompilation(projectId);
        AssertNoErrors(macroCompilation);
        Assert.Equal("43", GetLocalAnswerExpansion(macroCompilation));
        Assert.Equal(2, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(1, instrumentation.Macros.LocalPartitionReuses);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesLocalMacroArtifactUntilMacroSourceChanges()
    {
        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                performanceInstrumentation: instrumentation),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.DefaultWithRavenMacros)
            project = project.AddMetadataReference(reference);

        var document = project.AddDocument(
            "main.rvn",
            SourceText.From(CreateMixedLocalAnswerMacroSource(42, 0)),
            "/tmp/main.rvn");
        project = document.Project;
        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        AssertNoErrors(initialCompilation);
        Assert.Equal("42", GetLocalAnswerExpansion(initialCompilation));
        Assert.Equal(1, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(0, instrumentation.Macros.LocalPartitionReuses);

        var consumerEdit = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(CreateMixedLocalAnswerMacroSource(42, 1)));
        workspace.TryApplyChanges(consumerEdit);

        var consumerCompilation = workspace.GetCompilation(projectId);
        AssertNoErrors(consumerCompilation);
        Assert.Equal("42", GetLocalAnswerExpansion(consumerCompilation));
        Assert.Equal(1, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(1, instrumentation.Macros.LocalPartitionReuses);

        var macroEdit = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(CreateMixedLocalAnswerMacroSource(43, 1)));
        workspace.TryApplyChanges(macroEdit);

        var macroCompilation = workspace.GetCompilation(projectId);
        AssertNoErrors(macroCompilation);
        Assert.Equal("43", GetLocalAnswerExpansion(macroCompilation));
        Assert.Equal(2, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(1, instrumentation.Macros.LocalPartitionReuses);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void WorkspaceCompilation_BrokenMacroBodyRetainsDeclarationsAndValidSibling(bool diagnosticsFirst)
    {
        const string source = """
            import Raven.CodeAnalysis.Syntax.*

            macro func Inspect(expression: ExpressionSyntax) {
                expand expression
            }

            macro func Answer() {
                expand SyntaxFactory.ParseExpression("42")
            }

            func Main() -> int {
                let inspected = Inspect!(40)
                inspected + Answer!()
            }
            """;
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "macro-declaration-isolation",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.DefaultWithRavenMacros)
            project = project.AddMetadataReference(reference);

        var document = project.AddDocument("main.rvn", SourceText.From(source), "/tmp/main.rvn");
        workspace.TryApplyChanges(document.Project.Solution);
        AssertNoErrors(workspace.GetCompilation(projectId));

        var brokenSource = source.Replace(
            "expand expression",
            "match expression {\n    }\n    expand expression",
            StringComparison.Ordinal);
        workspace.TryApplyChanges(
            workspace.CurrentSolution.WithDocumentText(document.Id, SourceText.From(brokenSource)));

        var compilation = workspace.GetCompilation(projectId);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocations = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .ToArray();
        var inspectInvocation = Assert.Single(
            invocations,
            expression => expression.Name.ToString() == "Inspect");
        var answerInvocation = Assert.Single(
            invocations,
            expression => expression.Name.ToString() == "Answer");

        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var model = compilation.GetSemanticModel(consumerTree);
        var inspectSymbol = model.GetSymbolInfo(inspectInvocation).Symbol;
        var answerExpansion = model.GetMacroExpansion(answerInvocation);
        var diagnostics = compilation.GetDiagnostics();

        Assert.IsAssignableFrom<IMacroFunctionSymbol>(inspectSymbol);
        Assert.Equal("Inspect", inspectSymbol.Name);
        Assert.Equal("42", answerExpansion!.Expression!.ToString());
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAV2100");
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM010");
    }

    [Fact]
    public void WorkspaceCompilation_RemapsReusedLocalMacroDiagnosticsToCurrentProjection()
    {
        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                performanceInstrumentation: instrumentation),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.DefaultWithRavenMacros)
            project = project.AddMetadataReference(reference);

        var initialSource = CreateMixedLocalAnswerMacroSource(42, 0)
            .Replace("val Name: string", "val Name: int", StringComparison.Ordinal);
        var document = project.AddDocument(
            "main.rvn",
            SourceText.From(initialSource),
            "/tmp/main.rvn");
        workspace.TryApplyChanges(document.Project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialMacroTree = Assert.Single(initialCompilation.MacroSyntaxTrees);
        Assert.Contains(
            initialCompilation.GetDiagnostics(),
            diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, initialMacroTree));
        Assert.Equal(1, instrumentation.Macros.LocalPartitionCompilations);

        var editedSource = CreateMixedLocalAnswerMacroSource(42, 1)
            .Replace("val Name: string", "val Name: int", StringComparison.Ordinal);
        workspace.TryApplyChanges(
            workspace.CurrentSolution.WithDocumentText(document.Id, SourceText.From(editedSource)));

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedMacroTree = Assert.Single(updatedCompilation.MacroSyntaxTrees);
        var updatedMacroDiagnostics = updatedCompilation.GetDiagnostics()
            .Where(diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, updatedMacroTree))
            .ToArray();

        Assert.NotEmpty(updatedMacroDiagnostics);
        Assert.DoesNotContain(
            updatedCompilation.GetDiagnostics(),
            diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, initialMacroTree));
        Assert.Equal(1, instrumentation.Macros.LocalPartitionCompilations);
        Assert.Equal(1, instrumentation.Macros.LocalPartitionReuses);
    }

    [Fact]
    public void WorkspaceCompilation_MarkedMacroFile_IsAutomaticallyPartitioned()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.DefaultWithRavenMacros)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "macros.rvn",
            SourceText.From(
                """
                import Raven.CodeAnalysis.Macros.*
                import Raven.Macros.*

                class LocalAnswerMacro : ITokenTreeExpressionMacro {
                    val Name: string => "localAnswer"
                    val Kind: MacroKind => MacroKind.FreestandingExpression

                    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                        FreestandingMacroExpansionResult {
                            Expression = quote! { 42 }
                        }
                    }
                }
                """),
            "/tmp/macros.rvn").Project;
        project = project.AddDocument(
            "main.rvn",
            SourceText.From("func Main() -> int => localAnswer! { }"),
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

    private static string CreateMixedLocalAnswerMacroSource(int answer, int addend)
        => $$"""
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            class LocalAnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.FreestandingExpression

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = quote! { {{answer}} }
                    }
                }
            }

            func Main() -> int => localAnswer! { } + {{addend}}
            """;

    private static string CreateMixedMacroFunctionSource(int answer, int addend)
        => $$"""
            macro func Answer() {
                let answer = {{answer}}
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(answer.ToString())
            }

            func Main() -> int => Answer!() + {{addend}}
            """;

    private static string GetLocalAnswerExpansion(Compilation compilation)
    {
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        return compilation.GetSemanticModel(consumerTree)
            .GetMacroExpansion(invocation)!
            .Expression!
            .ToString();
    }

    private static void AssertNoErrors(Compilation compilation)
        => Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
}
