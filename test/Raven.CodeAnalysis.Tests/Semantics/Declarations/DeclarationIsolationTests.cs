using System;
using System.Linq;

using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Semantics.Declarations;

public sealed class DeclarationIsolationTests : CompilationTestBase
{
    [Fact]
    public void BrokenFunctionBody_RetainsSignatureAndDoesNotInvalidateSibling()
    {
        const string source = """
func Broken(value: int) -> int {
    missingValue
}

func Stable(value: int) -> int {
    value * 2
}

func Main() -> int {
    Stable(21)
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var functions = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .ToDictionary(static function => function.Identifier.ValueText);

        var broken = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Broken"]));
        var stable = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Stable"]));
        var stableInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        Assert.Equal("Broken", broken.Name);
        Assert.Equal(SpecialType.System_Int32, broken.ReturnType.SpecialType);
        Assert.Single(broken.Parameters);
        Assert.Equal(SpecialType.System_Int32, broken.Parameters[0].Type.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(
            stable,
            model.GetSymbolInfo(stableInvocation).Symbol));

        AssertErrorsAreConfinedTo(compilation, functions["Broken"].Span);
    }

    [Fact]
    public void EditingOneFunctionBodyToBeBroken_DoesNotInvalidateSiblingResolution()
    {
        const string source = """
func Broken(value: int) -> int {
    value + 1
}

func Stable(value: int) -> int {
    value * 2
}

func Main() -> int {
    Stable(21)
}
""";

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "declaration-isolation",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "declarations.rav",
            SourceText.From(source),
            "/tmp/declarations.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetCompilation(projectId).GetDiagnostics();

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single();
        var updatedSource = source.Replace("value + 1", "missingValue", StringComparison.Ordinal);
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource));
        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single();
        var model = updatedCompilation.GetSemanticModel(updatedTree);
        var functions = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .ToDictionary(static function => function.Identifier.ValueText);
        var stable = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Stable"]));
        var stableInvocation = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        Assert.True(SymbolEqualityComparer.Default.Equals(
            stable,
            model.GetSymbolInfo(stableInvocation).Symbol));

        AssertErrorsAreConfinedTo(updatedCompilation, functions["Broken"].Span);
    }

    private static void AssertErrorsAreConfinedTo(Compilation compilation, TextSpan brokenSpan)
    {
        var errors = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();

        Assert.NotEmpty(errors);
        Assert.All(
            errors,
            diagnostic =>
            {
                Assert.NotNull(diagnostic.Location.SourceTree);
                Assert.True(
                    brokenSpan.Contains(diagnostic.Location.SourceSpan),
                    $"Expected '{diagnostic}' to be confined to the broken declaration.");
            });
    }
}
