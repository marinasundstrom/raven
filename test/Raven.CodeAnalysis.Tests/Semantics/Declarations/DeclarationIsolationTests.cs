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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void BrokenGenericFunctionBody_DoesNotInvalidateConstructedSibling(bool diagnosticsFirst)
    {
        const string source = """
func Broken<T>(value: T) -> T {
    missingValue
}

func Stable<T>(value: T) -> T {
    value
}

func Main() -> int {
    Stable<int>(21)
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var functions = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .ToDictionary(static function => function.Identifier.ValueText);
        var stable = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Stable"]));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(stable, selected.ConstructedFrom));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(selected.TypeArguments).SpecialType);
        AssertErrorsAreConfinedTo(compilation, functions["Broken"].Span);
    }

    [Fact]
    public void BrokenGenericConstraintClause_DoesNotInvalidateSiblingLookup()
    {
        const string source = """
func Broken<T>(value: T) -> T
    where Missing: struct {
    value
}

func Stable<T>(value: T) -> T {
    value
}

func Main() -> int {
    Stable<int>(21)
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
        var stable = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Stable"]));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(stable, selected.ConstructedFrom));
        AssertErrorsAreConfinedTo(compilation, functions["Broken"].Span);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingGenericConstraintTypeToBeMissing_PreservesSiblingLookup(bool diagnosticsFirst)
    {
        const string source = """
func Broken<T>(value: T) -> T
    where T: struct {
    value
}

func Stable<T>(value: T) -> T {
    value
}

func Main() -> int {
    Stable<int>(21)
}
""";
        var (workspace, projectId, documentId) = CreateWorkspace(source, "generic-constraint-recovery");
        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            documentId,
            SourceText.From(source.Replace("where T: struct", "where T:", StringComparison.Ordinal))));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single();
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var model = compilation.GetSemanticModel(tree);
        var functions = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .ToDictionary(static function => function.Identifier.ValueText);
        var broken = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Broken"]));
        var stable = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(functions["Stable"]));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.True(broken.IsGenericMethod);
        Assert.True(SymbolEqualityComparer.Default.Equals(stable, selected.ConstructedFrom));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(selected.TypeArguments).SpecialType);
        AssertErrorsAreConfinedTo(compilation, functions["Broken"].Span);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingGenericTypeMemberBody_DoesNotInvalidateConstructedSibling(bool diagnosticsFirst)
    {
        const string source = """
class Container<T> {
    static func Broken<U>(value: U) -> T {
        default(T)
    }

    static func Stable<U>(value: U) -> U {
        value
    }
}

func Main() -> int {
    Container<string>.Stable<int>(21)
}
""";

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "generic-member-declaration-isolation",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "declarations.rav",
            SourceText.From(source),
            "/tmp/generic-member-declarations.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single();
        var updatedSource = source.Replace("default(T)", "missingValue", StringComparison.Ordinal);
        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource)));

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single();
        if (diagnosticsFirst)
            _ = updatedCompilation.GetDiagnostics();

        var model = updatedCompilation.GetSemanticModel(updatedTree);
        var methods = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .ToDictionary(static method => method.Identifier.ValueText);
        var stable = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methods["Stable"]));
        var invocation = updatedTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(stable, selected.OriginalDefinition));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(selected.TypeArguments).SpecialType);
        Assert.Equal(SpecialType.System_String, selected.ContainingType!.TypeArguments[0].SpecialType);
        AssertErrorsAreConfinedTo(updatedCompilation, methods["Broken"].Span);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingAccessorBody_DoesNotInvalidateSiblingMethod(bool diagnosticsFirst)
    {
        const string source = """
class Container {
    val Broken: int {
        get => 42
    }

    func Stable(value: int) -> int {
        value
    }
}

func Main() -> int {
    Container().Stable(21)
}
""";
        var (workspace, projectId, documentId) = CreateWorkspace(source, "accessor-isolation");
        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            documentId,
            SourceText.From(source.Replace("get => 42", "get => missingValue", StringComparison.Ordinal))));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single();
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var stable = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression.ToString().EndsWith(".Stable", StringComparison.Ordinal));
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));
        var stableSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(stable));

        Assert.NotNull(propertySymbol.GetMethod);
        Assert.Equal(SpecialType.System_Int32, propertySymbol.Type.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(stableSymbol, model.GetSymbolInfo(invocation).Symbol));
        AssertErrorsAreConfinedTo(compilation, property.Span);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingConstructorBody_DoesNotInvalidateSiblingMethod(bool diagnosticsFirst)
    {
        const string source = """
class Container {
    init(value: int) {
        System.Console.WriteLine(value)
    }

    func Stable(value: int) -> int {
        value
    }
}

func Main() -> int {
    Container(1).Stable(21)
}
""";
        var (workspace, projectId, documentId) = CreateWorkspace(source, "constructor-isolation");
        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            documentId,
            SourceText.From(source.Replace("System.Console.WriteLine(value)", "missingValue", StringComparison.Ordinal))));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single();
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var constructor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var stable = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var stableInvocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression.ToString().EndsWith(".Stable", StringComparison.Ordinal));
        var constructorSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(constructor));
        var stableSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(stable));

        Assert.Equal(MethodKind.Constructor, constructorSymbol.MethodKind);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(constructorSymbol.Parameters).Type.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(stableSymbol, model.GetSymbolInfo(stableInvocation).Symbol));
        AssertErrorsAreConfinedTo(compilation, constructor.Span);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingFieldInitializer_DoesNotInvalidateSiblingMethod(bool diagnosticsFirst)
    {
        const string source = """
            class Container {
                private field broken: int = 42

                func Stable(value: int) -> int {
                    value
                }
            }

            func Main() -> int {
                Container().Stable(21)
            }
            """;
        var (workspace, projectId, documentId) = CreateWorkspace(source, "field-initializer-isolation");
        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            documentId,
            SourceText.From(source.Replace("= 42", "= missingValue", StringComparison.Ordinal))));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single();
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var field = tree.GetRoot().DescendantNodes().OfType<FieldDeclarationSyntax>().Single();
        var fieldDeclarator = Assert.Single(field.Declaration.Declarators);
        var stable = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var stableInvocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression.ToString().EndsWith(".Stable", StringComparison.Ordinal));
        var fieldSymbol = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(fieldDeclarator));
        var stableSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(stable));

        Assert.Equal(SpecialType.System_Int32, fieldSymbol.Type.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(stableSymbol, model.GetSymbolInfo(stableInvocation).Symbol));
        AssertErrorsAreConfinedTo(compilation, field.Span);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingGenericFunctionAcrossFiles_DoesNotInvalidateStableDocument(bool diagnosticsFirst)
    {
        const string brokenSource = """
            func Broken<T>(value: T) -> T {
                value
            }
            """;
        const string stableSource = """
            func Stable<T>(value: T) -> T {
                value
            }

            func Main() -> int {
                Stable<int>(21)
            }
            """;
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "cross-file-declaration-isolation",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        var brokenDocument = project.AddDocument(
            "broken.rav",
            SourceText.From(brokenSource),
            "/tmp/broken.rav");
        project = brokenDocument.Project.AddDocument(
            "stable.rav",
            SourceText.From(stableSource),
            "/tmp/stable.rav").Project;
        workspace.TryApplyChanges(project.Solution);
        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            brokenDocument.Id,
            SourceText.From(brokenSource.Replace("    value\n", "    missingValue\n", StringComparison.Ordinal))));

        var compilation = workspace.GetCompilation(projectId);
        var brokenTree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/broken.rav");
        var stableTree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/stable.rav");
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var brokenModel = compilation.GetSemanticModel(brokenTree);
        var stableModel = compilation.GetSemanticModel(stableTree);
        var brokenFunction = brokenTree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var stableFunction = stableTree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "Stable");
        var invocation = stableTree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var brokenSymbol = Assert.IsAssignableFrom<IMethodSymbol>(brokenModel.GetDeclaredSymbol(brokenFunction));
        var stableSymbol = Assert.IsAssignableFrom<IMethodSymbol>(stableModel.GetDeclaredSymbol(stableFunction));
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(stableModel.GetSymbolInfo(invocation).Symbol);
        var errors = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();

        Assert.Equal("Broken", brokenSymbol.Name);
        Assert.True(brokenSymbol.IsGenericMethod);
        Assert.True(SymbolEqualityComparer.Default.Equals(stableSymbol, selected.ConstructedFrom));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(selected.TypeArguments).SpecialType);
        Assert.NotEmpty(errors);
        Assert.All(errors, diagnostic => Assert.Same(brokenTree, diagnostic.Location.SourceTree));
        Assert.All(errors, diagnostic => Assert.True(brokenFunction.Span.Contains(diagnostic.Location.SourceSpan)));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EditingGenericOverloadBody_PreservesSiblingOverloadResolution(bool diagnosticsFirst)
    {
        const string source = """
            func Convert<T>(value: T) -> string {
                ""
            }

            func Convert(value: string) -> string {
                value
            }

            func Main() -> int {
                let converted = Convert("ok")
                0
            }
            """;
        var (workspace, projectId, documentId) = CreateWorkspace(source, "overload-declaration-isolation");
        Assert.Empty(workspace.GetCompilation(projectId).GetDiagnostics());

        workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
            documentId,
            SourceText.From(source.Replace("    \"\"\n", "    missingValue\n", StringComparison.Ordinal))));

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single();
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var model = compilation.GetSemanticModel(tree);
        var functions = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>()
            .Where(static function => function.Identifier.ValueText == "Convert")
            .ToArray();
        var genericFunction = Assert.Single(functions, static function => function.TypeParameterList is not null);
        var nonGenericFunction = Assert.Single(functions, static function => function.TypeParameterList is null);
        var nonGenericSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(nonGenericFunction));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression.ToString() == "Convert");
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(nonGenericSymbol, selected));
        AssertErrorsAreConfinedTo(compilation, genericFunction.Span);
    }

    private static (RavenWorkspace Workspace, ProjectId ProjectId, DocumentId DocumentId) CreateWorkspace(
        string source,
        string projectName)
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            projectName,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "declarations.rav",
            SourceText.From(source),
            $"/tmp/{projectName}.rav").Project;
        workspace.TryApplyChanges(project.Solution);

        return (workspace, projectId, project.Documents.Single().Id);
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
