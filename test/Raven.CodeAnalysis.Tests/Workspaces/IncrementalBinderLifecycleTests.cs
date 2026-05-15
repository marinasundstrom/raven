using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class IncrementalBinderLifecycleTests
{
    [Fact]
    public void WorkspaceCompilation_TopLevelStatementEdit_IncrementalBinderBindsReferencesWithoutRootBinder()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        var initialSource = """
            val first = 1
            val second = first + 1
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialRoot = initialTree.GetRoot();
        var initialGlobals = initialRoot.DescendantNodes().OfType<GlobalStatementSyntax>().ToArray();
        var initialTopLevelBinder = initialModel.GetIncrementalSemanticQueryBinderForTesting(initialGlobals[0].Statement).ShouldBeAssignableTo<TopLevelBinder>();
        initialModel.RootBinderCreated.ShouldBeFalse();

        var initialFirstDeclarator = initialGlobals[0].DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initialFirstLocal = initialTopLevelBinder.BindDeclaredSymbol(initialFirstDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        initialFirstLocal.Name.ShouldBe("first");
        initialTopLevelBinder.GetLocalForTesting("first").ShouldBeSameAs(initialFirstLocal);
        initialTopLevelBinder.LocalStateVersionForTesting.ShouldBe(1);
        initialTopLevelBinder
            .BindReferencedSymbol(GetIdentifier(initialGlobals[1], "first"))
            .Symbol
            .ShouldBeSameAs(initialFirstLocal);
        initialModel.RootBinderCreated.ShouldBeFalse();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(initialSource.Replace("first + 1", "first + 2", StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedRoot = updatedTree.GetRoot();
        var updatedGlobals = updatedRoot.DescendantNodes().OfType<GlobalStatementSyntax>().ToArray();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedGlobals[1]).ShouldBeTrue();
        updatedModel.RootBinderCreated.ShouldBeFalse();

        var updatedTopLevelBinder = updatedModel.GetIncrementalSemanticQueryBinderForTesting(updatedGlobals[0].Statement).ShouldBeAssignableTo<TopLevelBinder>();
        updatedModel.RootBinderCreated.ShouldBeFalse();
        var updatedFirstDeclarator = updatedGlobals[0].DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var updatedFirstLocal = updatedTopLevelBinder.BindDeclaredSymbol(updatedFirstDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        updatedFirstLocal.ShouldNotBeSameAs(initialFirstLocal);
        updatedTopLevelBinder.GetLocalForTesting("first").ShouldBeSameAs(updatedFirstLocal);
        updatedTopLevelBinder.LocalStateVersionForTesting.ShouldBe(1);
        updatedTopLevelBinder
            .BindReferencedSymbol(GetIdentifier(updatedGlobals[1], "first"))
            .Symbol
            .ShouldBeSameAs(updatedFirstLocal);
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void GetDeclaredSymbol_ForTopLevelLaterLocal_AfterEdit_UsesIncrementalBinderState()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        var initialSource = """
            val first = 1
            val second = first + 1
            val third = second + 1
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialRoot = initialTree.GetRoot();
        var initialThirdDeclarator = initialRoot.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "third");
        var initialThirdLocal = initialModel.GetDeclaredSymbol(initialThirdDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        initialThirdLocal.Name.ShouldBe("third");
        initialModel.RootBinderCreated.ShouldBeFalse();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(initialSource.Replace("second + 1", "second + 2", StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedRoot = updatedTree.GetRoot();
        var updatedGlobals = updatedRoot.DescendantNodes().OfType<GlobalStatementSyntax>().ToArray();
        var updatedThirdDeclarator = updatedRoot.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "third");

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedGlobals[2]).ShouldBeTrue();
        updatedModel.RootBinderCreated.ShouldBeFalse();

        var updatedThirdLocal = updatedModel.GetDeclaredSymbol(updatedThirdDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

        updatedThirdLocal.ShouldNotBeSameAs(initialThirdLocal);
        updatedThirdLocal.Name.ShouldBe("third");
        updatedThirdLocal.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();

        var updatedTopLevelBinder = updatedModel.GetIncrementalSemanticQueryBinderForTesting(updatedGlobals[2].Statement).ShouldBeAssignableTo<TopLevelBinder>();
        updatedTopLevelBinder.GetLocalForTesting("first").ShouldNotBeNull();
        updatedTopLevelBinder.GetLocalForTesting("second").ShouldNotBeNull();
        updatedTopLevelBinder.GetLocalForTesting("third").ShouldBeSameAs(updatedThirdLocal);
        updatedTopLevelBinder
            .BindReferencedSymbol(GetIdentifier(updatedGlobals[2], "second"))
            .Symbol
            .ShouldBeSameAs(updatedTopLevelBinder.GetLocalForTesting("second"));
        updatedTopLevelBinder.LocalStateVersionForTesting.ShouldBe(3);
        updatedModel.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void GetDeclaredSymbol_ForMemberDeclarations_AfterBodyEdit_UsesSourceDeclarationFastPath()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        var initialSource = """
            class Edited {
                val Name: string = "A"

                func Changed() -> int {
                    return 1
                }
            }
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(initialSource.Replace("return 1", "return 2", StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedRoot = updatedTree.GetRoot();
        var updatedType = updatedRoot.DescendantNodes().OfType<TypeDeclarationSyntax>().Single();
        var updatedProperty = updatedRoot.DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var updatedMethod = updatedRoot.DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();

        var typeSymbol = updatedModel.GetDeclaredSymbol(updatedType).ShouldBeAssignableTo<INamedTypeSymbol>();
        var propertySymbol = updatedModel.GetDeclaredSymbol(updatedProperty).ShouldBeAssignableTo<IPropertySymbol>();
        var methodSymbol = updatedModel.GetDeclaredSymbol(updatedMethod).ShouldBeAssignableTo<IMethodSymbol>();

        typeSymbol.Name.ShouldBe("Edited");
        propertySymbol.Name.ShouldBe("Name");
        methodSymbol.Name.ShouldBe("Changed");
        updatedModel.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void SemanticModel_RemoveCachedBinder_DiscardsBinderOwnedLocalState()
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
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val copy = value
                        return copy
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var declarator = block.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var initialBinder = model.GetIncrementalSemanticQueryBinderForTesting(block).ShouldBeAssignableTo<BlockBinder>();
        var initialLocal = initialBinder.BindDeclaredSymbol(declarator).ShouldBeAssignableTo<ILocalSymbol>();
        initialBinder.GetLocalForTesting("copy").ShouldBeSameAs(initialLocal);
        initialBinder.LocalStateVersionForTesting.ShouldBe(1);
        model.GetBinderLifecycleSnapshotForTesting(initialBinder).BinderType.ShouldBe("MethodBodyBinder");

        model.RemoveCachedBinder(block);

        Should.Throw<InvalidOperationException>(() => model.GetBinderLifecycleSnapshotForTesting(initialBinder));

        var reboundBinder = model.GetIncrementalSemanticQueryBinderForTesting(block).ShouldBeAssignableTo<BlockBinder>();
        reboundBinder.ShouldNotBeSameAs(initialBinder);
        reboundBinder.GetLocalForTesting("copy").ShouldBeNull();
        reboundBinder.LocalStateVersionForTesting.ShouldBe(0);

        var reboundLocal = reboundBinder.BindDeclaredSymbol(declarator).ShouldBeAssignableTo<ILocalSymbol>();
        reboundLocal.ShouldNotBeSameAs(initialLocal);
        reboundBinder.GetLocalForTesting("copy").ShouldBeSameAs(reboundLocal);
        reboundBinder.LocalStateVersionForTesting.ShouldBe(1);
        reboundBinder
            .BindReferencedSymbol(GetIdentifier(block, "copy"))
            .Symbol
            .ShouldBeSameAs(reboundLocal);
    }

    [Fact]
    public void GetDeclaredSymbol_ForErrorLocal_StoresErrorLocalInOwningBlockBinder()
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
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Stable() -> unit {
                        val broken = missingValue
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var declarator = block.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        model.RootBinderCreated.ShouldBeFalse();

        var declaredSymbol = model.GetDeclaredSymbol(declarator);
        declaredSymbol.ShouldNotBeNull();
        var brokenLocal = declaredSymbol!.ShouldBeAssignableTo<ILocalSymbol>();

        brokenLocal.Name.ShouldBe("broken");
        brokenLocal.Type.ShouldNotBeNull();
        brokenLocal.Type.TypeKind.ShouldBe(TypeKind.Error);
        var blockBinder = model.GetIncrementalSemanticQueryBinderForTesting(block).ShouldBeAssignableTo<BlockBinder>();
        blockBinder.GetLocalForTesting("broken").ShouldBeSameAs(brokenLocal);
        blockBinder.LocalStateVersionForTesting.ShouldBe(1);
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void GetDeclaredSymbol_ForPatternDesignation_StoresPatternLocalInOwningBlockBinder()
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
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Stable(input: (string, int)) -> unit {
                        if val (key, value) = input {
                            val copy = value
                        }
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().First();
        var valueDesignation = block.DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(designation => designation.Identifier.ValueText == "value");

        model.RootBinderCreated.ShouldBeFalse();

        var valueLocal = model.GetDeclaredSymbol(valueDesignation).ShouldBeAssignableTo<ILocalSymbol>();

        valueLocal.Name.ShouldBe("value");
        valueLocal.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        var blockBinder = model.GetIncrementalSemanticQueryBinderForTesting(block).ShouldBeAssignableTo<BlockBinder>();
        blockBinder.GetLocalForTesting("value").ShouldBeSameAs(valueLocal);
        blockBinder.GetLocalForTesting("key").ShouldNotBeNull();
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void GetDeclaredSymbol_ForLaterLocal_RestoresPrecedingLocalsInOwningBlockBinder()
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
            "edited.rav",
            SourceText.From(
                """
                import System.*
                import System.Linq.*
                import System.Linq.Expressions.*

                class User(var Name: string, var Age: int, var IsActive: bool)

                class QueryOwner {
                    func Build(users: IQueryable<User>) {
                        val minAge = 21
                        val onlyActiveAdults: Expression<System.Func<User, bool>> =
                            user => user.IsActive && user.Age >= minAge

                        val query = users
                            |> Where(onlyActiveAdults)
                            |> OrderBy(user => user.Name)
                            |> Select(user => user.Name)
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var block = root.DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var blockBinder = model.GetIncrementalSemanticQueryBinderForTesting(block).ShouldBeAssignableTo<BlockBinder>();
        var onlyActiveAdultsDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "onlyActiveAdults");
        var queryDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "query");
        var onlyActiveAdultsReference = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "onlyActiveAdults");

        blockBinder.LocalStateVersionForTesting.ShouldBe(0);

        var onlyActiveAdultsLocal = model.GetDeclaredSymbol(onlyActiveAdultsDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

        onlyActiveAdultsLocal.Name.ShouldBe("onlyActiveAdults");
        blockBinder.GetLocalForTesting("minAge").ShouldNotBeNull();
        blockBinder.GetLocalForTesting("onlyActiveAdults").ShouldBeSameAs(onlyActiveAdultsLocal);
        blockBinder.GetLocalForTesting("query").ShouldBeNull();
        var stateVersionBeforeQuery = blockBinder.LocalStateVersionForTesting;

        var queryLocal = model.GetDeclaredSymbol(queryDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

        queryLocal.Name.ShouldBe("query");
        blockBinder.LocalStateVersionForTesting.ShouldBe(stateVersionBeforeQuery + 1);
        blockBinder.GetLocalForTesting("minAge").ShouldNotBeNull();
        blockBinder.GetLocalForTesting("onlyActiveAdults").ShouldNotBeNull();
        blockBinder.GetLocalForTesting("query").ShouldBeSameAs(queryLocal);
        blockBinder.BindReferencedSymbol(onlyActiveAdultsReference).Symbol?.Name.ShouldBe("onlyActiveAdults");
        model.GetDeclaredSymbol(queryDeclarator).ShouldBeSameAs(queryLocal);
        blockBinder.LocalStateVersionForTesting.ShouldBe(stateVersionBeforeQuery + 1);
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void BindReferencedSymbol_ForLaterStatement_RestoresPrecedingLocalsOnce()
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
            "edited.rav",
            SourceText.From(
                """
                class QueryOwner {
                    func Build() {
                        val first = 1
                        val second = first + 1
                        val third = second + 1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var block = root.DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var blockBinder = model.GetIncrementalSemanticQueryBinderForTesting(block).ShouldBeAssignableTo<BlockBinder>();
        var secondReference = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "second");

        blockBinder.LocalStateVersionForTesting.ShouldBe(0);

        var secondSymbol = blockBinder.BindReferencedSymbol(secondReference).Symbol.ShouldBeAssignableTo<ILocalSymbol>();

        secondSymbol.Name.ShouldBe("second");
        blockBinder.LocalStateVersionForTesting.ShouldBe(2);
        blockBinder.GetLocalForTesting("first").ShouldNotBeNull();
        blockBinder.GetLocalForTesting("second").ShouldBeSameAs(secondSymbol);
        blockBinder.GetLocalForTesting("third").ShouldBeNull();

        blockBinder.BindReferencedSymbol(secondReference).Symbol.ShouldBeSameAs(secondSymbol);
        blockBinder.LocalStateVersionForTesting.ShouldBe(2);
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void GetDeclaredSymbol_ForLocalInAsyncFunctionExpression_UsesFunctionExpressionBinderState()
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
            "edited.rav",
            SourceText.From(
                """
                import System.*
                import System.Threading.Tasks.*

                class RequestContext {
                    public val Text: string = "body"
                }

                class App {
                    func Configure() -> unit {
                        Accept(async func (context: RequestContext) {
                            val content = await Task.FromResult(context.Text)
                            return "submitted: $content"
                        })
                    }

                    func Accept(handler: func (RequestContext) -> Task<string>) -> unit { }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var functionExpression = root.DescendantNodes().OfType<FunctionExpressionSyntax>().Single();
        var functionBody = functionExpression.Body!;
        functionBody.ShouldNotBeNull();
        var contentDeclarator = functionBody.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "content");

        var contentLocal = model.GetDeclaredSymbol(contentDeclarator).ShouldBeAssignableTo<ILocalSymbol>();

        contentLocal.Type.SpecialType.ShouldBe(SpecialType.System_String);
        var functionBinder = model.GetIncrementalSemanticQueryBinderForTesting(functionBody).ShouldBeAssignableTo<FunctionExpressionBinder>();
        functionBinder.LookupSymbol("context").ShouldBeAssignableTo<IParameterSymbol>();
        functionBinder.GetLocalForTesting("content").ShouldBeSameAs(contentLocal);
        model.GetDiagnostics()
            .Where(diagnostic =>
                diagnostic.Descriptor.Id is "RAV2700" or "RAV1900" ||
                diagnostic.Descriptor.Id == "RAV0103" &&
                diagnostic.GetMessage().Contains("'context' is not in scope", StringComparison.Ordinal))
            .Select(diagnostic => $"{diagnostic.Descriptor.Id}: {diagnostic.GetMessage()}")
            .ShouldBeEmpty();
    }

    [Fact]
    public void WorkspaceCompilation_BodyEdit_IncrementalSemanticQueryBinderCreatesFreshChildWithMethodParent()
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
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val copy = value
                        return copy
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialBlock = initialTree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var initialBinder = initialModel.GetIncrementalSemanticQueryBinderForTesting(initialBlock);
        var initialLifecycle = initialModel.GetBinderLifecycleSnapshotForTesting(initialBinder);
        var initialBlockBinder = initialBinder.ShouldBeAssignableTo<BlockBinder>();
        var initialDeclarator = initialBlock.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initialCopyLocal = initialBlockBinder.BindDeclaredSymbol(initialDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        initialCopyLocal.Name.ShouldBe("copy");
        initialCopyLocal.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        initialBlockBinder.GetLocalForTesting("copy").ShouldBeSameAs(initialCopyLocal);
        initialBlockBinder.GetDeclaredLocalsForTesting().Single().ShouldBeSameAs(initialCopyLocal);
        initialBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        initialBlockBinder.BindDeclaredSymbol(initialDeclarator).ShouldBeSameAs(initialCopyLocal);
        initialBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        initialBlockBinder
            .BindReferencedSymbol(GetIdentifier(initialBlock, "value"))
            .Symbol
            .ShouldBeAssignableTo<IParameterSymbol>()
            .Name
            .ShouldBe("value");
        initialBlockBinder
            .BindReferencedSymbol(GetIdentifier(initialBlock, "copy"))
            .Symbol
            .ShouldBeSameAs(initialCopyLocal);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val copy = value + 1
                        return copy
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedMethod = updatedTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var updatedBlock = updatedMethod.DescendantNodes().OfType<BlockStatementSyntax>().Single();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedMethod).ShouldBeTrue();

        var updatedBinder = updatedModel.GetIncrementalSemanticQueryBinderForTesting(updatedBlock);
        var updatedLifecycle = updatedModel.GetBinderLifecycleSnapshotForTesting(updatedBinder);

        updatedBinder.ShouldNotBeSameAs(initialBinder);
        updatedBinder.GetType().Name.ShouldBe("MethodBodyBinder");
        updatedBinder.ContainingSymbol.ShouldBeAssignableTo<IMethodSymbol>().Name.ShouldBe("Stable");
        updatedBinder.ParentBinder.ShouldNotBeNull();
        var updatedMethodBinder = updatedBinder.ParentBinder.ShouldBeAssignableTo<MethodBinder>();
        updatedMethodBinder.GetType().Name.ShouldBe("MethodBinder");
        updatedMethodBinder.ContainingSymbol.ShouldBeSameAs(updatedBinder.ContainingSymbol);
        updatedMethodBinder.ParentBinder.ShouldNotBeNull();
        updatedMethodBinder.ParentBinder!.ContainingSymbol.ShouldBeAssignableTo<INamedTypeSymbol>().Name.ShouldBe("Edited");
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();
        updatedLifecycle.BinderType.ShouldBe("MethodBodyBinder");
        updatedLifecycle.NodeKind.ShouldBe(updatedBlock.Kind);
        updatedLifecycle.CacheKind.ShouldBe("ExactNode");
        updatedLifecycle.IsStructuralCacheable.ShouldBeFalse();
        updatedLifecycle.ContainingSymbolKey.ShouldBe(initialLifecycle.ContainingSymbolKey);
        updatedLifecycle.ParentBinderType.ShouldBe("MethodBinder");
        updatedLifecycle.ParentContainingSymbolKey.ShouldBe(initialLifecycle.ParentContainingSymbolKey);

        updatedMethodBinder.ScopeStateBuildCountForTesting.ShouldBe(0);
        var parameterFromLookup = updatedMethodBinder.LookupSymbol("value").ShouldBeAssignableTo<IParameterSymbol>();
        var parameterSyntax = updatedMethod.ParameterList!.Parameters.Single();
        var parameterFromDeclaredSymbol = updatedMethodBinder.BindDeclaredSymbol(parameterSyntax).ShouldBeAssignableTo<IParameterSymbol>();
        parameterFromDeclaredSymbol.ShouldBeSameAs(parameterFromLookup);
        updatedMethodBinder.GetCachedParametersForTesting().Single().ShouldBeSameAs(parameterFromLookup);
        updatedMethodBinder.ScopeStateBuildCountForTesting.ShouldBe(1);
        updatedMethodBinder.LookupSymbol("value").ShouldBeSameAs(parameterFromLookup);
        updatedMethodBinder.ScopeStateBuildCountForTesting.ShouldBe(1);

        var updatedBlockBinder = updatedBinder.ShouldBeAssignableTo<BlockBinder>();
        updatedBlockBinder.LocalStateVersionForTesting.ShouldBe(0);
        updatedBlockBinder.GetLocalForTesting("copy").ShouldBeNull();
        var updatedDeclarator = updatedBlock.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var copyLocalFromDeclarator = updatedBlockBinder.BindDeclaredSymbol(updatedDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        copyLocalFromDeclarator.ShouldNotBeSameAs(initialCopyLocal);
        updatedBlockBinder.GetLocalForTesting("copy").ShouldBeSameAs(copyLocalFromDeclarator);
        updatedBlockBinder.GetDeclaredLocalsForTesting().Single().ShouldBeSameAs(copyLocalFromDeclarator);
        updatedBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        updatedBlockBinder.BindDeclaredSymbol(updatedDeclarator).ShouldBeSameAs(copyLocalFromDeclarator);
        updatedBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        updatedBlockBinder
            .BindReferencedSymbol(GetIdentifier(updatedBlock, "value"))
            .Symbol
            .ShouldBeSameAs(parameterFromLookup);
        updatedBlockBinder
            .BindReferencedSymbol(GetIdentifier(updatedBlock, "copy"))
            .Symbol
            .ShouldBeSameAs(copyLocalFromDeclarator);

        _ = updatedBlockBinder.GetOrBind(updatedBlock);
        var copyLocal = updatedBlockBinder
            .GetDeclaredLocalsForTesting()
            .Single(local => local.Name == "copy");
        copyLocal.ShouldBeSameAs(copyLocalFromDeclarator);
        copyLocal.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
        copyLocal.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_UnrelatedBodyEdit_KeepsStableBodyBinderLifecycleCheapAndLocal()
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
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        return 1
                    }

                    func Stable(value: int) -> int {
                        val copy = value
                        return copy
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialStableMethod = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var initialBlock = initialStableMethod.DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var initialBinder = initialModel.GetIncrementalSemanticQueryBinderForTesting(initialBlock);
        var initialLifecycle = initialModel.GetBinderLifecycleSnapshotForTesting(initialBinder);
        var initialBlockBinder = initialBinder.ShouldBeAssignableTo<BlockBinder>();
        var initialDeclarator = initialBlock.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initialLocal = initialBlockBinder.BindDeclaredSymbol(initialDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        initialBlockBinder.GetLocalForTesting("copy").ShouldBeSameAs(initialLocal);
        initialBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        initialModel.RootBinderCreated.ShouldBeFalse();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        return 2
                    }

                    func Stable(value: int) -> int {
                        val copy = value
                        return copy
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedMethods = updatedTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        var updatedChangedMethod = updatedMethods.Single(method => method.Identifier.ValueText == "Changed");
        var updatedStableMethod = updatedMethods.Single(method => method.Identifier.ValueText == "Stable");
        var updatedBlock = updatedStableMethod.DescendantNodes().OfType<BlockStatementSyntax>().Single();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedChangedMethod).ShouldBeTrue();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedStableMethod).ShouldBeFalse();
        updatedModel.GetMatchedExecutableOwnerForTesting(updatedBlock).ShouldNotBeNull();
        updatedModel.RootBinderCreated.ShouldBeFalse();

        var updatedBinder = updatedModel.GetIncrementalSemanticQueryBinderForTesting(updatedBlock);
        var updatedLifecycle = updatedModel.GetBinderLifecycleSnapshotForTesting(updatedBinder);
        updatedBinder.ShouldNotBeSameAs(initialBinder);
        updatedLifecycle.BinderType.ShouldBe("MethodBodyBinder");
        updatedLifecycle.ContainingSymbolKey.ShouldBe(initialLifecycle.ContainingSymbolKey);
        updatedLifecycle.ParentBinderType.ShouldBe("MethodBinder");
        updatedLifecycle.ParentContainingSymbolKey.ShouldBe(initialLifecycle.ParentContainingSymbolKey);
        updatedModel.RootBinderCreated.ShouldBeFalse();

        var updatedMethodBinder = updatedBinder.ParentBinder.ShouldBeAssignableTo<MethodBinder>();
        updatedMethodBinder.ScopeStateBuildCountForTesting.ShouldBe(0);
        var updatedParameter = updatedMethodBinder.LookupSymbol("value").ShouldBeAssignableTo<IParameterSymbol>();
        updatedMethodBinder.GetCachedParametersForTesting().Single().ShouldBeSameAs(updatedParameter);
        updatedMethodBinder.ScopeStateBuildCountForTesting.ShouldBe(1);

        var updatedBlockBinder = updatedBinder.ShouldBeAssignableTo<BlockBinder>();
        updatedBlockBinder.LocalStateVersionForTesting.ShouldBe(0);
        updatedBlockBinder.GetLocalForTesting("copy").ShouldBeNull();
        var updatedDeclarator = updatedBlock.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var updatedLocal = updatedBlockBinder.BindDeclaredSymbol(updatedDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        updatedLocal.ShouldNotBeSameAs(initialLocal);
        updatedBlockBinder.GetLocalForTesting("copy").ShouldBeSameAs(updatedLocal);
        updatedBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        updatedBlockBinder
            .BindReferencedSymbol(GetIdentifier(updatedBlock, "value"))
            .Symbol
            .ShouldBeSameAs(updatedParameter);
        updatedBlockBinder
            .BindReferencedSymbol(GetIdentifier(updatedBlock, "copy"))
            .Symbol
            .ShouldBeSameAs(updatedLocal);
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_MethodSignatureEdit_ChangesBodyBinderLifecycleContainingSymbol()
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
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val copy = value
                        return copy
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialBlock = initialTree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var initialBinder = initialModel.GetIncrementalSemanticQueryBinderForTesting(initialBlock);
        var initialLifecycle = initialModel.GetBinderLifecycleSnapshotForTesting(initialBinder);
        var initialMethodBinder = initialBinder.ParentBinder.ShouldBeAssignableTo<MethodBinder>();
        var initialParameter = initialMethodBinder.LookupSymbol("value").ShouldBeAssignableTo<IParameterSymbol>();
        initialParameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
        var initialBlockBinder = initialBinder.ShouldBeAssignableTo<BlockBinder>();
        var initialDeclarator = initialBlock.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initialCopyLocal = initialBlockBinder.BindDeclaredSymbol(initialDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        initialCopyLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
        initialBlockBinder
            .BindReferencedSymbol(GetIdentifier(initialBlock, "value"))
            .Symbol
            .ShouldBeSameAs(initialParameter);
        initialBlockBinder
            .BindReferencedSymbol(GetIdentifier(initialBlock, "copy"))
            .Symbol
            .ShouldBeSameAs(initialCopyLocal);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(value: string) -> string {
                        val copy = value
                        return copy
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedMethod = updatedTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var updatedBlock = updatedMethod.DescendantNodes().OfType<BlockStatementSyntax>().Single();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeFalse();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedMethod).ShouldBeTrue();

        var updatedBinder = updatedModel.GetIncrementalSemanticQueryBinderForTesting(updatedBlock);
        var updatedLifecycle = updatedModel.GetBinderLifecycleSnapshotForTesting(updatedBinder);

        updatedBinder.GetType().Name.ShouldBe("MethodBodyBinder");
        updatedBinder.ContainingSymbol.ShouldBeAssignableTo<IMethodSymbol>().Name.ShouldBe("Stable");
        var updatedMethodBinder = updatedBinder.ParentBinder.ShouldBeAssignableTo<MethodBinder>();
        var updatedParameter = updatedMethodBinder.LookupSymbol("value").ShouldBeAssignableTo<IParameterSymbol>();
        updatedParameter.ShouldNotBeSameAs(initialParameter);
        updatedParameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        updatedMethodBinder.GetCachedParametersForTesting().Single().ShouldBeSameAs(updatedParameter);
        updatedMethodBinder.ScopeStateBuildCountForTesting.ShouldBe(1);
        var updatedBlockBinder = updatedBinder.ShouldBeAssignableTo<BlockBinder>();
        var updatedDeclarator = updatedBlock.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var updatedCopyLocal = updatedBlockBinder.BindDeclaredSymbol(updatedDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        updatedCopyLocal.ShouldNotBeSameAs(initialCopyLocal);
        updatedCopyLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        updatedBlockBinder.GetLocalForTesting("copy").ShouldBeSameAs(updatedCopyLocal);
        updatedBlockBinder.LocalStateVersionForTesting.ShouldBe(1);
        updatedBlockBinder
            .BindReferencedSymbol(GetIdentifier(updatedBlock, "value"))
            .Symbol
            .ShouldBeSameAs(updatedParameter);
        updatedBlockBinder
            .BindReferencedSymbol(GetIdentifier(updatedBlock, "copy"))
            .Symbol
            .ShouldBeSameAs(updatedCopyLocal);
        updatedLifecycle.BinderType.ShouldBe("MethodBodyBinder");
        updatedLifecycle.NodeKind.ShouldBe(updatedBlock.Kind);
        updatedLifecycle.CacheKind.ShouldBe("ExactNode");
        updatedLifecycle.ContainingSymbolKey.ShouldNotBe(initialLifecycle.ContainingSymbolKey);
        updatedLifecycle.ParentBinderType.ShouldBe("MethodBinder");
        updatedLifecycle.ParentContainingSymbolKey.ShouldNotBe(initialLifecycle.ParentContainingSymbolKey);
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();
    }

    private static IdentifierNameSyntax GetIdentifier(SyntaxNode node, string name)
        => node.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == name);
}
