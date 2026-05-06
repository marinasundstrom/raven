using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class IncrementalCompilationReuseTests
{
    private static object GetDescriptorState(Compilation compilation)
    {
        var field = typeof(Compilation).GetField("_descriptorState", BindingFlags.Instance | BindingFlags.NonPublic);
        field.ShouldNotBeNull();
        var value = field!.GetValue(compilation);
        value.ShouldNotBeNull();
        return value!;
    }

    [Fact]
    public void WorkspaceCompilation_ReusesDeclarationKeys_ForUnchangedSyntaxTreesAcrossDocumentEdit()
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
            "unchanged.rav",
            SourceText.From(
                """
                class Stable {
                    func Ping() -> string {
                        "pong"
                    }
                }
                """),
            "/tmp/unchanged.rav").Project;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialStableTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var initialStableRoot = (CompilationUnitSyntax)initialStableTree.GetRoot();
        var initialStableType = initialStableRoot.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        initialCompilation.DeclarationTable.TryGetDeclKey(initialStableType, out var initialStableKey).ShouldBeTrue();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedStableTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var updatedStableRoot = (CompilationUnitSyntax)updatedStableTree.GetRoot();
        var updatedStableType = updatedStableRoot.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        updatedCompilation.DeclarationTable.TryGetDeclKey(updatedStableType, out var updatedStableKey).ShouldBeTrue();

        updatedStableTree.ShouldBeSameAs(initialStableTree);
        updatedStableKey.ShouldBeSameAs(initialStableKey);
    }

    [Fact]
    public void WorkspaceCompilation_UsesDistinctDescriptorStatePerCompilationIncrement()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialState = GetDescriptorState(initialCompilation);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialStableIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        initialModel.GetSymbolInfo(initialStableIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetNodeInterestSymbolDescriptorForTesting(initialStableIdentifier).ShouldNotBeNull();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        2
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedState = GetDescriptorState(updatedCompilation);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedStableIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        ReferenceEquals(initialState, updatedState).ShouldBeFalse();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedStableIdentifier).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_ReusesVisibleValueScopeDeclarations_ForUnchangedSyntaxTreesAcrossDocumentEdit()
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
            "unchanged.rav",
            SourceText.From(
                """
                class Stable {
                    func Ping(value: int) -> int {
                        val copy = value
                        return copy
                    }
                }
                """),
            "/tmp/unchanged.rav").Project;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialStableTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialStableTree);
        var initialScope = initialStableTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var initialDeclarations = initialModel.GetVisibleValueDeclarationsForTesting(initialScope);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedStableTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedStableTree);
        var updatedScope = updatedStableTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var updatedDeclarations = updatedModel.GetVisibleValueDeclarationsForTesting(updatedScope);

        updatedStableTree.ShouldBeSameAs(initialStableTree);
        updatedDeclarations.Select(static declaration => declaration.Name).ShouldBe(["value"]);
        updatedDeclarations.Length.ShouldBe(initialDeclarations.Length);
        updatedDeclarations[0].DeclarationNode.Kind.ShouldBe(initialDeclarations[0].DeclarationNode.Kind);
        updatedDeclarations[0].DeclarationNode.Span.ShouldBe(initialDeclarations[0].DeclarationNode.Span);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesContextualBindingRootDescriptors_ForUnchangedSyntaxTreesAcrossDocumentEdit()
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
            "unchanged.rav",
            SourceText.From(
                """
                class Stable {
                    func Ping(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """),
            "/tmp/unchanged.rav").Project;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialStableTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialStableTree);
        var initialIdentifier = initialStableTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var initialDescriptor = initialModel.GetContextualBindingRootDescriptorForTesting(initialIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedStableTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedStableTree);
        var updatedIdentifier = updatedStableTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var updatedDescriptor = updatedModel.GetContextualBindingRootDescriptorForTesting(updatedIdentifier);

        updatedStableTree.ShouldBeSameAs(initialStableTree);
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesExecutableOwnerDescriptors_ForUnchangedSyntaxTreesAcrossDocumentEdit()
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
            "unchanged.rav",
            SourceText.From(
                """
                class Stable {
                    func Ping(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        return projection(value)
                    }
                }
                """),
            "/tmp/unchanged.rav").Project;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialStableTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialStableTree);
        var initialIdentifier = initialStableTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "item");
        var initialDescriptor = initialModel.GetExecutableOwnerDescriptorForTesting(initialIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedStableTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedStableTree);
        var updatedIdentifier = updatedStableTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "item");
        var updatedDescriptor = updatedModel.GetExecutableOwnerDescriptorForTesting(updatedIdentifier);

        updatedStableTree.ShouldBeSameAs(initialStableTree);
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesFunctionExpressionRebindRootDescriptors_ForUnchangedSyntaxTreesAcrossDocumentEdit()
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
            "unchanged.rav",
            SourceText.From(
                """
                class Stable {
                    func Ping(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        return projection(value)
                    }
                }
                """),
            "/tmp/unchanged.rav").Project;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialStableTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialStableTree);
        var initialFunction = initialStableTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var initialDescriptor = initialModel.GetFunctionExpressionRebindRootDescriptorForTesting(initialFunction);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedStableTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedStableTree);
        var updatedFunction = updatedStableTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var updatedDescriptor = updatedModel.GetFunctionExpressionRebindRootDescriptorForTesting(updatedFunction);

        updatedStableTree.ShouldBeSameAs(initialStableTree);
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesBinderParentAnchorDescriptors_ForUnchangedSyntaxTreesAcrossDocumentEdit()
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
            "unchanged.rav",
            SourceText.From(
                """
                class Stable {
                    func Ping(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """),
            "/tmp/unchanged.rav").Project;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        1
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialStableTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialStableTree);
        var initialIdentifier = initialStableTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var initialDescriptor = initialModel.GetBinderParentAnchorDescriptorForTesting(initialIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Value() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedStableTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/unchanged.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedStableTree);
        var updatedIdentifier = updatedStableTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var updatedDescriptor = updatedModel.GetBinderParentAnchorDescriptorForTesting(updatedIdentifier);

        updatedStableTree.ShouldBeSameAs(initialStableTree);
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_TracksChangedExecutableOwners_ForChangedSyntaxTrees()
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
                        1
                    }

                    func Stable() -> int {
                        2
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);
        _ = workspace.GetCompilation(projectId);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable() -> int {
                        2
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var changedMethod = updatedTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(method => method.Identifier.ValueText == "Changed");
        var stableMethod = updatedTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(method => method.Identifier.ValueText == "Stable");
        var changedDescriptors = updatedCompilation.GetChangedExecutableOwnerDescriptorsForTesting(updatedTree);

        changedDescriptors.ShouldContain(new Compilation.ExecutableOwnerDescriptor(changedMethod.Span, changedMethod.Kind));
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(changedMethod).ShouldBeTrue();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(stableMethod).ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_ReusesContextualBindingRootDescriptors_ForMatchedExecutableOwnersInChangedSyntaxTree()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialStableIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var initialDescriptor = initialModel.GetContextualBindingRootDescriptorForTesting(initialStableIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedStableMethod = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var updatedStableIdentifier = updatedStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var updatedDescriptor = updatedModel.GetContextualBindingRootDescriptorForTesting(updatedStableIdentifier);
        var matchedOwner = updatedModel.GetMatchedExecutableOwnerForTesting(updatedStableIdentifier);

        updatedTree.ShouldNotBeSameAs(initialTree);
        matchedOwner.ShouldNotBeNull();
        matchedOwner.Value.PreviousSyntaxTree.ShouldBeSameAs(initialTree);
        matchedOwner.Value.CurrentOwner.ShouldBe(new Compilation.ExecutableOwnerDescriptor(updatedStableMethod.Span, updatedStableMethod.Kind));
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesNodeInterestSymbolDescriptors_ForMatchedExecutableOwnersInChangedSyntaxTree()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialStableIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        initialModel.GetSymbolInfo(initialStableIdentifier).Symbol?.Name.ShouldBe("value");
        var initialDescriptor = initialModel.GetNodeInterestSymbolDescriptorForTesting(initialStableIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedStableIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        var matchedOwner = updatedModel.GetMatchedExecutableOwnerForTesting(updatedStableIdentifier);
        var updatedInfo = updatedModel.GetSymbolInfo(updatedStableIdentifier);
        var updatedDescriptor = updatedModel.GetNodeInterestSymbolDescriptorForTesting(updatedStableIdentifier);

        matchedOwner.ShouldNotBeNull();
        updatedDescriptor.ShouldBe(initialDescriptor);
        updatedInfo.Symbol?.Name.ShouldBe("value");
    }

    [Fact]
    public void WorkspaceCompilation_FirstPostEditSymbolLookup_ForMatchedExecutableOwner_ReusesTransferredStateWithoutBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithPerformanceInstrumentation(instrumentation),
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
                        1
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialStableIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        initialModel.GetSymbolInfo(initialStableIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetNodeInterestSymbolDescriptorForTesting(initialStableIdentifier).ShouldNotBeNull();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedStableIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        instrumentation.BinderReentry.Reset();

        var updatedInfo = updatedModel.GetSymbolInfo(updatedStableIdentifier);

        updatedInfo.Symbol?.Name.ShouldBe("value");
        updatedModel.GetMatchedExecutableOwnerForTesting(updatedStableIdentifier).ShouldNotBeNull();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedStableIdentifier).ShouldBeTrue();
        instrumentation.BinderReentry.TotalBindExecutions.ShouldBe(0);
        instrumentation.BinderReentry.GetBindExecutionCount(updatedStableIdentifier).ShouldBe(0);
    }

    [Fact]
    public void WorkspaceCompilation_SemanticModel_PreparesDeclarationsWithoutRootBinder()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialStableIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        initialModel.GetSymbolInfo(initialStableIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetNodeInterestSymbolDescriptorForTesting(initialStableIdentifier).ShouldNotBeNull();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        2
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedStableIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");
        var updatedStableMethod = updatedStableIdentifier.Ancestors().OfType<MethodDeclarationSyntax>().Single();
        var updatedStableParameter = updatedStableMethod.ParameterList!.Parameters.Single();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeTrue();
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.MemberSignaturesDeclared.ShouldBeTrue();
        updatedModel.RootBinderCreated.ShouldBeFalse();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedStableIdentifier).ShouldBeTrue();
        updatedModel.GetDeclaredSymbol(updatedStableMethod).ShouldBeAssignableTo<IMethodSymbol>();
        updatedModel.RootBinderCreated.ShouldBeFalse();
        updatedModel.GetDeclaredSymbol(updatedStableParameter).ShouldBeAssignableTo<IParameterSymbol>();
        updatedModel.RootBinderCreated.ShouldBeFalse();

        updatedModel.GetSymbolInfo(updatedStableIdentifier).Symbol?.Name.ShouldBe("value");
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_MethodSignatureSymbol_IsReusedWhenFullSemanticPassCompletes()
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
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var signatureSymbol = model.GetDeclaredSymbol(method).ShouldBeAssignableTo<IMethodSymbol>();

        model.RootBinderCreated.ShouldBeFalse();
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeFalse();

        model.GetDiagnostics();

        var completedSymbol = model.GetDeclaredSymbol(method).ShouldBeAssignableTo<IMethodSymbol>();

        completedSymbol.ShouldBeSameAs(signatureSymbol);
        model.RootBinderCreated.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeTrue();
        completedSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
        completedSymbol.Parameters.Single().Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
    }

    [Fact]
    public void WorkspaceCompilation_MetadataTypeLookup_CompletesSourceDeclarationsWithoutRootBinder()
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
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);

        var stringType = compilation.GetTypeByMetadataName("System.String");

        stringType.ShouldNotBeNull();
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeTrue();
        model.MemberSignaturesDeclared.ShouldBeTrue();
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_EntryPointDiscovery_CompletesSourceDeclarationsWithoutRootBinder()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "main.rav",
            SourceText.From(
                """
                class Program {
                    static func Main() -> () {
                    }
                }
                """),
            "/tmp/main.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/main.rav");
        var model = compilation.GetSemanticModel(tree);

        var entryPoint = compilation.GetEntryPoint();

        entryPoint.ShouldNotBeNull();
        entryPoint.Name.ShouldBe("Main");
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeTrue();
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_EventDeclarationSymbol_ResolvesWithoutRootBinder()
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
                class Button {
                    event Clicked: System.Action {
                        add { }
                        remove { }
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var eventDeclaration = tree.GetRoot().DescendantNodes().OfType<EventDeclarationSyntax>().Single();

        model.MemberSignaturesDeclared.ShouldBeTrue();
        model.TryGetEventSymbol(eventDeclaration, out _).ShouldBeTrue();

        var eventSymbol = model.GetDeclaredSymbol(eventDeclaration).ShouldBeAssignableTo<IEventSymbol>();

        eventSymbol.Name.ShouldBe("Clicked");
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_AccessorDeclarationSymbol_ResolvesWithoutRootBinder()
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
                class Person {
                    val Name: string {
                        get => ""
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var accessor = tree.GetRoot().DescendantNodes().OfType<AccessorDeclarationSyntax>().Single();
        var property = accessor.Ancestors().OfType<PropertyDeclarationSyntax>().Single();

        model.MemberSignaturesDeclared.ShouldBeTrue();
        model.TryGetPropertySymbol(property, out var propertySymbol).ShouldBeTrue();
        propertySymbol.GetMethod.ShouldNotBeNull();

        var accessorSymbol = model.GetDeclaredSymbol(accessor).ShouldBeAssignableTo<IMethodSymbol>();

        accessorSymbol.Name.ShouldBe("get_Name");
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        model.RootBinderCreated.ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_MethodSignatureSymbol_ResolvesDeclaredNamedTypeWithoutRootBinder()
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
                class User

                class Edited {
                    func Find(user: User) -> User {
                        return user
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var signatureSymbol = model.GetDeclaredSymbol(method).ShouldBeAssignableTo<IMethodSymbol>();

        model.RootBinderCreated.ShouldBeFalse();
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeFalse();
        signatureSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("User");
        signatureSymbol.Parameters.Single().Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("User");
    }

    [Fact]
    public void WorkspaceCompilation_AddedMethodOverload_DeclaresSignatureWithoutRootBinderAfterEdit()
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
                    func Pick(value: int) -> int {
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        _ = initialCompilation.GetDiagnostics();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Pick(value: int) -> int {
                        return value
                    }

                    func Pick(text: string) -> string {
                        return text
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var addedOverload = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.ParameterList!.Parameters.Single().Identifier.ValueText == "text");

        var addedSymbol = updatedModel.GetDeclaredSymbol(addedOverload).ShouldBeAssignableTo<IMethodSymbol>();

        updatedCompilation.SourceDeclarationsDeclared.ShouldBeTrue();
        updatedCompilation.SourceDeclarationsComplete.ShouldBeFalse();
        updatedModel.RootBinderCreated.ShouldBeFalse();
        addedSymbol.Name.ShouldBe("Pick");
        addedSymbol.Parameters.Single().Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        addedSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
    }

    [Fact]
    public void WorkspaceCompilation_InsertSameArityOverloadBeforeExistingMethod_ReusesExistingMethodDescriptors()
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
                    func Pick(value: int) -> int {
                        return value
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialValueReference = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "value");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        initialModel.GetSymbolInfo(initialValueReference).Symbol?.Name.ShouldBe("value");
        initialModel.GetNodeInterestSymbolDescriptorForTesting(initialValueReference).ShouldNotBeNull();

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Pick(text: string) -> string {
                        return text
                    }

                    func Pick(value: int) -> int {
                        return value
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var unchangedMethodValueReference = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.ParameterList!.Parameters.Single().Identifier.ValueText == "value")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "value");

        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(unchangedMethodValueReference).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_PropertySignatureSymbol_IsReusedWhenFullSemanticPassCompletes()
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
                    val Name: string
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var signatureSymbol = model.GetDeclaredSymbol(property).ShouldBeAssignableTo<IPropertySymbol>();

        model.RootBinderCreated.ShouldBeFalse();
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeFalse();
        signatureSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");

        model.GetDiagnostics();

        var completedSymbol = model.GetDeclaredSymbol(property).ShouldBeAssignableTo<IPropertySymbol>();

        completedSymbol.ShouldBeSameAs(signatureSymbol);
        model.RootBinderCreated.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeTrue();
        completedSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        completedSymbol.GetMethod.ShouldNotBeNull();
    }

    [Fact]
    public void WorkspaceCompilation_InterfacePropertySignatureSymbol_IsReusedWhenFullSemanticPassCompletes()
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
                interface IEdited {
                    val Name: string
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var signatureSymbol = model.GetDeclaredSymbol(property).ShouldBeAssignableTo<IPropertySymbol>();

        model.RootBinderCreated.ShouldBeFalse();
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeFalse();
        signatureSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");

        model.GetDiagnostics();

        var completedSymbol = model.GetDeclaredSymbol(property).ShouldBeAssignableTo<IPropertySymbol>();

        completedSymbol.ShouldBeSameAs(signatureSymbol);
        model.RootBinderCreated.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeTrue();
        completedSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        completedSymbol.ContainingType?.TypeKind.ShouldBe(TypeKind.Interface);
    }

    [Fact]
    public void WorkspaceCompilation_PropertySignatureSymbol_ResolvesDeclaredNamedTypeWithoutRootBinder()
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
                class User

                class Edited {
                    val Owner: User
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var tree = compilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var signatureSymbol = model.GetDeclaredSymbol(property).ShouldBeAssignableTo<IPropertySymbol>();

        model.RootBinderCreated.ShouldBeFalse();
        compilation.SourceDeclarationsDeclared.ShouldBeTrue();
        compilation.SourceDeclarationsComplete.ShouldBeFalse();
        signatureSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("User");
    }

    [Fact]
    public void WorkspaceCompilation_PropertyRemovedAndReadded_ResolvesDeclaredPropertyAfterEdit()
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
                class Foo {
                    val Test: string => ""
                }

                class Use {
                    func Read() -> string {
                        val foo = Foo()
                        return foo.Test
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialProperty = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        initialModel.GetDeclaredSymbol(initialProperty).ShouldBeAssignableTo<IPropertySymbol>().Name.ShouldBe("Test");

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var withoutProperty = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Foo

                class Use {
                    func Read() -> string {
                        val foo = Foo()
                        return foo.Test
                    }
                }
                """));

        workspace.TryApplyChanges(withoutProperty);
        _ = workspace.GetCompilation(projectId).GetDiagnostics();

        var documentAfterRemoval = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var withPropertyAgain = workspace.CurrentSolution.WithDocumentText(
            documentAfterRemoval.Id,
            SourceText.From(
                """
                class Foo {
                    val Test: string => ""
                }

                class Use {
                    func Read() -> string {
                        val foo = Foo()
                        return foo.Test
                    }
                }
                """));

        workspace.TryApplyChanges(withPropertyAgain);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedProperty = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        var updatedSymbol = updatedModel.GetDeclaredSymbol(updatedProperty).ShouldBeAssignableTo<IPropertySymbol>();

        updatedSymbol.Name.ShouldBe("Test");
        updatedSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        updatedCompilation.GetDiagnostics().ShouldNotContain(diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.TypeAlreadyDefinesMember);
    }

    [Fact]
    public void WorkspaceCompilation_FirstPostEditMemberHoverLookup_ForMatchedExecutableOwner_ReusesParentBindingScope()
    {
        var instrumentation = new PerformanceInstrumentation();
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithPerformanceInstrumentation(instrumentation),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(
                """
                class User(Name: string)

                class Edited {
                    func Changed() -> int {
                        1
                    }

                    func Stable(user: User) -> string {
                        return user.Name
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialMemberAccess = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single();
        var initialMemberName = (IdentifierNameSyntax)initialMemberAccess.Name;

        initialModel.GetSymbolInfo(initialMemberName).Symbol?.Name.ShouldBe("Name");

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class User(Name: string)

                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(user: User) -> string {
                        return user.Name
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedMemberAccess = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single();
        var updatedMemberName = (IdentifierNameSyntax)updatedMemberAccess.Name;

        instrumentation.BinderReentry.Reset();

        var updatedInfo = updatedModel.GetSymbolInfo(updatedMemberName);

        updatedInfo.Symbol?.Name.ShouldBe("Name");
        updatedModel.GetMatchedExecutableOwnerForTesting(updatedMemberName).ShouldNotBeNull();
        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedMemberName).ShouldBeTrue();
        instrumentation.BinderReentry.GetBindExecutionCount(updatedMemberName).ShouldBe(0);
        instrumentation.BinderReentry.TotalBindExecutions.ShouldBeLessThanOrEqualTo(1);
        instrumentation.BinderReentry.GetBindExecutionCount(updatedMemberAccess).ShouldBeLessThanOrEqualTo(1);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesBinderParentAnchorDescriptors_ForMatchedExecutableOwnersInChangedSyntaxTree()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var initialDescriptor = initialModel.GetBinderParentAnchorDescriptorForTesting(initialIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var matchedOwner = updatedModel.GetMatchedExecutableOwnerForTesting(updatedIdentifier);
        var updatedDescriptor = updatedModel.GetBinderParentAnchorDescriptorForTesting(updatedIdentifier);

        matchedOwner.ShouldNotBeNull();
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_TransfersBinderParentAnchorDescriptors_ForStructuralBinders_WhenMatchedOwnerReused()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        return value
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

        _ = initialModel.GetBinder(initialStableMethod);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        return value
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedStableMethod = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");

        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedStableMethod).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_ReusesInterestBindingRootDescriptors_ForMatchedExecutableOwnersInChangedSyntaxTree()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var initialDescriptor = initialModel.GetInterestBindingRootDescriptorForTesting(initialIdentifier, includeExtendedExecutableRoots: false);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        if value > 0 {
                            return value
                        }

                        return 0
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");
        var matchedOwner = updatedModel.GetMatchedExecutableOwnerForTesting(updatedIdentifier);
        var updatedDescriptor = updatedModel.GetInterestBindingRootDescriptorForTesting(updatedIdentifier, includeExtendedExecutableRoots: false);

        matchedOwner.ShouldNotBeNull();
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_ReusesFunctionExpressionRebindRootDescriptors_ForMatchedExecutableOwnersInChangedSyntaxTree()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        return projection(value)
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialFunction = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var initialDescriptor = initialModel.GetFunctionExpressionRebindRootDescriptorForTesting(initialFunction);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        return projection(value)
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedFunction = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable")
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var matchedOwner = updatedModel.GetMatchedExecutableOwnerForTesting(updatedFunction);
        var updatedDescriptor = updatedModel.GetFunctionExpressionRebindRootDescriptorForTesting(updatedFunction);

        matchedOwner.ShouldNotBeNull();
        updatedDescriptor.ShouldBe(initialDescriptor);
    }

    [Fact]
    public void WorkspaceCompilation_DoesNotMatchNestedFunctionExpressions_WhenEnclosingMethodChanged()
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
                    func Main() -> int {
                        val minAge = 22
                        val predicate = func user: int -> bool => user > minAge
                        if predicate(30) {
                            return 1
                        }

                        return 0
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialFunction = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var initialDescriptor = initialModel.GetFunctionExpressionRebindRootDescriptorForTesting(initialFunction);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Main() -> int {
                        val minAge = 24
                        val predicate = func user: int -> bool => user > minAge
                        if predicate(30) {
                            return 1
                        }

                        return 0
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedFunction = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var updatedMinAgeIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "minAge");

        var matchedOwner = updatedModel.GetMatchedExecutableOwnerForTesting(updatedFunction);
        matchedOwner.ShouldBeNull();
        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedFunction).ShouldBeTrue();
        updatedCompilation.HasTransferredFunctionExpressionRebindRootDescriptorForTesting(updatedFunction).ShouldBeFalse();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedMinAgeIdentifier).ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_TransfersNestedFunctionRebindRoot_WhenParentBodyDeclarationEditIsAfterFunction()
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
                        val projection = func item: int -> int => item + value
                        val changed = 1
                        return projection(changed)
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialFunction = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        _ = initialModel.GetFunctionExpressionRebindRootDescriptorForTesting(initialFunction);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        val changed = 10
                        return projection(changed)
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedFunction = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();

        updatedModel.GetMatchedExecutableOwnerForTesting(updatedFunction).ShouldNotBeNull();
        updatedCompilation.HasTransferredFunctionExpressionRebindRootDescriptorForTesting(updatedFunction).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_TransfersMatchedOwnerBinderAndNodeInterestState_IntoNextIncrement()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        return projection(value)
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
        var initialFunction = initialStableMethod
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var initialValueIdentifier = initialStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");

        _ = initialModel.GetFunctionExpressionRebindRootDescriptorForTesting(initialFunction);
        _ = initialModel.GetBinderParentAnchorDescriptorForTesting(initialValueIdentifier);
        initialModel.GetSymbolInfo(initialValueIdentifier).Symbol?.Name.ShouldBe("value");
        _ = initialModel.GetNodeInterestSymbolDescriptorForTesting(initialValueIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        val projection = func item: int -> int => item + value
                        return projection(value)
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedStableMethod = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var updatedFunction = updatedStableMethod
            .DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .Single();
        var updatedValueIdentifier = updatedStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "value");

        updatedCompilation.HasTransferredFunctionExpressionRebindRootDescriptorForTesting(updatedFunction).ShouldBeTrue();
        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedValueIdentifier).ShouldBeTrue();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedValueIdentifier).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_TransfersUnaffectedOwnerRelativeState_AcrossChangedMethodBodyWithShiftedSpans()
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
                        val before = value
                        val first = 1
                        val second = value
                        return before + second
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
        var initialValueIdentifiers = initialStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(node => node.Identifier.ValueText == "value")
            .ToArray();
        var initialBeforeValueIdentifier = initialValueIdentifiers[0];
        var initialSecondValueIdentifier = initialValueIdentifiers[1];

        initialModel.GetSymbolInfo(initialBeforeValueIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetSymbolInfo(initialSecondValueIdentifier).Symbol?.Name.ShouldBe("value");
        _ = initialModel.GetBinderParentAnchorDescriptorForTesting(initialBeforeValueIdentifier);
        _ = initialModel.GetBinderParentAnchorDescriptorForTesting(initialSecondValueIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val before = value
                        val first = 10
                        val second = value
                        return before + second
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedStableMethod = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var updatedValueIdentifiers = updatedStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(node => node.Identifier.ValueText == "value")
            .ToArray();
        var updatedBeforeValueIdentifier = updatedValueIdentifiers[0];
        var updatedSecondValueIdentifier = updatedValueIdentifiers[1];

        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedStableMethod).ShouldBeTrue();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedBeforeValueIdentifier).ShouldBeTrue();
        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedBeforeValueIdentifier).ShouldBeTrue();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedSecondValueIdentifier).ShouldBeFalse();
        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedSecondValueIdentifier).ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_MethodSignatureChange_InvalidatesBodySymbolDescriptors()
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
        var initialStableMethod = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var initialValueIdentifier = initialStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        initialModel.GetSymbolInfo(initialValueIdentifier).Symbol?.Name.ShouldBe("value");
        _ = initialModel.GetNodeInterestSymbolDescriptorForTesting(initialValueIdentifier);
        _ = initialModel.GetBinderParentAnchorDescriptorForTesting(initialValueIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(item: int) -> int {
                        val copy = item
                        return copy
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedStableMethod = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var updatedItemIdentifier = updatedStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "item");

        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedStableMethod).ShouldBeTrue();
        updatedModel.GetMatchedExecutableOwnerForTesting(updatedItemIdentifier).ShouldNotBeNull();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedItemIdentifier).ShouldBeFalse();
        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedItemIdentifier).ShouldBeFalse();
        updatedModel.GetSymbolInfo(updatedItemIdentifier).Symbol?.Name.ShouldBe("item");
    }

    [Fact]
    public void WorkspaceCompilation_TransfersVisibleValueScopeDeclarations_ForMatchedExecutableOwnersInChangedSyntaxTree()
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
                        1
                    }

                    func Stable(value: int) -> int {
                        val first = value
                        val second = value
                        return second
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

        var initialDeclarations = initialModel.GetVisibleValueDeclarationsForTesting(initialBlock);
        initialDeclarations.Select(static declaration => declaration.Name).ShouldBe(["second", "first"]);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Changed() -> int {
                        3
                    }

                    func Stable(value: int) -> int {
                        val first = value
                        val second = value
                        return second
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedStableMethod = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var updatedBlock = updatedStableMethod.DescendantNodes().OfType<BlockStatementSyntax>().Single();

        updatedCompilation.HasTransferredVisibleValueScopeDeclarationsForTesting(updatedBlock).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_DoesNotTransferVisibleValueScopeDeclarations_AcrossBodyDeclarationEdit()
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
                        val first = value
                        val second = value
                        return second
                    }
                }
                """),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        var initialBlock = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<BlockStatementSyntax>()
            .Single();
        var initialBeforeValueIdentifier = initialBlock
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "value");

        initialModel.GetVisibleValueDeclarationsForTesting(initialBlock)
            .Select(static declaration => declaration.Name)
            .ShouldBe(["second", "first"]);
        _ = initialModel.GetContextualBindingRootDescriptorForTesting(initialBeforeValueIdentifier);
        _ = initialModel.GetInterestBindingRootDescriptorForTesting(initialBeforeValueIdentifier, includeExtendedExecutableRoots: false);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val first = value
                        val renamed = value
                        return renamed
                    }
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);
        var updatedBlock = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<BlockStatementSyntax>()
            .Single();
        var updatedBeforeValueIdentifier = updatedBlock
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "value");

        updatedCompilation.HasTransferredVisibleValueScopeDeclarationsForTesting(updatedBlock).ShouldBeFalse();
        updatedCompilation.HasTransferredContextualBindingRootDescriptorForTesting(updatedBeforeValueIdentifier).ShouldBeFalse();
        updatedCompilation.HasTransferredInterestBindingRootDescriptorForTesting(updatedBeforeValueIdentifier).ShouldBeFalse();
        updatedModel.GetVisibleValueDeclarationsForTesting(updatedBlock)
            .Select(static declaration => declaration.Name)
            .ShouldBe(["renamed", "first"]);
    }

    [Fact]
    public void WorkspaceCompilation_DiagnosticsAfterEdit_DoNotPoisonQueryableInvocationBinding()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "main.rav",
            SourceText.From(
                """
                import System.Linq.*

                func Main() -> () {
                    val minAge = 22
                    val query = [1, 2, 3]
                        .AsQueryable()
                        |> Where(value => value > minAge)
                        |> Select(value => value.ToString())

                    _ = query
                }
                """),
            "/tmp/main.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetCompilation(projectId);

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/main.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(
                """
                import System.Linq.*

                func Main() -> () {
                    val minAge = 24
                    val query = [1, 2, 3]
                        .AsQueryable()
                        |> Where(value => value > minAge)
                        |> Select(value => value.ToString())

                    _ = query
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedDiagnostics = updatedCompilation.GetDiagnostics();
        updatedDiagnostics.Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ShouldBeEmpty();
        Console.WriteLine($"semantic model assembly: {typeof(SemanticModel).Assembly.Location}");

        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/main.rav");
        var updatedRoot = updatedTree.GetRoot();
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);

        var queryDeclarator = updatedRoot.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(node => node.Identifier.ValueText == "query");
        var queryLocal = Assert.IsAssignableFrom<ILocalSymbol>(updatedModel.GetDeclaredSymbol(queryDeclarator));
        queryLocal.Type.TypeKind.ShouldNotBe(TypeKind.Error);

        var whereIdentifier = updatedRoot.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "Where");
        var whereSymbolInfo = updatedModel.GetSymbolInfo(whereIdentifier);
        var whereSymbol = whereSymbolInfo.Symbol ?? whereSymbolInfo.CandidateSymbols.FirstOrDefault();
        var whereInvocation = whereIdentifier.AncestorsAndSelf()
            .OfType<InvocationExpressionSyntax>()
            .First();
        var wherePipe = whereInvocation.Parent as InfixOperatorExpressionSyntax;
        var whereLambda = updatedRoot.DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .First();
        var whereLambdaParameter = whereLambda switch
        {
            SimpleFunctionExpressionSyntax simple => simple.Parameter,
            ParenthesizedFunctionExpressionSyntax parenthesized => parenthesized.ParameterList.Parameters.First(),
            _ => throw new InvalidOperationException()
        };

        Console.WriteLine($"where symbol: {whereSymbol?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        Console.WriteLine($"where candidates: {string.Join(", ", whereSymbolInfo.CandidateSymbols.Select(static s => s.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))}");
        Console.WriteLine($"where parent kind: {whereIdentifier.Parent?.Kind}");
        Console.WriteLine($"where invocation parent kind: {whereInvocation.Parent?.Kind}");
        Console.WriteLine($"where pipe operator kind: {wherePipe?.OperatorToken.Kind}");
        Console.WriteLine($"where invocation bound: {updatedModel.GetBoundNode(whereInvocation!).GetType().Name}");
        var whereBoundLambda = Assert.IsType<BoundFunctionExpression>(updatedModel.GetBoundNode(whereLambda));
        Console.WriteLine($"where lambda bound: {whereBoundLambda.GetType().Name}");
        Console.WriteLine($"where bound lambda symbol type: {whereBoundLambda.Symbol.GetType().FullName}");
        Console.WriteLine($"where bound lambda symbol: {whereBoundLambda.Symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}");
        Console.WriteLine($"where bound lambda param types: {string.Join(", ", whereBoundLambda.Parameters.Select(static p => p.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))}");
        Console.WriteLine($"where bound lambda return type: {whereBoundLambda.ReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}");
        Console.WriteLine($"where bound lambda delegate type: {whereBoundLambda.DelegateType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}");
        updatedModel.TryGetFunctionExpressionSymbol(whereLambda, out var directWhereLambdaSymbol).ShouldBeTrue();
        Console.WriteLine($"where direct lambda symbol: {directWhereLambdaSymbol?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        var wherePipeLeftType = updatedModel.GetTypeInfo(wherePipe!.Left).Type;
        Console.WriteLine($"where pipe left type: {wherePipeLeftType?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        if (wherePipeLeftType is not null)
        {
            var whereExtensions = updatedModel.LookupApplicableExtensionMembers(wherePipeLeftType, whereIdentifier, "Where");
            Console.WriteLine($"where extension candidates: {string.Join(", ", whereExtensions.InstanceMethods.Concat(whereExtensions.StaticMethods).Select(static m => m.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))}");
        }
        var whereInvocationInfo = updatedModel.GetSymbolInfo(whereInvocation!);
        Console.WriteLine($"where invocation symbol: {(whereInvocationInfo.Symbol ?? whereInvocationInfo.CandidateSymbols.FirstOrDefault())?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        var whereLambdaInfo = updatedModel.GetSymbolInfo(whereLambda);
        Console.WriteLine($"where lambda symbol: {(whereLambdaInfo.Symbol ?? whereLambdaInfo.CandidateSymbols.FirstOrDefault())?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        Console.WriteLine($"where lambda parameter symbol: {updatedModel.GetFunctionExpressionParameterSymbol(whereLambdaParameter)?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        Assert.IsAssignableFrom<IMethodSymbol>(whereSymbol);
    }

    [Fact]
    public void WorkspaceCompilation_DiagnosticsAfterEdit_ChainedQueryableInvocationRemainsBound()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "test",
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);

        project = project.AddDocument(
            "main.rav",
            SourceText.From(
                """
                import System.Linq.*

                func Main() -> () {
                    val minAge = 22
                    val query = [1, 2, 3]
                        .AsQueryable()
                        .Where(value => value > minAge)
                        .Select(value => value.ToString())

                    _ = query
                }
                """),
            "/tmp/main.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        _ = workspace.GetCompilation(projectId);

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/main.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(
                """
                import System.Linq.*

                func Main() -> () {
                    val minAge = 24
                    val query = [1, 2, 3]
                        .AsQueryable()
                        .Where(value => value > minAge)
                        .Select(value => value.ToString())

                    _ = query
                }
                """));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedDiagnostics = updatedCompilation.GetDiagnostics();
        updatedDiagnostics.Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ShouldBeEmpty();

        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/main.rav");
        var updatedRoot = updatedTree.GetRoot();
        var updatedModel = updatedCompilation.GetSemanticModel(updatedTree);

        var whereIdentifier = updatedRoot.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(node => node.Identifier.ValueText == "Where");
        var whereSymbolInfo = updatedModel.GetSymbolInfo(whereIdentifier);
        var whereSymbol = whereSymbolInfo.Symbol ?? whereSymbolInfo.CandidateSymbols.FirstOrDefault();
        var whereInvocation = whereIdentifier.AncestorsAndSelf()
            .OfType<InvocationExpressionSyntax>()
            .First();
        var whereLambda = updatedRoot.DescendantNodes()
            .OfType<FunctionExpressionSyntax>()
            .First();
        var whereBoundNode = updatedModel.GetBoundNode(whereInvocation);
        var whereInvocationInfo = updatedModel.GetSymbolInfo(whereInvocation);
        var whereLambdaInfo = updatedModel.GetSymbolInfo(whereLambda);

        Console.WriteLine($"chained where symbol: {whereSymbol?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        Console.WriteLine($"chained where candidates: {string.Join(", ", whereSymbolInfo.CandidateSymbols.Select(static s => s.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))}");
        Console.WriteLine($"chained where invocation bound: {whereBoundNode.GetType().Name}");
        Console.WriteLine($"chained where invocation symbol: {(whereInvocationInfo.Symbol ?? whereInvocationInfo.CandidateSymbols.FirstOrDefault())?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");
        Console.WriteLine($"chained where lambda symbol: {(whereLambdaInfo.Symbol ?? whereLambdaInfo.CandidateSymbols.FirstOrDefault())?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<null>"}");

        Assert.IsAssignableFrom<IMethodSymbol>(whereSymbol);
    }

    [Fact]
    public void WorkspaceCompilation_TransfersOwnerRelativeDescriptor_WhenGreenNodeSurvivesSiblingOwnerEdit()
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
                func Changed() -> int {
                    return 1
                }

                func Stable(value: int) -> int {
                    val changed = 1
                    return value + changed
                }
            }
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialValueIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        initialModel.GetSymbolInfo(initialValueIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetNodeInterestSymbolDescriptorForTesting(initialValueIdentifier).ShouldNotBeNull();

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(initialSource.Replace("return 1", "return 20", StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedValueIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");

        updatedValueIdentifier.Green.ShouldBeSameAs(initialValueIdentifier.Green);
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedValueIdentifier).ShouldBeTrue();
    }

    [Fact]
    public void WorkspaceCompilation_DropsOwnerRelativeDescriptor_WhenDescriptorGreenNodeIsEdited()
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
                func Stable(value: int) -> int {
                    return value
                }
            }
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        var initialTree = initialCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var initialValueIdentifier = initialTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "value");
        var initialModel = initialCompilation.GetSemanticModel(initialTree);
        initialModel.GetSymbolInfo(initialValueIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetNodeInterestSymbolDescriptorForTesting(initialValueIdentifier).ShouldNotBeNull();

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(initialSource.Replace("return value", "return input", StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedInputIdentifier = updatedTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "input");

        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedInputIdentifier).ShouldBeFalse();
    }

    [Fact]
    public void WorkspaceCompilation_DiagnosticsAfterEdit_ReusesDiagnosticsForMatchedUnchangedExecutableOwners()
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
                func Stable() -> int {
                    return missing
                }

                func Changed(value: int) -> int {
                    return value
                }
            }
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        var initialCompilation = workspace.GetCompilation(projectId);
        initialCompilation.GetDiagnostics()
            .ShouldContain(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(initialSource.Replace("return value", "return value + 1", StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);
        var updatedTree = updatedCompilation.SyntaxTrees.Single(tree => tree.FilePath == "/tmp/edited.rav");
        var updatedRoot = updatedTree.GetRoot();
        var updatedStableMethod = updatedRoot
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Stable");
        var updatedChangedMethod = updatedRoot
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Changed");

        updatedCompilation.HasTransferredSemanticDiagnosticsForTesting(updatedStableMethod).ShouldBeTrue();
        updatedCompilation.HasTransferredSemanticDiagnosticsForTesting(updatedChangedMethod).ShouldBeFalse();

        updatedCompilation.GetDiagnostics()
            .ShouldContain(diagnostic =>
                diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext &&
                diagnostic.Location.SourceSpan.IntersectsWith(updatedStableMethod.Span));
    }

    [Fact]
    public void WorkspaceCompilation_DiagnosticsAfterEdit_RemovingRequiredObjectCreationArgumentReportsNoOverload()
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
            val foo = Foo(
                Name: "Foo",
                Test: true
            )

            record Foo(
                val Name: string,
                val Test: bool
            )
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        workspace.GetCompilation(projectId).GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(initialSource.Replace("    Test: true\n", string.Empty, StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);

        updatedCompilation.GetDiagnostics()
            .ShouldContain(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }

    [Fact]
    public void WorkspaceCompilation_DiagnosticsAfterEdit_RemovingRecordParameterReportsNoOverloadAtStaleObjectCreationArgument()
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
            val foo = Foo(
                Name: "Foo",
                Test: true
            )

            record Foo(
                val Name: string,
                val Test: bool
            )
            """;

        project = project.AddDocument(
            "edited.rav",
            SourceText.From(initialSource),
            "/tmp/edited.rav").Project;

        workspace.TryApplyChanges(project.Solution);

        workspace.GetCompilation(projectId).GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(initialSource.Replace("    val Test: bool\n", string.Empty, StringComparison.Ordinal)));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);

        updatedCompilation.GetDiagnostics()
            .ShouldContain(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }

    [Fact]
    public void WorkspaceCompilation_DiagnosticsAfterMixedDeclarationAndBodyEdit_DoesNotReuseStaleCallerDiagnostics()
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
            val foo = Foo(
                Name: "Foo",
                Test: true
            )

            record Foo(
                val Name: string,
                val Test: bool
            )

            class Edited {
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

        workspace.GetCompilation(projectId).GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ShouldBeEmpty();

        var document = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(doc => doc.FilePath == "/tmp/edited.rav");
        var updatedSource = initialSource
            .Replace("    val Test: bool\n", string.Empty, StringComparison.Ordinal)
            .Replace("return 1", "return 2", StringComparison.Ordinal);
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            document.Id,
            SourceText.From(updatedSource));

        workspace.TryApplyChanges(updatedSolution);

        var updatedCompilation = workspace.GetCompilation(projectId);

        updatedCompilation.GetDiagnostics()
            .ShouldContain(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NoOverloadForMethod);
    }

}
