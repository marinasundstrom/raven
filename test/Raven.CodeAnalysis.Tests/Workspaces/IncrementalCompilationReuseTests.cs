using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class IncrementalCompilationReuseTests
{
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
        instrumentation.BinderReentry.TotalBindExecutions.ShouldBe(0);
        instrumentation.BinderReentry.GetBindExecutionCount(updatedStableIdentifier).ShouldBe(0);
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
        var initialValueIdentifiers = initialStableMethod
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(node => node.Identifier.ValueText == "value")
            .ToArray();
        var initialFirstValueIdentifier = initialValueIdentifiers[0];
        var initialSecondValueIdentifier = initialValueIdentifiers[1];

        initialModel.GetSymbolInfo(initialFirstValueIdentifier).Symbol?.Name.ShouldBe("value");
        initialModel.GetSymbolInfo(initialSecondValueIdentifier).Symbol?.Name.ShouldBe("value");
        _ = initialModel.GetBinderParentAnchorDescriptorForTesting(initialSecondValueIdentifier);

        var editedDocument = workspace.CurrentSolution.GetProject(projectId)!.Documents.Single(document => document.FilePath == "/tmp/edited.rav");
        var updatedSolution = workspace.CurrentSolution.WithDocumentText(
            editedDocument.Id,
            SourceText.From(
                """
                class Edited {
                    func Stable(value: int) -> int {
                        val first = value + 10
                        val second = value
                        return second
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
        var updatedFirstValueIdentifier = updatedValueIdentifiers[0];
        var updatedSecondValueIdentifier = updatedValueIdentifiers[1];

        updatedModel.IsExecutableOwnerMarkedChangedForTesting(updatedStableMethod).ShouldBeTrue();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedFirstValueIdentifier).ShouldBeTrue();
        updatedCompilation.HasTransferredNodeInterestSymbolDescriptorForTesting(updatedSecondValueIdentifier).ShouldBeTrue();
        updatedCompilation.HasTransferredBinderParentAnchorDescriptorForTesting(updatedSecondValueIdentifier).ShouldBeTrue();
    }

}
