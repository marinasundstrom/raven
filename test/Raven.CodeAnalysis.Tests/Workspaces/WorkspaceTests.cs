using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class WorkspaceTest
{
    [Fact]
    public void AddProjectAndDocument_ShouldPreserveIds()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "MyProject");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Test.rvn", SourceText.From("class Test {}"));
        workspace.TryApplyChanges(solution);

        var retrieved = workspace.CurrentSolution.GetDocument(docId);
        Assert.Equal("Test.rvn", retrieved?.Name);
        Assert.Equal(docId, retrieved?.Id);
    }

    [Fact]
    public async Task UpdateDocumentText_ShouldCreateNewVersion()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "Project");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Main.rvn", SourceText.From("print(1);"));
        workspace.TryApplyChanges(solution);

        var originalDoc = workspace.CurrentSolution.GetDocument(docId)!;
        var originalVersion = originalDoc.Version;
        var originalProjectVersion = workspace.CurrentSolution.GetProject(projectId)!.Version;
        var originalSolutionVersion = workspace.CurrentSolution.Version;

        var updatedText = SourceText.From("print(2);");
        var updatedDoc = originalDoc.WithText(updatedText);
        solution = solution.WithDocument(updatedDoc);
        workspace.TryApplyChanges(solution);

        var finalDoc = workspace.CurrentSolution.GetDocument(docId)!;
        Assert.Equal(docId, finalDoc.Id);
        Assert.NotEqual(originalVersion, finalDoc.Version);
        Assert.NotEqual(originalProjectVersion, workspace.CurrentSolution.GetProject(projectId)!.Version);
        Assert.NotEqual(originalSolutionVersion, workspace.CurrentSolution.Version);
        Assert.Equal(updatedText.ToString(), (await finalDoc.GetTextAsync()).ToString());
    }

    [Fact]
    public void WorkspaceEvents_ShouldRaiseOnDocumentChange()
    {
        var triggered = false;
        var workspace = new AdhocWorkspace();
        workspace.WorkspaceChanged += (_, args) =>
        {
            if (args.Kind == WorkspaceChangeKind.DocumentChanged)
                triggered = true;
        };

        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Code.rvn", SourceText.From("x = 1"));
        workspace.TryApplyChanges(solution);

        var doc = workspace.CurrentSolution.GetDocument(docId)!;
        var updated = doc.WithText(SourceText.From("x = 2"));
        var newSolution = solution.WithDocument(updated);
        workspace.TryApplyChanges(newSolution);

        Assert.True(triggered);
    }

    [Fact]
    public void GetCompilation_ShouldReuseUnchangedTrees()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var doc1 = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(doc1, "A.rvn", SourceText.From("a = 1"));

        var doc2 = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(doc2, "B.rvn", SourceText.From("b = 2"));

        workspace.TryApplyChanges(solution);

        var comp1 = workspace.GetCompilation(projectId);
        var treeB1 = comp1.SyntaxTrees.Single(t => t.FilePath == "B.rvn");

        var comp2 = workspace.GetCompilation(projectId);
        Assert.Same(comp1, comp2);

        // update first document only
        var doc = workspace.CurrentSolution.GetDocument(doc1)!;
        var updated = doc.WithText(SourceText.From("a = 3"));
        solution = workspace.CurrentSolution.WithDocument(updated);
        workspace.TryApplyChanges(solution);

        var comp3 = workspace.GetCompilation(projectId);
        Assert.NotSame(comp1, comp3);
        var treeB2 = comp3.SyntaxTrees.Single(t => t.FilePath == "B.rvn");
        Assert.Same(treeB1, treeB2);
    }

    [Fact]
    public async Task GetCompilation_ShouldPreserveSyntaxTreeInstanceWhenVersionMatchesAcrossSolutionSnapshots()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Main.rvn", SourceText.From("func Main() -> () { }"), "Main.rvn");
        workspace.TryApplyChanges(solution);

        var originalDocument = workspace.CurrentSolution.GetDocument(docId)!;
        var originalTree = await originalDocument.GetSyntaxTreeAsync();
        var compilation1 = workspace.GetCompilation(projectId);
        Assert.Same(originalTree, Assert.Single(compilation1.SyntaxTrees));

        solution = workspace.CurrentSolution.WithCompilationOptions(projectId, new CompilationOptions(OutputKind.ConsoleApplication));
        workspace.TryApplyChanges(solution);

        var reparsedDocument = workspace.CurrentSolution.GetDocument(docId)!;
        var reparsedTree = await reparsedDocument.GetSyntaxTreeAsync();
        Assert.Same(originalTree, reparsedTree);

        var compilation2 = workspace.GetCompilation(projectId);
        var compilationTree = Assert.Single(compilation2.SyntaxTrees);

        Assert.Same(originalTree, compilationTree);
        Assert.Same(compilation2.GetSemanticModel(reparsedTree!), compilation2.GetSemanticModel(compilationTree));
    }

    [Fact]
    public void WorkspaceEvents_ShouldRaiseOnProjectAdded()
    {
        WorkspaceChangeEventArgs? args = null;
        var workspace = new AdhocWorkspace();
        workspace.WorkspaceChanged += (_, e) => args = e;

        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        var newSolution = solution.AddProject(projectId, "P");
        workspace.TryApplyChanges(newSolution);

        Assert.NotNull(args);
        Assert.Equal(WorkspaceChangeKind.ProjectAdded, args!.Kind);
        Assert.Equal(projectId, args.ProjectId);
        Assert.Null(args.DocumentId);
    }

    [Fact]
    public void WorkspaceEvents_ShouldRaiseOnDocumentAdded()
    {
        WorkspaceChangeEventArgs? args = null;
        var workspace = new AdhocWorkspace();
        workspace.WorkspaceChanged += (_, e) => args = e;

        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        workspace.TryApplyChanges(solution);

        var docId = DocumentId.CreateNew(projectId);
        var newSolution = solution.AddDocument(docId, "Code.rvn", SourceText.From("x = 1"));
        workspace.TryApplyChanges(newSolution);

        Assert.NotNull(args);
        Assert.Equal(WorkspaceChangeKind.DocumentAdded, args!.Kind);
        Assert.Equal(projectId, args.ProjectId);
        Assert.Equal(docId, args.DocumentId);
    }

    [Fact]
    public void TryApplyChanges_SameSolution_ShouldNotRaiseEvents()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var triggered = false;
        workspace.WorkspaceChanged += (_, __) => triggered = true;

        workspace.TryApplyChanges(solution);

        Assert.False(triggered);
    }

    [Fact]
    public void GetCompilation_ShouldIncludeMetadataReferences()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");
        var mscorlib = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        solution = solution.AddMetadataReference(projectId, mscorlib);
        workspace.TryApplyChanges(solution);

        var compilation = workspace.GetCompilation(projectId);
        Assert.Contains(mscorlib, compilation.References);
    }

    [Fact]
    public void GetCompilation_ShouldIncludeProjectReferences()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var libId = ProjectId.CreateNew(solution.Id);
        var appId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(libId, "Lib");
        solution = solution.AddProject(appId, "App");
        solution = solution.AddProjectReference(appId, new ProjectReference(libId));
        workspace.TryApplyChanges(solution);

        var libComp = workspace.GetCompilation(libId);
        var appComp = workspace.GetCompilation(appId);

        var refComp = Assert.Single(appComp.References.OfType<CompilationReference>());
        Assert.Same(libComp, refComp.Compilation);
    }

    [Fact]
    public void GetCompilation_CircularProjectReference_ShouldThrow()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;

        var aId = ProjectId.CreateNew(solution.Id);
        var bId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(aId, "A");
        solution = solution.AddProject(bId, "B");
        solution = solution.AddProjectReference(aId, new ProjectReference(bId));
        solution = solution.AddProjectReference(bId, new ProjectReference(aId));
        workspace.TryApplyChanges(solution);

        Assert.Throws<InvalidOperationException>(() => workspace.GetCompilation(aId));
    }

    [Fact]
    public void GetRefactorings_ReturnsRegisteredContextActions()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From("func Main() -> () { }"));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new TestRefactoringProvider()],
            new TextSpan(0, 4));

        var refactoring = Assert.Single(refactorings);
        Assert.Equal(documentId, refactoring.DocumentId);
        Assert.Equal("Test refactoring", refactoring.Action.Title);
        Assert.Equal(new TextSpan(0, 4), refactoring.Span);
    }

    [Fact]
    public void GetRefactorings_IgnoresProviderFailures()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From("func Main() -> () { }"));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new ThrowingRefactoringProvider(), new TestRefactoringProvider()],
            new TextSpan(0, 4));

        var refactoring = Assert.Single(refactorings);
        Assert.Equal("Test refactoring", refactoring.Action.Title);
    }

    [Fact]
    public async Task GetRefactorings_ExpressionBodyProvider_RewritesSelectedDeclaration()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var code = "func Value() -> int => 1";
        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new ExpressionBodyToBlockBodyRefactoringProvider()],
            new TextSpan(code.IndexOf("=>", StringComparison.Ordinal), 2));

        var refactoring = Assert.Single(refactorings);
        var updated = refactoring.Action.GetChangedSolution(workspace.CurrentSolution);
        var updatedText = await updated.GetDocument(documentId)!.GetTextAsync();

        Assert.Equal("func Value() -> int {\n    return 1\n}", updatedText.ToString());
    }

    [Fact]
    public void GetRefactorings_ExpressionBodyProvider_IsAvailableFromDeclarationHeader()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var code = "func Value() -> int => 1";
        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new ExpressionBodyToBlockBodyRefactoringProvider()],
            new TextSpan(code.IndexOf("Value", StringComparison.Ordinal), 0));

        Assert.Contains(refactorings, refactoring => refactoring.Action.Title == "Convert to block body");
    }

    [Fact]
    public async Task GetRefactorings_SingleStatementBlockProvider_RewritesSelectedDeclaration()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var code = "func Value() -> int { return 1 }";
        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new SingleStatementBlockBodyRefactoringProvider()],
            new TextSpan(code.IndexOf("return", StringComparison.Ordinal), 6));

        var refactoring = Assert.Single(refactorings);
        var updated = refactoring.Action.GetChangedSolution(workspace.CurrentSolution);
        var updatedText = await updated.GetDocument(documentId)!.GetTextAsync();

        Assert.Equal("func Value() -> int => 1", updatedText.ToString());
    }

    [Fact]
    public void GetRefactorings_SingleStatementBlockProvider_IsAvailableFromDeclarationHeader()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var code = "func Value() -> int {\n    return 1\n}";
        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new SingleStatementBlockBodyRefactoringProvider()],
            new TextSpan(code.IndexOf("Value", StringComparison.Ordinal), 0));

        Assert.Contains(refactorings, refactoring => refactoring.Action.Title == "Convert to expression body");
    }

    [Fact]
    public async Task GetRefactorings_StringConcatenationProvider_RewritesSelectedExpression()
    {
        var workspace = new AdhocWorkspace();
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "P");

        var code = "func Main() -> () {\n    val text = \"a\" + \"b\"\n}";
        var documentId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(documentId, "Main.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var refactorings = workspace.GetRefactorings(
            documentId,
            [new StringConcatenationRefactoringProvider()],
            new TextSpan(code.IndexOf("+", StringComparison.Ordinal), 1));

        var refactoring = Assert.Single(refactorings);
        var updated = refactoring.Action.GetChangedSolution(workspace.CurrentSolution);
        var updatedText = await updated.GetDocument(documentId)!.GetTextAsync();

        Assert.Equal("func Main() -> () {\n    val text = \"ab\"\n}", updatedText.ToString());
    }

    private sealed class TestRefactoringProvider : CodeRefactoringProvider
    {
        public override void RegisterRefactorings(CodeRefactoringContext context)
        {
            context.RegisterRefactoring(CodeAction.Create("Test refactoring", static (solution, _) => solution));
        }
    }

    private sealed class ThrowingRefactoringProvider : CodeRefactoringProvider
    {
        public override void RegisterRefactorings(CodeRefactoringContext context)
        {
            throw new NotSupportedException("boom");
        }
    }
}
