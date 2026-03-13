using OmniSharp.Extensions.LanguageServer.Protocol;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public class LanguageServerRenameTests
{
    [Fact]
    public void IsValidIdentifier_RejectsKeywordsUnlessEscaped()
    {
        RenameService.IsValidIdentifier("if").ShouldBeFalse();
        RenameService.IsValidIdentifier("@if").ShouldBeTrue();
        RenameService.IsValidIdentifier("newName").ShouldBeTrue();
        RenameService.IsValidIdentifier("1name").ShouldBeFalse();
    }

    [Fact]
    public async Task BuildWorkspaceEdit_RenamesDeclarationAndReferenceAsync()
    {
        const string code = """
class Counter {
    func Increment() -> () { }
    func Run() -> () { self.Increment() }
}
""";

        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject(
            "TestProject",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var documentId = DocumentId.CreateNew(projectId);
        var filePath = "/workspace/rename-test.rav";

        var solution = workspace.CurrentSolution.AddDocument(documentId, "rename-test.rav", SourceText.From(code), filePath);
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in LanguageServerTestReferences.Default)
            project = project.AddMetadataReference(reference);

        workspace.TryApplyChanges(project.Solution);
        project = workspace.CurrentSolution.GetProject(projectId)!;

        var document = project.GetDocument(documentId)!;
        var syntaxTree = await document.GetSyntaxTreeAsync();
        var root = syntaxTree!.GetRoot();
        var compilation = workspace.GetCompilation(projectId);
        var semanticModel = compilation!.GetSemanticModel(syntaxTree);

        var methodDeclaration = root.DescendantNodes().OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.ValueText == "Increment");
        var methodSymbol = semanticModel.GetDeclaredSymbol(methodDeclaration).ShouldBeAssignableTo<IMethodSymbol>();

        var workspaceEdit = await RenameService.BuildWorkspaceEditAsync(
            [project],
            methodSymbol,
            "Increase",
            CancellationToken.None);

        workspaceEdit.Changes.ShouldNotBeNull();

        var uri = DocumentUri.FromFileSystemPath(filePath);
        workspaceEdit.Changes!.ContainsKey(uri).ShouldBeTrue();
        var edits = workspaceEdit.Changes[uri].ToArray();
        edits.Length.ShouldBe(2);
        edits.ShouldAllBe(edit => edit.NewText == "Increase");
    }

    [Fact]
    public async Task BuildWorkspaceEdit_PrimaryConstructorParameter_UsesIdentifierOnlySpanAsync()
    {
        const string code = """
class Foo(name: string) {
    func Get() -> string => name
}
""";

        var workspace = RavenWorkspace.Create();
        var projectId = workspace.AddProject(
            "TestProject",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var documentId = DocumentId.CreateNew(projectId);
        var filePath = "/workspace/rename-primary-parameter.rav";

        var solution = workspace.CurrentSolution.AddDocument(documentId, "rename-primary-parameter.rav", SourceText.From(code), filePath);
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in LanguageServerTestReferences.Default)
            project = project.AddMetadataReference(reference);

        workspace.TryApplyChanges(project.Solution);
        project = workspace.CurrentSolution.GetProject(projectId)!;

        var document = project.GetDocument(documentId)!;
        var syntaxTree = await document.GetSyntaxTreeAsync();
        var root = syntaxTree!.GetRoot();
        var sourceText = await document.GetTextAsync();
        var compilation = workspace.GetCompilation(projectId);
        var semanticModel = compilation!.GetSemanticModel(syntaxTree);

        var classDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var parameter = classDeclaration.ParameterList!.Parameters.Single();
        var parameterSymbol = semanticModel.GetDeclaredSymbol(parameter).ShouldBeAssignableTo<IParameterSymbol>();

        var workspaceEdit = await RenameService.BuildWorkspaceEditAsync(
            [project],
            parameterSymbol,
            "firstName",
            CancellationToken.None);

        workspaceEdit.Changes.ShouldNotBeNull();

        var uri = DocumentUri.FromFileSystemPath(filePath);
        workspaceEdit.Changes!.ContainsKey(uri).ShouldBeTrue();
        var edits = workspaceEdit.Changes[uri].ToArray();
        edits.Length.ShouldBe(2);

        foreach (var edit in edits)
        {
            edit.NewText.ShouldBe("firstName");
            var start = PositionHelper.ToOffset(sourceText, edit.Range.Start);
            var end = PositionHelper.ToOffset(sourceText, edit.Range.End);
            sourceText.GetSubText(new TextSpan(start, end - start)).ShouldBe("name");
        }
    }
}
