using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests;

public class WorkspaceTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Foo1()
    {
        var solution = new Solution();
        var projectId = ProjectId.CreateNewId();
        solution = solution.AddProject(projectId, "MyProject", "MyAssembly", LanguageNames.Raven);

        var documentId = DocumentId.CreateNewId(projectId);
        var updatedSolution = solution.AddDocument(documentId, "Test.cs", SourceText.From("class Test {}"));

        testOutputHelper.WriteLine($"Document added to project: {updatedSolution.Projects.First().Documents.First().Name}");
    }

    [Fact]
    public async Task Foo2()
    {
        // Create an AdhocWorkspace
        var workspace = new AdhocWorkspace();

        workspace.WorkspaceChanged += (sender, args) =>
        {
            if (args.Kind == WorkspaceChangeKind.DocumentChanged)
            {
                var newDocument = args.NewSolution.GetDocument(args.DocumentId.GetValueOrDefault());
                var newText = newDocument.GetTextAsync().Result;
                testOutputHelper.WriteLine("Document text changed!");
            }
        };

        // Create a new Solution
        var solution = workspace.CurrentSolution;

        // Add a project
        var projectId = ProjectId.CreateNewId();
        solution = solution.AddProject(projectId, "MyProject", "MyProject.dll", LanguageNames.Raven);

        workspace.TryApplyChanges(solution);

        // Add a document to the project
        var documentId = DocumentId.CreateNewId(projectId);
        solution = solution.AddDocument(documentId, "MyDocument.cs", SourceText.From("class C {}"));

        workspace.TryApplyChanges(solution);

        // Retrieve the document
        var document = workspace.CurrentSolution.GetDocument(documentId);
        testOutputHelper.WriteLine((await document.GetTextAsync()).ToString());
    }

    [Fact]
    public async Task Foo3()
    {
        // Create an AdhocWorkspace
        var workspace = new AdhocWorkspace();

        workspace.WorkspaceChanged += (sender, args) =>
        {
            /*
            if (args.Kind == WorkspaceChangeKind.DocumentChanged)
            {
                var newDocument = args.NewSolution.GetDocument(args.DocumentId.GetValueOrDefault());
                var newText = newDocument.GetTextAsync().Result;
                testOutputHelper.WriteLine("Document text changed!");
            }
            */

            testOutputHelper.WriteLine(args.Kind.ToString());
        };

        // Create initial solution and add a project
        var solution = workspace.CurrentSolution;
        var projectId = ProjectId.CreateNewId();
        solution = solution.AddProject(projectId, "MyProject", "MyAssembly", LanguageNames.Raven);

        workspace.TryApplyChanges(solution);

        // Add a document
        var documentId = DocumentId.CreateNewId(projectId);
        solution = solution.AddDocument(documentId, "Test.cs", SourceText.From("Console.WriteLine(2);"), filePath: "Test.cs");

        var updatedDocument = solution.GetDocument(documentId);

        var tree = await updatedDocument.GetSyntaxTreeAsync();

        workspace.TryApplyChanges(solution);

        // Update the document
        var updatedText = SourceText.From("Console.WriteLine(\"test\");");
        var updatedSolution = solution.UpdateDocument(documentId, updatedText, "UpdatedTest.cs");

        workspace.TryApplyChanges(updatedSolution);

        // Verify the update
        var updatedDocument2 = updatedSolution.GetDocument(documentId);

        var tree2 = await updatedDocument2.GetSyntaxTreeAsync();

        testOutputHelper.WriteLine($"Updated Document: {updatedDocument2.Name}");
        testOutputHelper.WriteLine((await updatedDocument2.GetTextAsync()).ToString());
    }
}
