using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class SolutionProjectPersistenceTests
{
    [Fact]
    public void SaveAndOpenSolution_RoundTripsProject()
    {
        var ws = RavenWorkspace.Create();
        var projectId = ws.AddProject("App", targetFramework: "net9.0");
        var project = ws.CurrentSolution.GetProject(projectId)!;
        var document = project.AddDocument("Program.rav", SourceText.From("System.Console.WriteLine(\"Hi\");"), "Program.rav");
        ws.TryApplyChanges(document.Project.Solution);

        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var solutionPath = Path.Combine(dir, "App.ravensln");
        ws.SaveSolution(solutionPath);

        var programPath = Path.Combine(dir, "Program.rav");
        Assert.True(File.Exists(programPath));
        Assert.Contains("WriteLine", File.ReadAllText(programPath));

        var ws2 = RavenWorkspace.Create();
        ws2.OpenSolution(solutionPath);

        var proj2 = ws2.CurrentSolution.Projects.Single();
        Assert.Equal("App", proj2.Name);
        Assert.Equal("net9.0", proj2.TargetFramework);
        Assert.Single(proj2.Documents);
        var doc2 = proj2.Documents.Single();
        Assert.Equal("Program.rav", doc2.Name);
        Assert.Equal(programPath, doc2.FilePath);
        var text = doc2.GetTextAsync().Result.ToString();
        Assert.Contains("WriteLine", text);
        var comp = ws2.GetCompilation(proj2.Id);
        Assert.NotEmpty(comp.References);
    }

    [Fact]
    public void AdHocWorkspace_SaveSolution_Throws()
    {
        var ws = new AdhocWorkspace();
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var path = Path.Combine(dir, "Test.ravensln");
        Assert.Throws<NotSupportedException>(() => ws.SaveSolution(path));
    }

    [Fact]
    public void AdHocWorkspace_SaveDocument_WritesFile()
    {
        var ws = new AdhocWorkspace();
        var solution = ws.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "App");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Program.rav", SourceText.From("print"));
        ws.TryApplyChanges(solution);
        var doc = ws.CurrentSolution.GetDocument(docId)!;
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var path = Path.Combine(dir, "Program.rav");
        doc.SaveDocument(path);
        Assert.True(File.Exists(path));
        Assert.Contains("print", File.ReadAllText(path));
    }

    [Fact]
    public void ToRavenWorkspace_CopiesProjects()
    {
        var ws = new AdhocWorkspace();
        var solution = ws.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "App");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Program.rav", SourceText.From("print"));
        ws.TryApplyChanges(solution);

        var raven = ws.ToRavenWorkspace();
        var proj = raven.CurrentSolution.Projects.Single();
        Assert.Equal("App", proj.Name);
        Assert.Single(proj.Documents);
        Assert.Equal("Program.rav", proj.Documents.Single().Name);
    }
}
