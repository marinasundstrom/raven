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
    public void AdHocWorkspace_SaveDocument_WritesFile()
    {
        var ws = new AdhocWorkspace();
        var solution = ws.CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, "App");
        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, "Program.rvn", SourceText.From("print"));
        ws.TryApplyChanges(solution);
        var doc = ws.CurrentSolution.GetDocument(docId)!;
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var path = Path.Combine(dir, "Program.rvn");
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
        solution = solution.AddDocument(docId, "Program.rvn", SourceText.From("print"));
        ws.TryApplyChanges(solution);

        var raven = ws.ToRavenWorkspace();
        var proj = raven.CurrentSolution.Projects.Single();
        Assert.Equal("App", proj.Name);
        Assert.Single(proj.Documents);
        Assert.Equal("Program.rvn", proj.Documents.Single().Name);
    }

    [Fact]
    public void Project_WithCompilationOptions_UpdatesSolution()
    {
        var ws = RavenWorkspace.Create();
        var projectId = ws.AddProject("App");
        var project = ws.CurrentSolution.GetProject(projectId)!;
        var updated = project.WithCompilationOptions(new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        ws.TryApplyChanges(updated.Solution);
        var result = ws.CurrentSolution.GetProject(projectId)!;
        Assert.Equal(OutputKind.DynamicallyLinkedLibrary, result.CompilationOptions?.OutputKind);
        var comp = ws.GetCompilation(projectId);
        Assert.Equal(OutputKind.DynamicallyLinkedLibrary, comp.Options.OutputKind);
    }
}
