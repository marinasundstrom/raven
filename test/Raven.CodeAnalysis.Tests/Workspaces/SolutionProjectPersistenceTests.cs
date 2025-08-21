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
        var libId = ws.AddProject("Lib", targetFramework: "net9.0", assemblyName: "Lib", compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var appId = ws.AddProject("App", targetFramework: "net9.0");
        var libProj = ws.CurrentSolution.GetProject(libId)!;
        var appProj = ws.CurrentSolution.GetProject(appId)!;
        libProj.AddDocument("Lib.rav", SourceText.From("fn add(a:int,b:int)=a+b"), "Lib.rav");
        var appDoc = appProj.AddDocument("Program.rav", SourceText.From("System.Console.WriteLine(\"Hi\");"), "Program.rav");
        var sol = appDoc.Project.Solution.AddProjectReference(appId, new ProjectReference(libId));
        ws.TryApplyChanges(sol);

        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var solutionPath = Path.Combine(dir, "App.ravensln");
        ws.SaveSolution(solutionPath);

        var programPath = Path.Combine(dir, "Program.rav");
        Assert.True(File.Exists(programPath));
        Assert.Contains("WriteLine", File.ReadAllText(programPath));

        var ws2 = RavenWorkspace.Create();
        ws2.OpenSolution(solutionPath);

        Assert.Equal(2, ws2.CurrentSolution.Projects.Count());
        var lib2 = ws2.CurrentSolution.Projects.First(p => p.Name == "Lib");
        var app2 = ws2.CurrentSolution.Projects.First(p => p.Name == "App");
        Assert.Equal("Lib", lib2.AssemblyName);
        Assert.Single(app2.ProjectReferences);
        Assert.Equal(lib2.Id, app2.ProjectReferences.Single().ProjectId);
        var doc2 = app2.Documents.Single();
        Assert.Equal("Program.rav", doc2.Name);
        Assert.Equal(programPath, doc2.FilePath);
        var text = doc2.GetTextAsync().Result.ToString();
        Assert.Contains("WriteLine", text);
        var comp = ws2.GetCompilation(lib2.Id);
        Assert.Equal(OutputKind.DynamicallyLinkedLibrary, comp.Options.OutputKind);
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
