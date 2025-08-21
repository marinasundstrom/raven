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
}
