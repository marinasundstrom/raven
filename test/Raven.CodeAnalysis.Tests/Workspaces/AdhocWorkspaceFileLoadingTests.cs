using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class AdhocWorkspaceFileLoadingTests
{
    [Fact]
    public void OpenDocument_ShouldAddDocumentToProject()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var file = Path.Combine(dir, "test.rvn");
        File.WriteAllText(file, "x = 1");
        try
        {
            var ws = new AdhocWorkspace();
            var docId = ws.OpenDocument(file);
            var doc = ws.CurrentSolution.GetDocument(docId);
            Assert.NotNull(doc);
            Assert.Equal(file, doc!.FilePath);
            Assert.Equal("test.rvn", doc.Name);
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    [Fact]
    public void OpenDocument_MultipleCalls_ShouldReuseProject()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        var file1 = Path.Combine(dir, "a.rav");
        var file2 = Path.Combine(dir, "b.rvn");
        File.WriteAllText(file1, "a");
        File.WriteAllText(file2, "b");
        try
        {
            var ws = new AdhocWorkspace();
            ws.OpenDocument(file1);
            ws.OpenDocument(file2);
            var project = Assert.Single(ws.CurrentSolution.Projects);
            Assert.Equal(2, project.Documents.Count());
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    [Fact]
    public void OpenFolder_ShouldLoadRavAndRvnFiles()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        File.WriteAllText(Path.Combine(dir, "a.rav"), "a");
        File.WriteAllText(Path.Combine(dir, "b.rvn"), "b");
        try
        {
            var ws = new AdhocWorkspace();
            var projectId = ws.OpenFolder(dir);
            var project = ws.CurrentSolution.GetProject(projectId)!;
            var names = project.Documents.Select(d => d.Name).ToArray();
            Assert.Equal(2, names.Length);
            Assert.Contains("a.rav", names);
            Assert.Contains("b.rvn", names);
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }
}
