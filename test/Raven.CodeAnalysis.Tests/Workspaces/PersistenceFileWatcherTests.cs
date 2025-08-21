using System;
using System.IO;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class PersistenceFileWatcherTests
{
    [Fact]
    public void WatcherTracksFileChanges()
    {
        var dir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(dir);
        try
        {
            var workspace = RavenWorkspace.Create();
            var projectId = workspace.AddProject("Test", filePath: Path.Combine(dir, "Test.ravenproj"));
            using var watcher = workspace.EnableFileWatching();

            var file = Path.Combine(dir, "main.rav");
            File.WriteAllText(file, "print 1");

            Assert.True(SpinWait.SpinUntil(() =>
            {
                var doc = workspace.CurrentSolution.GetProject(projectId)!
                    .Documents.FirstOrDefault(d => d.FilePath == file);
                return doc is not null && doc.Text.ToString() == "print 1";
            }, TimeSpan.FromSeconds(2)));

            File.WriteAllText(file, "print 2");
            Assert.True(SpinWait.SpinUntil(() =>
            {
                var doc = workspace.CurrentSolution.GetProject(projectId)!
                    .Documents.FirstOrDefault(d => d.FilePath == file);
                return doc is not null && doc.Text.ToString() == "print 2";
            }, TimeSpan.FromSeconds(2)));

            File.Delete(file);
            Assert.True(SpinWait.SpinUntil(() =>
            {
                return !workspace.CurrentSolution.GetProject(projectId)!
                    .Documents.Any(d => d.FilePath == file);
            }, TimeSpan.FromSeconds(2)));
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }
}
