using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MetadataInputSnapshotTests
{
    [Fact]
    public void DotNet_CaptureNormalizesPathsAndPreservesTheirOrderAndMultiplicity()
    {
        var a = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        var b = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        var snapshot = DotNetMetadataInputSnapshot.Capture([a, b], null);

        Assert.True(snapshot.Matches(DotNetMetadataInputSnapshot.Capture(
            [Path.GetRelativePath(Environment.CurrentDirectory, a), "  ", b], null)));
        Assert.False(snapshot.Matches(DotNetMetadataInputSnapshot.Capture([b, a], null)));
        Assert.False(snapshot.Matches(DotNetMetadataInputSnapshot.Capture([a, a, b], null)));
    }

    [Fact]
    public void DotNet_ReuseReflectsFileExistenceLengthAndWriteTime()
    {
        var path = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        DotNetMetadataInputSnapshot Capture() => DotNetMetadataInputSnapshot.Capture([path], null);
        var missing = Capture();
        try
        {
            File.WriteAllText(path, "abc");
            var original = Capture();
            var timestamp = File.GetLastWriteTimeUtc(path);
            Assert.False(missing.Matches(original));
            Assert.True(original.Matches(Capture()));

            File.WriteAllText(path, "abcd");
            File.SetLastWriteTimeUtc(path, timestamp);
            var resized = Capture();
            Assert.False(original.Matches(resized));

            File.SetLastWriteTimeUtc(path, timestamp.AddMinutes(1));
            Assert.False(resized.Matches(Capture()));
        }
        finally
        {
            File.Delete(path);
        }
        Assert.True(missing.Matches(Capture()));
    }

    [Fact]
    public void DotNet_ReuseComparesImportOptionsByValue()
    {
        var snapshot = DotNetMetadataInputSnapshot.Capture([], new MetadataImportOptions("CoreA"));
        Assert.True(snapshot.Matches(DotNetMetadataInputSnapshot.Capture([], new MetadataImportOptions("CoreA"))));
        Assert.False(snapshot.Matches(DotNetMetadataInputSnapshot.Capture([], new MetadataImportOptions("CoreB"))));
        Assert.False(snapshot.Matches(DotNetMetadataInputSnapshot.Capture([], null)));
    }
}
