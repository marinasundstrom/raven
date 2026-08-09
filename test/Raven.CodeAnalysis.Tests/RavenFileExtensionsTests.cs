namespace Raven.CodeAnalysis.Tests;

public sealed class RavenFileExtensionsTests
{
    [Theory]
    [InlineData("App.rvnproj")]
    [InlineData("App.RVNPROJ")]
    public void HasProjectExtension_AcceptsRavenMsBuildProjects(string path)
    {
        RavenFileExtensions.HasProjectExtension(path).ShouldBeTrue();
    }

    [Theory]
    [InlineData("App.ravenproj")]
    [InlineData("App.csproj")]
    [InlineData("App.rvn")]
    public void HasProjectExtension_RejectsOtherExtensions(string path)
    {
        RavenFileExtensions.HasProjectExtension(path).ShouldBeFalse();
    }
}
