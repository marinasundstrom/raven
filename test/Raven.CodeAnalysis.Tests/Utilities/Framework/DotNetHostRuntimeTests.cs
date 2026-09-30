using System.Reflection;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Tests;

public class DotNetHostRuntimeTests
{
    [Fact]
    public void LocalRegisteredPathsRemainIndependentOfLaterSharedRegistration()
    {
        var name = "RavenHostPath_" + Guid.NewGuid().ToString("N");
        var firstPath = Path.Combine(Path.GetTempPath(), name, "first.dll");
        var secondPath = Path.Combine(Path.GetTempPath(), name, "second.dll");
        var first = new DotNetHostRuntime();
        var second = new DotNetHostRuntime();
        first.RegisterMetadataAssemblyPath(name, firstPath);
        second.RegisterMetadataAssemblyPath(name, secondPath);

        Assert.Contains(firstPath, first.GetHostMetadataAssemblyPaths());
        Assert.Equal(firstPath, first.GetRegisteredMetadataAssemblyPath(name));
        Assert.Equal(secondPath, second.GetRegisteredMetadataAssemblyPath(name));

        var later = new DotNetHostRuntime();
        Assert.Contains(secondPath, later.GetHostMetadataAssemblyPaths());
        Assert.Equal(secondPath, later.GetRegisteredMetadataAssemblyPath(name));
        Assert.Equal(firstPath, first.GetRegisteredMetadataAssemblyPath(name));
    }

    [Fact]
    public void RuntimeRegistrationReusesLoadedAssemblyAndResolvesItsTypes()
    {
        var host = new DotNetHostRuntime();
        var assembly = typeof(DotNetHostRuntimeTests).Assembly;
        Assert.Same(assembly, host.RegisterRuntimeAssembly(assembly, assembly.Location));
        Assert.Same(assembly, host.RegisterRuntimeAssembly(assembly));
        Assert.Same(typeof(DotNetHostRuntimeTests), host.ResolveRuntimeType(typeof(DotNetHostRuntimeTests).FullName!));
        Assert.Same(typeof(string), host.ResolveRuntimeType("System.String"));
        Assert.Null(host.ResolveRuntimeType("Raven.Missing_" + Guid.NewGuid().ToString("N")));

        var later = new DotNetHostRuntime();
        _ = later.GetHostMetadataAssemblyPaths();
        Assert.Same(typeof(DotNetHostRuntimeTests), later.ResolveRuntimeType(typeof(DotNetHostRuntimeTests).FullName!));
    }

    [Fact]
    public void MetadataCoreMapsToHostTypesWithoutBecomingAnExecutableAssembly()
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var session = DotNetMetadataSession.Create(DotNetMetadataReferenceSet.Create(paths), "System.Runtime");
        var metadataAssembly = session.CoreAssembly;
        var host = new DotNetHostRuntime();
        var runtimeAssembly = host.RegisterRuntimeAssembly(metadataAssembly);
        Assert.NotNull(runtimeAssembly);
        Assert.NotSame(metadataAssembly, runtimeAssembly);
        Assert.Same(runtimeAssembly, host.RegisterRuntimeAssembly(metadataAssembly));
        var metadataType = metadataAssembly.GetType("System.String", throwOnError: true)!.GetTypeInfo();
        Assert.Same(typeof(string), host.ResolveRuntimeType(metadataType));
        Assert.NotSame(typeof(string), metadataType);
    }
}
