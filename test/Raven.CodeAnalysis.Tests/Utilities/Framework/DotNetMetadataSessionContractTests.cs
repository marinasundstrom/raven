using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Tests;

public sealed partial class DotNetMetadataResolutionTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FailedPathLoadCanResolveSuppliedFallbackIdentity(bool corrupt)
    {
        var valid = CreateAssembly("valid.dll", "1.0.0.0", "Valid");
        var session = DotNetMetadataSession.Create(
            DotNetMetadataReferenceSet.Create([typeof(object).Assembly.Location, valid]), typeof(object).Assembly.GetName().Name);
        var unavailable = Path.Combine(_directory, "unavailable.dll");
        if (corrupt)
            File.WriteAllText(unavailable, "not a managed assembly");

        var assembly = session.LoadFromPath(unavailable, AssemblyName.GetAssemblyName(valid));

        Assert.NotNull(assembly.GetType("Contracts.Valid"));
    }

    [Fact]
    public void MissingPathWithoutFallbackReportsMissingFile()
    {
        var session = DotNetMetadataSession.Create(
            DotNetMetadataReferenceSet.Create([typeof(object).Assembly.Location]), typeof(object).Assembly.GetName().Name);

        Assert.Throws<FileNotFoundException>(() => session.LoadFromPath(Path.Combine(_directory, "missing.dll"), null));
    }

    [Fact]
    public void MalformedPathWithoutFallbackReportsInvalidImage()
    {
        var corrupt = Path.Combine(_directory, "corrupt.dll");
        File.WriteAllText(corrupt, "not a managed assembly");
        var session = DotNetMetadataSession.Create(
            DotNetMetadataReferenceSet.Create([typeof(object).Assembly.Location]), typeof(object).Assembly.GetName().Name);

        Assert.Throws<BadImageFormatException>(() => session.LoadFromPath(corrupt, null));
    }

    public interface IRow<T> { }
    public sealed class Row : IRow<string?> { }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MetadataSessionRetainsCapturedInterfacesAfterDeletingTheInput(bool loadByName)
    {
        var path = Path.Combine(_directory, "row.dll");
        File.Copy(typeof(Row).Assembly.Location, path);
        var references = DotNetMetadataReferenceSet.Create([typeof(object).Assembly.Location, Assembly.Load("System.Runtime").Location, path]);
        var session = DotNetMetadataSession.Create(references, typeof(object).Assembly.GetName().Name);
        var assembly = loadByName ? session.LoadFromAssemblyName(AssemblyName.GetAssemblyName(path))
            : session.LoadFromPath(path, null);
        File.Delete(path);
        var row = assembly.GetType(typeof(Row).FullName!, true)!.GetTypeInfo();
        var declaration = Assert.Single(session.ReadInterfaces(row));
        Assert.Equal(row.GetInterfaces().Single(), declaration.Type);
        Assert.Contains((byte)2, declaration.NullableFlags);
    }

    [Fact]
    public void ReuseRequiresMatchingCoreNameAndCapturedInputs()
    {
        var name = typeof(object).Assembly.GetName().Name!;
        string[] paths = [typeof(object).Assembly.Location];
        var inputs = DotNetMetadataInputSnapshot.Capture(paths, null);
        var references = DotNetMetadataReferenceSet.Create(paths);
        var session = DotNetMetadataSession.Create(references, name, inputs);
        Assert.True(session.CanReuse(DotNetMetadataInputSnapshot.Capture(paths, null), name));
        Assert.False(session.CanReuse(inputs, "Different"));
        Assert.False(session.CanReuse(DotNetMetadataInputSnapshot.Capture([], null), name));
        Assert.False(DotNetMetadataSession.Create(references, name).CanReuse(inputs, name));
        Assert.Equal(name, session.CoreAssembly.GetName().Name);
    }
}
