using System.Reflection;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Targets;

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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SessionUsesRegisteredHostPathsOnlyForHostAssistedImport(bool explicitReferences)
    {
        var hostOnly = CreateAssembly("host-only.dll", "1.0.0.0", "HostOnly");
        var host = new DotNetHostRuntime();
        host.RegisterMetadataAssemblyPath(AssemblySimpleName, hostOnly);

        var session = DotNetSemanticDataLoader.OpenSession(
            [MetadataReference.CreateFromFile(typeof(object).Assembly.Location)],
            explicitReferences ? new MetadataImportOptions() : null,
            host,
            previousSession: null);

        if (explicitReferences)
        {
            Assert.Throws<FileNotFoundException>(() => session.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)));
        }
        else
        {
            var assembly = session.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName));
            Assert.NotNull(assembly.GetType("Contracts.HostOnly"));
        }
    }

    [Fact]
    public void ReorderedReferencesReplaceSessionAndUseNewFirstIdentity()
    {
        var first = CreateAssembly("z.dll", "1.0.0.0", "First");
        var second = CreateAssembly("a.dll", "1.0.0.0", "Second");
        var previous = OpenSession([first, second]);
        Assert.NotNull(previous.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)).GetType("Contracts.First"));

        var current = OpenSession([second, first], previous);

        Assert.NotSame(previous, current);
        var assembly = current.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName));
        Assert.NotNull(assembly.GetType("Contracts.Second"));
        Assert.Null(assembly.GetType("Contracts.First"));
        Assert.NotNull(previous.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)).GetType("Contracts.First"));
    }

    [Fact]
    public void CompilationReorderedReferencesObserveNewSurfaceWithoutChangingPreviousSymbols()
    {
        var first = MetadataReference.CreateFromFile(CreateAssembly("z.dll", "1.0.0.0", "First"));
        var second = MetadataReference.CreateFromFile(CreateAssembly("a.dll", "1.0.0.0", "Second"));
        var core = MetadataReference.CreateFromFile(typeof(object).Assembly.Location);
        var options = CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        var previous = Compilation.Create("before", [], [core, first, second], options);
        var previousType = previous.GetTypeByMetadataName("Contracts.First");
        Assert.NotNull(previousType);
        var current = Compilation.Create("after", [], [core, second, first], options);
        current.AdoptIncrementalReuseFrom(previous);

        Assert.NotNull(current.GetTypeByMetadataName("Contracts.Second"));
        Assert.Null(current.GetTypeByMetadataName("Contracts.First"));
        Assert.Same(previousType, previous.GetTypeByMetadataName("Contracts.First"));
        Assert.Null(previous.GetTypeByMetadataName("Contracts.Second"));
    }

    [Fact]
    public void EquivalentReferenceInputsReuseSession()
    {
        var path = CreateAssembly("same.dll", "1.0.0.0", "Same");
        var previous = OpenSession([path]);
        Assert.Same(previous, OpenSession([path], previous));
    }

    [Fact]
    public void NewlyAvailableReferenceReplacesSession()
    {
        var path = Path.Combine(_directory, "new.dll");
        var previous = OpenSession([path]);
        Assert.Throws<FileNotFoundException>(() => previous.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)));
        CreateAssembly("new.dll", "1.0.0.0", "New");

        var current = OpenSession([path], previous);

        Assert.NotSame(previous, current);
        Assert.NotNull(current.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)).GetType("Contracts.New"));
    }

    [Fact]
    public void ExplicitImportsCannotReuseHostAssistedSession()
    {
        var hostOnly = CreateAssembly("host-only.dll", "1.0.0.0", "HostOnly");
        var host = new DotNetHostRuntime();
        host.RegisterMetadataAssemblyPath(AssemblySimpleName, hostOnly);
        MetadataReference[] references = [MetadataReference.CreateFromFile(typeof(object).Assembly.Location)];
        var previous = DotNetSemanticDataLoader.OpenSession(references, null, host, null);
        Assert.NotNull(previous.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)).GetType("Contracts.HostOnly"));

        var current = DotNetSemanticDataLoader.OpenSession(references, new MetadataImportOptions(), host, previous);

        Assert.NotSame(previous, current);
        Assert.Throws<FileNotFoundException>(() => current.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)));
    }

    private static DotNetMetadataSession OpenSession(string[] paths, DotNetMetadataSession? previous = null)
        => DotNetSemanticDataLoader.OpenSession(
            paths.Prepend(typeof(object).Assembly.Location).Select(path => MetadataReference.CreateFromFile(path)),
            new MetadataImportOptions(), new DotNetHostRuntime(), previous);

}
