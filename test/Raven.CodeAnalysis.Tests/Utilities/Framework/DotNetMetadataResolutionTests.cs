using System.Reflection;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Tests;

public sealed class DotNetMetadataResolutionTests : IDisposable
{
    private const string AssemblySimpleName = "ResolverContract";
    private readonly string _directory = Path.Combine(Path.GetTempPath(), $"raven-resolver-{Guid.NewGuid():N}");

    public DotNetMetadataResolutionTests()
    {
        Directory.CreateDirectory(_directory);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DuplicateIdentityUsesFirstInputBeforePathSorting(bool reverse)
    {
        var first = CreateAssembly("z.dll", "1.0.0.0", "First");
        var second = CreateAssembly("a.dll", "1.0.0.0", "Second");
        using var context = CreateContext(reverse ? [second, first] : [first, second]);

        var assembly = context.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName));

        Assert.NotNull(assembly.GetType(reverse ? "Contracts.Second" : "Contracts.First"));
        Assert.Null(assembly.GetType(reverse ? "Contracts.First" : "Contracts.Second"));
    }

    [Fact]
    public void ExactIdentityWinsOverSimpleNameCandidate()
    {
        var earlier = CreateAssembly("a.dll", "1.0.0.0", "Earlier");
        var requested = CreateAssembly("z.dll", "2.0.0.0", "Requested");
        using var context = CreateContext([earlier, requested]);

        var assembly = context.LoadFromAssemblyName(AssemblyName.GetAssemblyName(requested));

        Assert.Equal(new Version(2, 0, 0, 0), assembly.GetName().Version);
        Assert.NotNull(assembly.GetType("Contracts.Requested"));
    }

    [Fact]
    public void SimpleNameFallbackUsesSortedPathOrder()
    {
        var later = CreateAssembly("z.dll", "2.0.0.0", "Later");
        var earlier = CreateAssembly("a.dll", "1.0.0.0", "Earlier");
        using var context = CreateContext([later, earlier]);

        var assembly = context.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName));

        Assert.Equal(new Version(1, 0, 0, 0), assembly.GetName().Version);
        Assert.NotNull(assembly.GetType("Contracts.Earlier"));
    }

    [Fact]
    public void UnusableCandidatesDoNotPreventValidReferenceResolution()
    {
        var corrupt = Path.Combine(_directory, "corrupt.dll");
        File.WriteAllText(corrupt, "not a managed assembly");
        var valid = CreateAssembly("valid.dll", "1.0.0.0", "Valid");
        using var context = CreateContext(["", " ", "\0", Path.Combine(_directory, "missing.dll"), corrupt, valid]);

        Assert.NotNull(context.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName)).GetType("Contracts.Valid"));
        Assert.Throws<FileNotFoundException>(() => context.LoadFromAssemblyName(new AssemblyName("corrupt")));
    }

    [Fact]
    public void ReferenceSetRetainsOrderedCandidatesForHostRegistration()
    {
        var later = CreateAssembly("z.dll", "2.0.0.0", "Later");
        var earlier = CreateAssembly("a.dll", "1.0.0.0", "Earlier");
        var duplicate = CreateAssembly("duplicate.dll", "2.0.0.0", "Duplicate");

        var references = DotNetMetadataReferenceSet.Create([later, earlier, duplicate]);

        Assert.Equal(new[] { earlier, later }, references.References.Select(reference => reference.Path));
        Assert.All(references.References, reference => Assert.Equal(AssemblySimpleName, reference.SimpleName));
        Assert.Equal(AssemblyName.GetAssemblyName(later).FullName, references.References.Last().FullName);
    }

    [Fact]
    public void ReferenceSetCanBeReusedAfterInputListChanges()
    {
        var original = CreateAssembly("original.dll", "1.0.0.0", "Original");
        var replacement = CreateAssembly("replacement.dll", "1.0.0.0", "Replacement");
        var paths = new List<string> { typeof(object).Assembly.Location, original };
        var references = DotNetMetadataReferenceSet.Create(paths);
        paths[1] = replacement;

        using var first = DotNetMetadataContextFactory.Create(references, typeof(object).Assembly.GetName().Name);
        using var second = DotNetMetadataContextFactory.Create(references, typeof(object).Assembly.GetName().Name);
        foreach (var context in new[] { first, second })
        {
            var assembly = context.LoadFromAssemblyName(new AssemblyName(AssemblySimpleName));
            Assert.NotNull(assembly.GetType("Contracts.Original"));
            Assert.Null(assembly.GetType("Contracts.Replacement"));
        }
    }

    [Fact]
    public void UnlistedHostAssemblyIsNotAnImplicitReference()
    {
        using var context = CreateContext([]);

        Assert.Throws<FileNotFoundException>(() => context.LoadFromAssemblyName(typeof(Compilation).Assembly.GetName()));
    }

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

    private static MetadataLoadContext CreateContext(string[] paths)
        => DotNetMetadataContextFactory.Create(
            DotNetMetadataReferenceSet.Create(paths.Prepend(typeof(object).Assembly.Location)), typeof(object).Assembly.GetName().Name);

    private string CreateAssembly(string fileName, string version, string marker)
    {
        // These fixtures need only CLI identity and a distinct public type surface.
        // They are inspected as metadata, never loaded for host execution.
        using var assembly = Mono.Cecil.AssemblyDefinition.CreateAssembly(
            new Mono.Cecil.AssemblyNameDefinition(AssemblySimpleName, Version.Parse(version)),
            AssemblySimpleName, Mono.Cecil.ModuleKind.Dll);
        assembly.MainModule.Types.Add(new Mono.Cecil.TypeDefinition(
            "Contracts", marker, Mono.Cecil.TypeAttributes.Public | Mono.Cecil.TypeAttributes.Class,
            assembly.MainModule.TypeSystem.Object));
        var path = Path.Combine(_directory, fileName);
        assembly.Write(path);
        return path;
    }

    public void Dispose() => Directory.Delete(_directory, recursive: true);
}
