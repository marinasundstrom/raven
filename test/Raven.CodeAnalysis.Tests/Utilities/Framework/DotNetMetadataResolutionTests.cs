using System.Reflection;

using Raven.CodeAnalysis.Metadata;

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
            [typeof(object).Assembly.Location, valid], typeof(object).Assembly.GetName().Name,
            static (_, _) => { });
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
            [typeof(object).Assembly.Location], typeof(object).Assembly.GetName().Name,
            static (_, _) => { });

        Assert.Throws<FileNotFoundException>(() => session.LoadFromPath(Path.Combine(_directory, "missing.dll"), null));
    }

    [Fact]
    public void MalformedPathWithoutFallbackReportsInvalidImage()
    {
        var corrupt = Path.Combine(_directory, "corrupt.dll");
        File.WriteAllText(corrupt, "not a managed assembly");
        var session = DotNetMetadataSession.Create(
            [typeof(object).Assembly.Location], typeof(object).Assembly.GetName().Name,
            static (_, _) => { });

        Assert.Throws<BadImageFormatException>(() => session.LoadFromPath(corrupt, null));
    }

    private static MetadataLoadContext CreateContext(string[] paths)
        => DotNetMetadataContextFactory.Create(
            paths.Prepend(typeof(object).Assembly.Location), typeof(object).Assembly.GetName().Name,
            static (_, _) => { });

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
