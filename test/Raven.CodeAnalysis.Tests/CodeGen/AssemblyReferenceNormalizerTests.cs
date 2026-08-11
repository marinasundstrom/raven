using Mono.Cecil;

using Raven.CodeAnalysis.CodeGen;

namespace Raven.CodeAnalysis.Tests;

public sealed class AssemblyReferenceNormalizerTests
{
    [Fact]
    public void NormalizeCoreLibReference_DoesNotResolveReferencedAssemblies()
    {
        using var input = File.OpenRead(typeof(Compilation).Assembly.Location);
        using var output = new MemoryStream();
        using var resolver = new ThrowingAssemblyResolver();

        AssemblyReferenceNormalizer.NormalizeCoreLibReference(input, output, resolver);

        Assert.NotEqual(0, output.Length);
    }

    [Fact]
    public void RetargetCoreLibraryReference_RewritesHostCoreScopesToTargetIdentity()
    {
        using var input = File.OpenRead(typeof(Compilation).Assembly.Location);
        using var output = new MemoryStream();
        using var resolver = new ThrowingAssemblyResolver();
        var targetIdentity = new AssemblyNameReference("mscorlib", new Version(1, 17, 11, 0));

        AssemblyReferenceNormalizer.RetargetCoreLibraryReference(
            input,
            output,
            targetIdentity,
            resolver);

        output.Position = 0;
        using var retargeted = AssemblyDefinition.ReadAssembly(output);
        var references = retargeted.MainModule.AssemblyReferences;

        Assert.Contains(references, reference => reference.FullName == targetIdentity.FullName);
        Assert.DoesNotContain(references, reference => reference.Name == "System.Private.CoreLib");
        Assert.DoesNotContain(references, reference => reference.Name == "System.Runtime");
        Assert.DoesNotContain(
            retargeted.MainModule.GetTypeReferences(),
            type => type.Scope is AssemblyNameReference reference &&
                    (reference.Name == "System.Private.CoreLib" || reference.Name == "System.Runtime"));
    }

    private sealed class ThrowingAssemblyResolver : IAssemblyResolver
    {
        public AssemblyDefinition Resolve(AssemblyNameReference name)
            => throw new InvalidOperationException($"Unexpected resolution of '{name}'.");

        public AssemblyDefinition Resolve(
            AssemblyNameReference name,
            ReaderParameters parameters)
            => throw new InvalidOperationException($"Unexpected resolution of '{name}'.");

        public void Dispose()
        {
        }
    }
}
