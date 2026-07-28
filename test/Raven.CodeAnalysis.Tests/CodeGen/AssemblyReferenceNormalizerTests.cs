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
