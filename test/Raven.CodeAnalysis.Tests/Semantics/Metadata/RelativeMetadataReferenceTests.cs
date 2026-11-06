using Raven.CodeAnalysis.Syntax;

using CS = Microsoft.CodeAnalysis.CSharp;
using CSApi = Microsoft.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class RelativeMetadataReferenceTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void DotNet_RelativeReferenceResolvesMetadata(bool createFromImage)
    {
        var identity = $"RelativeReference{Guid.NewGuid():N}";
        var path = Path.Combine(Path.GetTempPath(), identity + ".dll");
        var relative = Path.GetRelativePath(Environment.CurrentDirectory, path);
        var references = TestMetadataReferences.Default;
        var fixture = CS.CSharpCompilation.Create(identity,
            [CS.CSharpSyntaxTree.ParseText("public static class Library { public static int Value => 42; }")],
            references.OfType<PortableExecutableReference>().Select(r => CSApi.MetadataReference.CreateFromFile(r.FilePath!)),
            new CS.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = fixture.Emit(image, options: new CSApi.Emit.EmitOptions(metadataOnly: true, includePrivateMembers: false));
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        try
        {
            var bytes = image.ToArray();
            if (!createFromImage)
                File.WriteAllBytes(path, bytes);
            var reference = createFromImage ? MetadataReference.CreateFromImage(bytes, relative) : MetadataReference.CreateFromFile(relative);
            var tree = SyntaxTree.ParseText("class Consumer { static func Read() -> int => Library.Value }");
            var compilation = Compilation.Create("Consumer", [tree], [.. references, reference],
                CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
            Assert.Equal(identity, compilation.GetAssemblyOrModuleSymbol(reference)?.Name);
            Assert.Equal(relative, reference.FilePath);
        }
        finally
        {
            File.Delete(path);
        }
    }
}
