using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ExternalEnumDefaultParameterTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitReferencePathSuppliesEnumDefaultMetadata(bool retarget)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-enum-reference-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var provider = Compilation.Create("ExternalDefaults", [SyntaxTree.ParseText("namespace Fixture { public enum State { Ready = 1 } }")], TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using var providerImage = new MemoryStream();
            var providerResult = provider.Emit(providerImage);
            Assert.True(providerResult.Success, string.Join(Environment.NewLine, providerResult.Diagnostics));
            // The reference identity deliberately differs from the file name.
            var path = Path.Combine(directory, "reference-payload.bin");
            File.WriteAllBytes(path, providerImage.ToArray());
            var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(path)).ToArray();
            var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
            if (retarget) options = options.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime")).WithTargetCoreAssemblyName("System.Runtime");
            var consumer = Compilation.Create("Consumer", [SyntaxTree.ParseText("""
                import Fixture.*
                public class Probe {
                    static func Read(value: State = State.Ready) -> State => value
                }
                """)], references, options);
            using var output = new MemoryStream();
            var result = consumer.Emit(output);
            Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, consumer.References);
            var method = loaded.Assembly.GetType("Probe")!.GetMethod("Read")!;
            var parameter = Assert.Single(method.GetParameters());
            Assert.True(parameter.IsOptional);
            Assert.Equal("Fixture.State", parameter.ParameterType.FullName);
            Assert.Equal(1, Convert.ToInt32(parameter.RawDefaultValue));
            Assert.Equal(1, Convert.ToInt32(method.Invoke(null, [Type.Missing])));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }
}
