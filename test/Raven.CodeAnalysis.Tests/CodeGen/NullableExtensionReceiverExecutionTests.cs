using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.CodeGen.Tests;

[Trait("Target", "DotNet")]
public class NullableExtensionReceiverExecutionTests
{
    [Theory]
    [InlineData(null, false)]
    [InlineData(5, true)]
    public void ImportedExtensionReceivesNullableValue(int? value, bool expected)
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.MetadataFixtures.Linq.*
            public class Probe {
                static func Check(value: int?) -> bool => value.IsPresent()
            }
            """);
        var references = TestMetadataReferences.DefaultWithoutSystemLinqWithExtensionMethods;
        var compilation = Compilation.Create("NullableReceiver", [tree], references,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var check = loaded.Assembly.GetType("Probe")!.GetMethod("Check")!;
        Assert.Equal(expected, check.Invoke(null, [value]));
    }
}
