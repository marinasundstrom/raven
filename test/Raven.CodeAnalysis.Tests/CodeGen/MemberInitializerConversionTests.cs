using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MemberInitializerConversionTests
{
    [Theory]
    [InlineData("field", false, false)]
    [InlineData("field", false, true)]
    [InlineData("field", true, true)]
    [InlineData("val", false, false)]
    [InlineData("val", false, true)]
    [InlineData("val", true, true)]
    public void DotNet_InitializerAppliesImplicitConversionOnce(string keyword, bool diagnosticsFirst, bool operationFirst)
    {
        var tree = SyntaxTree.ParseText($$"""
            public class Box {
                public static field Calls: int = 0
                public static func implicit(value: Box) -> int {
                    Calls += 1
                    return 42
                }
            }
            public class Probe {
                public static {{keyword}} Value: int = Box()
                public static func Read() -> int => Value
            }
            """);
        var compilation = Compilation.Create("InitializerConversion", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var initializer = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        if (operationFirst)
            Assert.IsAssignableFrom<IObjectCreationOperation>(model.GetOperation(initializer));
        var type = model.GetTypeInfo(initializer);
        Assert.Equal("Box", type.Type?.Name);
        Assert.Equal(SpecialType.System_Int32, type.ConvertedType?.SpecialType);

        Assert.IsAssignableFrom<IObjectCreationOperation>(model.GetOperation(initializer));

        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var read = loaded.Assembly.GetType("Probe", true)!.GetMethod("Read")!;
        Assert.Equal(42, read.Invoke(null, null));
        Assert.Equal(42, read.Invoke(null, null));
        Assert.Equal(1, loaded.Assembly.GetType("Box", true)!.GetField("Calls")!.GetValue(null));
    }
}
