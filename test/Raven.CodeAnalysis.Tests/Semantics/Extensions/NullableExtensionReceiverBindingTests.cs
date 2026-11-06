using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullableExtensionReceiverBindingTests
{
    [Theory, Trait("Target", "DotNet")]
    [InlineData("value.MeasureReceiver(1)", null)]
    [InlineData("value.MeasureReceiver(\"text\")", "RAV1503")]
    [InlineData("value.Length", "RAV0402")]
    public void DotNet_SelectedOverloadDeterminesReceiverHandling(string expression, string? errorId)
        => Check(TargetPlatform.DotNet, expression, errorId);

    [Theory, Trait("Target", "NeoCLR")]
    [InlineData("value.MeasureReceiver(1)", null)]
    [InlineData("value.MeasureReceiver(\"text\")", "RAV1503")]
    [InlineData("value.Length", "RAV0402")]
    public void NeoCLR_SelectedOverloadDeterminesReceiverHandling(string expression, string? errorId)
        => Check(TargetPlatform.NeoCLR, expression, errorId);

    private static void Check(TargetPlatform target, string expression, string? errorId)
    {
        var tree = SyntaxTree.ParseText($$"""
            import Raven.CodeAnalysis.Semantics.Tests.*
            class Probe {
                static func Check(value: string?) -> int => {{expression}}
            }
            """);
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(NullableReceiverFixture).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("NullableReceiver", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        var errors = model.GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        if (errorId is null)
        {
            Assert.Empty(errors);
            var call = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
            var selected = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(call).Symbol);
            Assert.True(selected.Parameters[0].Type.IsNullable);
        }
        else
        {
            Assert.Equal(errorId, Assert.Single(errors).Id);
        }
    }
}

public static class NullableReceiverFixture
{
    public static int MeasureReceiver(this string? value, int argument) => argument;
    public static int MeasureReceiver(this string value, string argument) => value.Length + argument.Length;
}
