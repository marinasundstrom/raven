using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics;

public class QualifiedTypeAccessibilityTests
{
    [Theory]
    [InlineData("Example.Hidden.Value()")]
    [InlineData("Example.Visible.Value()")]
    public void QualifiedExternalTypeAccessRespectsVisibility(string expression)
    {
        var library = Compilation.Create("VisibilityLibrary", [SyntaxTree.ParseText("""
            namespace Example {
                internal static class Hidden {
                    public static func Value() -> int { 42 }
                }
                public static class Visible {
                    public static func Value() -> int { Hidden.Value() }
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = library.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var consumer = Compilation.Create("VisibilityConsumer", [SyntaxTree.ParseText($"func Main() -> int {{ {expression} }}")],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(image.ToArray())], new CompilationOptions(OutputKind.ConsoleApplication));
        var errors = consumer.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (expression.Contains("Hidden"))
            Assert.Contains(errors, d => d.Id == "RAV0500");
        else
            Assert.Empty(errors);
    }

}
