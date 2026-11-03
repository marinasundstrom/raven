using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen.Patterns;

public sealed class NullablePropertyCaptureTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("string", "{}", "captured.Length", "\"abc\"", 3)]
    [InlineData("string", "{ Length: > 0 }", "captured.Length", "\"abc\"", 3)]
    [InlineData("int", "{}", "captured + 1", "41", 42)]
    [InlineData("System.DateTime", "{ Year: 2026 }", "captured.Year", "System.DateTime(2026, 1, 1)", 2026)]
    public void DotNetWholePropertyCaptureIsNonNullable(string type, string pattern, string result, string value, int expected)
    {
        var compilation = CreateCompilation(type, pattern, result, value, TargetPlatform.DotNet);
        AssertCapture(compilation);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        var runner = Assembly.Load(image.ToArray()).GetType("Runner")!;
        Assert.Equal(expected, runner.GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(-1, runner.GetMethod("Read")!.Invoke(null, [null]));
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("{}")]
    [InlineData("{ Length: > 0 }")]
    public void NeoClrWholePropertyCaptureIsNonNullable(string pattern)
    {
        // Shared semantic binding with host references; no native execution.
        AssertCapture(CreateCompilation("string", pattern, "captured.Length", "\"abc\"", TargetPlatform.NeoCLR));
    }

    private static Compilation CreateCompilation(string type, string pattern, string result, string value, TargetPlatform target)
        => Compilation.Create("NullablePropertyCapture", [SyntaxTree.ParseText($$"""
            class Runner {
                static func Read(value: {{type}}?) -> int {
                    if let {{pattern}} captured = value {
                        return {{result}}
                    }
                    return -1
                }
                static func Run() -> int => Read({{value}})
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

    private static void AssertCapture(Compilation compilation)
    {
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var designation = tree.GetRoot().DescendantNodes().OfType<SingleVariableDesignationSyntax>()
            .Single(node => node.Identifier.ValueText == "captured");
        var local = Assert.IsAssignableFrom<LocalSymbol>(model.GetDeclaredSymbol(designation));
        Assert.False(local.Type.IsNullable);
    }
}
