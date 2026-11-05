using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class InterfaceReferenceConversionTests
{
    private const string Source = """
        interface Left {}
        interface Right { val Value: string? { get; } }
        class Both(val Value: string?) : Left, Right {}
        class LeftOnly : Left {}
        class Probe {
            static func Read(value: Left) -> string {
                if value is Right { Value: { } result } { return result }
                return "missing"
            }
            static func Cast(value: Left) -> Right => (Right)value
            static func Run(kind: int) -> string {
                if kind == 0 { return Read(Both("found")) }
                if kind == 1 { return Read(Both(null)) }
                return Read(LeftOnly())
            }
        }
        """;

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetUnrelatedInterfacesHaveExplicitReferenceConversions()
        => AssertConversions(TargetPlatform.DotNet);

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrUnrelatedInterfacesHaveExplicitReferenceConversions()
        // Host references exercise the shared conversion rules, not native execution.
        => AssertConversions(TargetPlatform.NeoCLR);

    private static void AssertConversions(TargetPlatform target)
    {
        var compilation = CreateCompilation(target);
        // Semantic-only neoCLR validation deliberately uses host references, without CLI emission.
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees.Single());
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var left = compilation.GetTypeByMetadataName("Left")!;
        var right = compilation.GetTypeByMetadataName("Right")!;
        foreach (var (from, to) in new[] { (left, right), (right, left) })
        {
            var conversion = compilation.ClassifyConversion(from, to, includeUserDefined: false);
            Assert.True(conversion.Exists);
            Assert.True(conversion.IsReference);
            Assert.False(conversion.IsImplicit);
        }
        // A closed class without the interface still cannot acquire it at runtime.
        var closed = compilation.GetTypeByMetadataName("LeftOnly")!;
        Assert.False(compilation.ClassifyConversion(closed, right, includeUserDefined: false).Exists);
        Assert.False(compilation.ClassifyConversion(right, closed, includeUserDefined: false).Exists);
    }

    internal static Compilation CreateCompilation(TargetPlatform target)
        => Compilation.Create("InterfaceReferenceConversions", [SyntaxTree.ParseText(Source)],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
