using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NullableParamsOverloadTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_NullableParamsArrayAcceptsExpandedArguments(bool imported)
        => Verify(TargetPlatform.DotNet, imported);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClr_NullableParamsArrayAcceptsExpandedArguments(bool imported)
        => Verify(TargetPlatform.NeoCLR, imported);

    private static void Verify(TargetPlatform target, bool imported)
    {
        var receiver = imported ? typeof(NullableParamsFixture).FullName! : "Collector";
        var tree = SyntaxTree.ParseText($$"""
            class Collector {
                static func Collect(marker: string, params values: object?[]?) {}
                static func Collect(marker: string, mode: int, params values: object?[]?) {}
            }
            class Probe {
                static func Run(value: object?) {
                    {{receiver}}.Collect("empty")
                    {{receiver}}.Collect("normal", null)
                    {{receiver}}.Collect("single", value)
                    {{receiver}}.Collect("multiple", value, value)
                }
            }
            """);
        var compilation = Compilation.Create("NullableParamsOverloads", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(NullableParamsFixture).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        Assert.Empty(compilation.GetSemanticModel(tree).GetDiagnostics()
            .Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
    }
}

public static class NullableParamsFixture
{
    public static int Collect(string marker, params object?[]? values) => values?.Length ?? -100;

    public static int Collect(string marker, int mode, params object?[]? values) => 1000;
}
