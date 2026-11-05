using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ConstantArgumentConversionTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("0", false)]
    [InlineData("255", false)]
    [InlineData("128 + 127", false)]
    [InlineData("Limit", false)]
    [InlineData("unchecked(2147483647 + 2147483647 + 2)", false)]
    [InlineData("256", true)]
    [InlineData("-1", true)]
    [InlineData("value", true)]
    public void DotNet_ByteArgumentsRequireInRangeConstants(string argument, bool rejected)
        => Verify(TargetPlatform.DotNet, argument, rejected);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("0", false)]
    [InlineData("255", false)]
    [InlineData("128 + 127", false)]
    [InlineData("Limit", false)]
    [InlineData("unchecked(2147483647 + 2147483647 + 2)", false)]
    [InlineData("256", true)]
    [InlineData("-1", true)]
    [InlineData("value", true)]
    public void NeoClr_ByteArgumentsRequireInRangeConstants(string argument, bool rejected)
        => Verify(TargetPlatform.NeoCLR, argument, rejected);

    private static void Verify(TargetPlatform target, string argument, bool rejected)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Box<T> {
                init(value: T) {}
            }
            class Probe {
                const Limit: int = 255

                static func Read(value: byte) -> byte => value

                static func Make(value: int) -> byte {
                    let box = Box<byte>({{argument}})
                    return Read({{argument}})
                }
            }
            """);
        var compilation = Compilation.Create("ConstantArgumentConversion", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics()
            .Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejected)
            Assert.NotEmpty(errors);
        else
        {
            Assert.Empty(errors);
            if (argument == "0")
            {
                foreach (var literal in tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>()
                    .Where(node => node.Token.Value is int value && value == 0))
                {
                    var info = compilation.GetSemanticModel(tree).GetTypeInfo(literal);
                    Assert.Equal(SpecialType.System_Int32, info.Type?.SpecialType);
                    Assert.Equal(SpecialType.System_Byte, info.ConvertedType?.SpecialType);
                }
            }
        }
    }
}
