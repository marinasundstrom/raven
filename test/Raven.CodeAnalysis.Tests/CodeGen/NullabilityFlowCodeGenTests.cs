using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class NullabilityFlowCodeGenTests
{
    [Fact]
    public void NarrowedNullableExpression_RemainsValidThroughLoweringAndEmission()
    {
        const string source = """
func Accept(value: object?) -> object? => value

func Test(input: string?) -> object? {
    if input is null {
        return null
    }

    let passed = Accept(input)
    return input match {
        string text => passed
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "nullability_flow_lowering",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var image = new MemoryStream();
        var result = compilation.Emit(image);

        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics.Select(diagnostic => diagnostic.ToString())));
        Assert.NotEqual(0, image.Length);
    }
}
