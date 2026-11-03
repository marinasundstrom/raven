using System.Globalization;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class EnumFlagConstantTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("byte", "254")]
    [InlineData("int", "-2")]
    [InlineData("long", "-2")]
    [InlineData("ulong", "18446744073709551614")]
    public void DotNetFlagInitializersPreserveUnderlyingValues(string underlying, string inverse)
    {
        var compilation = CreateCompilation(underlying, TargetPlatform.DotNet);
        AssertValues(compilation, inverse);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var type = assembly.GetType("Bits")!;
        Assert.Equal("3", Convert.ToString(type.GetField("Combined")!.GetRawConstantValue(), CultureInfo.InvariantCulture));
        Assert.Equal(inverse, Convert.ToString(type.GetField("Inverse")!.GetRawConstantValue(), CultureInfo.InvariantCulture));
        var inverted = assembly.GetType("Runner")!.GetMethod("Invert")!.Invoke(null, [Enum.ToObject(type, 1)]);
        Assert.Equal(inverse, Convert.ToString(Convert.ChangeType(inverted, Enum.GetUnderlyingType(type)), CultureInfo.InvariantCulture));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrFlagInitializersUseSharedConstantEvaluation()
        => AssertValues(CreateCompilation("int", TargetPlatform.NeoCLR), "-2");

    private static Compilation CreateCompilation(string underlying, TargetPlatform target)
        => Compilation.Create("EnumFlagConstants", [SyntaxTree.ParseText($$"""
            enum Bits : {{underlying}} {
                First = 1,
                Second = 2,
                Combined = First | Second,
                OnlyFirst = Combined & First,
                OnlySecond = Combined ^ First,
                Inverse = ~First,
            }
            class Runner {
                static func Invert(value: Bits) -> Bits => ~value
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

    private static void AssertValues(Compilation compilation, string inverse)
    {
        // Semantic host references do not establish native runtime execution.
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees.Single());
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var type = compilation.GetTypeByMetadataName("Bits")!;
        foreach (var (name, expected) in new[] { ("Combined", "3"), ("OnlyFirst", "1"), ("OnlySecond", "2"), ("Inverse", inverse) })
        {
            var field = Assert.Single(type.GetMembers(name).OfType<FieldSymbol>());
            Assert.Equal(expected, Convert.ToString(field.GetConstantValue(), CultureInfo.InvariantCulture));
        }
    }
}
