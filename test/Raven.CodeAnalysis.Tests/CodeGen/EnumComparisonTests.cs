using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class EnumComparisonTests
{
    [Theory]
    [InlineData("int", "-1", "1")]
    [InlineData("uint", "0", "2147483648")]
    [InlineData("long", "-1", "1")]
    [InlineData("ulong", "0", "9223372036854775808")]
    public void RelationalOperatorsUseUnderlyingSignedness(string underlyingType, string low, string high)
    {
        var tree = SyntaxTree.ParseText($$"""
            public enum Order : {{underlyingType}} { Low, High }
            public class Probe {
                static func Compare(low: Order, high: Order) -> bool =>
                    low < high && high > low &&
                    low <= high && high >= low &&
                    low <= low && high >= high &&
                    !(high < low) && !(low > high) &&
                    !(high <= low) && !(low >= high)
            }
            """);
        var compilation = Compilation.Create("EnumComparison", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var enumType = loaded.Assembly.GetType("Order")!;
        var storageType = Enum.GetUnderlyingType(enumType);
        var lowValue = Enum.ToObject(enumType, Convert.ChangeType(low, storageType, System.Globalization.CultureInfo.InvariantCulture));
        var highValue = Enum.ToObject(enumType, Convert.ChangeType(high, storageType, System.Globalization.CultureInfo.InvariantCulture));
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Compare")!.Invoke(null, [lowValue, highValue]));
    }
}
