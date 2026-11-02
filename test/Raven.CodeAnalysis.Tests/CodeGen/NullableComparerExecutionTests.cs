using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableComparerExecutionTests
{
    [Theory]
    [InlineData(null)]
    [InlineData("Raven")]
    public void NullableComparerHash_MatchesCSharp(string? value)
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            class Probe {
                static func Hash(value: string?) -> int {
                    var hash = HashCode()
                    hash.Add(value, StringComparer.Ordinal)
                    return hash.ToHashCode()
                }
            }
            """);
        var compilation = Compilation.Create("ComparerExecution", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var method = Assembly.Load(image.ToArray()).GetType("Probe")!.GetMethod("Hash")!;
        var expected = new HashCode();
        expected.Add(value, StringComparer.Ordinal);
        Assert.Equal(expected.ToHashCode(), method.Invoke(null, [value]));
    }
}
