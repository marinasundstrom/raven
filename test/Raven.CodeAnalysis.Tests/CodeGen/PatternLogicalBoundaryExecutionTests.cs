using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class PatternLogicalBoundaryExecutionTests
{
    [Theory]
    [InlineData("abc", 0, 3)]
    [InlineData("([]{})", 3, 3)]
    [InlineData("[x]", 1, 2)]
    public void PatternConditionsRetainShortCircuitAndIncrementBehavior(string text, int expectedOpen, int expectedOther)
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Open(text: string) -> int {
                    var depth = 0
                    for ch in text {
                        if ch is '(' or '[' or '{' && ++depth > 128 { return -1 }
                    }
                    return depth
                }
                static func Other(text: string) -> int {
                    var count = 0
                    for ch in text {
                        if ch is ')' or ']' or '}' || ++count > 128 { continue }
                    }
                    return count
                }
            }
            """);
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(MetadataReference.CreateFromFile).ToArray();
        var compilation = Compilation.Create("PatternLogicalBoundary", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var probe = loaded.Assembly.GetType("Probe")!;
        Assert.Equal(expectedOpen, probe.GetMethod("Open")!.Invoke(null, [text]));
        Assert.Equal(expectedOther, probe.GetMethod("Other")!.Invoke(null, [text]));
    }
}
