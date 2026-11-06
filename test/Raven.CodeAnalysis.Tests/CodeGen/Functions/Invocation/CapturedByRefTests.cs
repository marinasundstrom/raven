using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class CapturedByRefTests
{
    [Theory]
    [InlineData("if !GetValue(out var value) { return -1 }; let read: () -> int = () => value; return read()")]
    [InlineData("var value = 41; let read: () -> int = () => value; Bump(ref value); return read()")]
    [InlineData("var value = initial; let bump: () -> int = () => { Bump(ref value); return value }; return bump()")]
    public void DotNet_ByRefWriteIsVisibleToClosure(string body)
    {
        var tree = SyntaxTree.ParseText($$"""
            public class Probe {
                public static func Bump(ref value: int) { value += 1 }
                static func GetValue(out value: int) -> bool { value = 42; return true }
                static func Read(initial: int) -> int { {{body}} }
            }
            """);
        var compilation = Compilation.Create("Capture", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Read")!.Invoke(null, [41]));
    }
}
