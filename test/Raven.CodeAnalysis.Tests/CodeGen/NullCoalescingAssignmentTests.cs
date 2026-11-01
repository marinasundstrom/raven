using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullCoalescingAssignmentTests
{
    [Theory]
    [InlineData("var value: string? = null; value ??= \"filled\"; return value!", "filled")]
    [InlineData("var value: string? = \"kept\"; value ??= throw System.Exception(); return value!", "kept")]
    [InlineData("var value: int? = null; value ??= 42; return value!.ToString()", "42")]
    [InlineData("var value: int? = 7; value ??= 42; return value!.ToString()", "7")]
    [InlineData("var values: string?[] = [null]; values[0] ??= \"filled\"; return values[0]!", "filled")]
    public void LocalStorageIsAssignedOnlyWhenNull(string body, string expected)
    {
        Assert.Equal(expected, Run($$"""
            public class Runner {
                static func Run() -> string { {{body}} }
            }
            """));
    }

    [Theory]
    [InlineData("GetHolder().Value", "RGWSRG")]
    [InlineData("GetHolder()[Index()]", "RIGWSRIG")]
    [InlineData("GetArray()[Index()]", "RIWRI")]
    [InlineData("Missing()?.Value", "RR")]
    public void PropertyAndIndexerLocationsAreEvaluatedOnceAndSetterIsSkipped(string target, string expected)
    {
        Assert.Equal(expected, Run($$"""
            public class Holder {
                private field storage: string? = null
                var Value: string? {
                    get { Runner.Trace += "G"; return storage }
                    set { Runner.Trace += "S"; storage = value }
                }
                var self[index: int]: string? {
                    get { Runner.Trace += "G"; return storage }
                    set { Runner.Trace += "S"; storage = value }
                }
            }
            public class Runner {
                static var Trace: string = ""
                private static val holder: Holder = Holder()
                private static val items: string?[] = [null]
                static func GetArray() -> string?[] { Trace += "R"; return items }
                static func Missing() -> Holder? { Trace += "R"; return null }
                static func GetHolder() -> Holder { Trace += "R"; return holder }
                static func Index() -> int { Trace += "I"; return 0 }
                static func Make() -> string { Trace += "W"; return "filled" }
                static func Run() -> string {
                    {{target}} ??= Make()
                    {{target}} ??= Make()
                    return Trace
                }
            }
            """));
    }

    [Fact]
    public void StaticCacheAndRefStoragePreserveLaziness()
    {
        Assert.Equal("filled:1", Run("""
            public class Runner {
                private static field cache: string? = null
                private static field calls: int = 0
                static func Make() -> string { calls += 1; return "filled" }
                static func Fill(ref value: string?) { value ??= Make() }
                static func Run() -> string {
                    cache ??= Make()
                    cache ??= Make()
                    Fill(ref cache)
                    return "${cache}:${calls}"
                }
            }
            """));
    }

    [Fact]
    public void NullGuardDoesNotCallOverloadedEquality()
    {
        Assert.Equal("kept", Run("""
            class Item {
                static func ==(left: Item?, right: Item?) -> bool => throw System.Exception()
                static func !=(left: Item?, right: Item?) -> bool => throw System.Exception()
            }
            public class Runner {
                static func Run() -> string {
                    var item: Item? = Item()
                    item ??= throw System.Exception()
                    return "kept"
                }
            }
            """));
    }

    [Theory]
    [InlineData("val value: string? = null; value ??= \"text\"")]
    [InlineData("var value: int = 0; value ??= 1")]
    [InlineData("var value: string? = null; value ??= 42")]
    public void InvalidStorageOrValueProducesDiagnostics(string body)
    {
        var compilation = Compilation.Create("InvalidCoalescingAssignment", [SyntaxTree.ParseText($$"""
            public class Runner { static func Run() { {{body}} } }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static object? Run(string source)
    {
        var compilation = Compilation.Create("CoalescingAssignment", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        return loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null);
    }
}
