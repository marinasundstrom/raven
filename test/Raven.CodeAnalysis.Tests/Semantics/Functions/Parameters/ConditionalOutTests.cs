using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ConditionalOutTests
{
    [Theory, Trait("Target", "Shared")]
    [InlineData("if flag && Set(out value) && value > 0 { return true }; value = 0; return false", true)]
    [InlineData("if flag || Set(out value) { value = 0 }; return false", true)]
    [InlineData("if !(flag && Set(out value)) { value = 0 }; return true", true)]
    [InlineData("while flag && Set(out value) { return true }; value = 0; return false", true)]
    [InlineData("if flag || Set(out value) { return true }; value = 0; return false", false)]
    [InlineData("if flag && Set(out value) { value = 0 }; return false", false)]
    [InlineData("return flag && Set(out value)", false)]
    [InlineData("while flag || Set(out value) { value = 0; break }; return false", true)]
    [InlineData("while flag || Set(out value) { break }; return false", false)]
    [InlineData("while flag || Set(out value) { while flag { break }; value = 0; break }; return false", true)]
    [InlineData("while flag || Set(out value) { throw System.Exception() }; return false", true)]
    [InlineData("loop { if flag { throw System.Exception() }; value = 0; break }; return true", true)]
    [InlineData("loop { for x in 0..3 { break }; value = 0; break }; return true", true)]
    [InlineData("loop { if flag { break }; value = 0; break }; return true", false)]
    public void ShortCircuitBranchesTrackForwardedOutStorage(string body, bool valid)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static func Set(out value: int) -> bool { value = 42; return true }
                static func Test(flag: bool, out value: int) -> bool { {{body}} }
            }
            """);
        var compilation = Compilation.Create("ConditionalOut", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (valid) Assert.Empty(errors);
        else Assert.Equal("RAV0269", Assert.Single(errors).Id);
    }

    [Theory, Trait("Target", "DotNet")]
    [InlineData(false, 0)]
    [InlineData(true, 42)]
    public void DotNet_ForwardedStorageReflectsTheExecutedBranch(bool flag, int expected)
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Set(out value: int) -> bool { value = 42; return true }
                static func Test(flag: bool, out value: int) {
                    if flag && Set(out value) { return }
                    value = 0
                }
            }
            """);
        var compilation = Compilation.Create("ForwardedStorage", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        object?[] arguments = [flag, -1];
        loaded.Assembly.GetType("Probe", true)!.GetMethod("Test")!.Invoke(null, arguments);
        Assert.Equal(expected, arguments[1]);
    }
}
