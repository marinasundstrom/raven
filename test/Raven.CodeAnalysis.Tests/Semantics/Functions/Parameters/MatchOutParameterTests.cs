using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class MatchOutParameterTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("flag match { true => Set(out value); false => Set(out value) }", true)]
    [InlineData("(flag match { true => Set(out value); false => Set(out value) })", true)]
    [InlineData("flag match { true => Set(out value); false => false }", false)]
    [InlineData("flag match { true => { value = 1; true }; false => false }", false)]
    [InlineData("flag match { true => { value = 1; true }; false => { value = 2; false } }", true)]
    [InlineData("flag match { true => Set(out value); false => throw System.Exception() }", true)]
    [InlineData("flag match { true when Set(out value) => true; _ => Set(out value) }", true)]
    [InlineData("flag match { true when Set(out value) => true; _ => false }", false)]
    [InlineData("if flag { Set(out value) } else { Set(out value) }", true)]
    [InlineData("if flag { value = 1; true } else { false }", false)]
    public void EveryNormallyReturningArmMustAssign(string expression, bool valid)
    {
        var compilation = Create(expression, TargetPlatform.DotNet);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (valid)
            Assert.Empty(errors);
        else
            Assert.Equal("RAV0269", Assert.Single(errors).Id);
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrUsesSharedMatchAssignmentRules()
    {
        var compilation = Create("flag match { true => Set(out value); false => Set(out value) }", TargetPlatform.NeoCLR);
        var tree = compilation.SyntaxTrees.Single();
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void ReturnedMatchUpdatesCallerStorage(bool flag)
    {
        var compilation = Create("flag match { true => Set(out value); false => Set(out value) }", TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        object?[] arguments = [flag, -1];
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Test")!.Invoke(null, arguments));
        Assert.Equal(42, arguments[1]);
    }

    private static Compilation Create(string expression, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static func Set(out value: int) -> bool { value = 42; return true }
                static func Test(flag: bool, out value: int) -> bool => {{expression}}
            }
            """);
        return Compilation.Create("MatchOut" + Guid.NewGuid().ToString("N"), [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithTargetPlatform(target).WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
    }
}
