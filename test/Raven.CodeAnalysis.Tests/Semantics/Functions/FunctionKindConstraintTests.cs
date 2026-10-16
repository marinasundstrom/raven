using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class FunctionKindConstraintTests
{
    private static Compilation Create(string source, TargetPlatform target = TargetPlatform.NeoCLR) =>
        Compilation.Create("FunctionConstraints", [SyntaxTree.ParseText(source)], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

    private static IEnumerable<Diagnostic> Diagnostics(Compilation compilation) =>
        compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics()
            .Concat(compilation.SyntaxTrees.Single().GetDiagnostics());

    [Fact]
    public void SyntaxExposesFunctionConstraintKeyword()
    {
        var tree = SyntaxTree.ParseText("class Slot<T> where T: func {}");
        var constraint = Assert.Single(tree.GetRoot().DescendantNodes().OfType<FunctionConstraintSyntax>());
        Assert.Equal(SyntaxKind.FuncKeyword, constraint.FuncKeyword.Kind);
    }

    [Theory]
    [InlineData("(int) -> int", true)]
    [InlineData("() -> ()", true)]
    [InlineData("int", false)]
    [InlineData("System.Func<int, int>", false)]
    [InlineData("System.Action", false)]
    [InlineData("Invokable", false)]
    public void FunctionCategoryIsStructural(string argument, bool valid)
    {
        var compilation = Create($$"""
            class Signal<T> where T: func {}
            class Invokable { func Invoke() -> int => 42 }
            func Accept(value: Signal<{{argument}}>) -> int => 42
            """);
        var errors = Diagnostics(compilation).Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (valid) Assert.Empty(errors);
        else Assert.Contains(errors, d => d.Id == "RAV0320");
    }

    [Fact]
    public void SymbolicArgumentsMustCarryFunctionConstraint()
    {
        var valid = Create("class Signal<T> where T: func {}\nfunc Accept<F>(value: Signal<F>) where F: func {}");
        Assert.DoesNotContain(Diagnostics(valid), d => d.Severity == DiagnosticSeverity.Error);
        var invalid = Create("class Signal<T> where T: func {}\nfunc Accept<F>(value: Signal<F>) {}");
        Assert.Contains(Diagnostics(invalid), d => d.Id == "RAV0320");
    }

    [Fact]
    public void OrdinaryTargetDoesNotEraseFunctionConstraint()
    {
        var compilation = Create("class Signal<T> where T: func {}", TargetPlatform.DotNet);
        Assert.Contains(Diagnostics(compilation), d => d.Id == "RAV0365");
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.False(emitted.Success);
        Assert.Equal(0, image.Length);
        Assert.Contains(emitted.Diagnostics, d => d.Id == "RAV0365");
    }

    [Theory]
    [InlineData("func")]
    [InlineData("class")]
    [InlineData("struct")]
    [InlineData("new()")]
    [InlineData("System.Object")]
    public void FunctionCategoryCannotCombineWithOtherBounds(string bound)
    {
        var compilation = Create($"class Signal<T> where T: func, {bound} {{}}");
        Assert.Contains(Diagnostics(compilation), d => d.Id == "RAV0365");
    }

    [Fact]
    public void DotNetMethodConstraintsCannotBeErased()
    {
        var compilation = Create("func Keep<F>(callback: F) -> F where F: func => callback", TargetPlatform.DotNet);
        Assert.Contains(Diagnostics(compilation), d => d.Id == "RAV0365");
    }

    [Fact]
    public void GenericFunctionKindDoesNotInventAnInvocationSignature()
    {
        var compilation = Create("func Call<F>(callback: F) where F: func { callback(42) }");
        var diagnostics = Diagnostics(compilation).ToArray();
        Assert.DoesNotContain(diagnostics, d => d.Id == "RAV0365");
        Assert.Contains(diagnostics, d => d.Id is "RAV0030" or "RAV1955");
    }
}
