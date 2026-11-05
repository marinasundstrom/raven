using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class StructOverrideLookupTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("struct")]
    [InlineData("record struct")]
    public void UnqualifiedCallUsesStructOverride(string kind)
    {
        var compilation = Create(kind, TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal("itemother", loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("struct")]
    [InlineData("record struct")]
    public void NeoClrSemanticLookupUsesStructOverride(string kind)
    {
        var compilation = Create(kind, TargetPlatform.NeoCLR);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(call => call.ArgumentList.Arguments.Count == 0 &&
                call.Expression is IdentifierNameSyntax { Identifier.ValueText: "ToString" });
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("Item", method.ContainingType!.Name);
        Assert.True(method.IsOverride);
    }

    private static Compilation Create(string kind, TargetPlatform target)
        => Compilation.Create("StructOverride" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText($$"""
            {{kind}} Item {
                override func ToString() -> string => "item"
                func ToString(value: int) -> string => "other"
                func Display() -> string => ToString() + ToString(1)
            }
            class Probe { static func Run() -> string => Item().Display() }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
