using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ExplicitPropertyForwardingTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetExplicitGetterForwardsToOrdinaryProperty()
    {
        var compilation = Create(TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(7, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrExplicitGetterBindsOrdinaryProperty()
    {
        var compilation = Create(TargetPlatform.NeoCLR);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var access = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "Value" && node.Parent is ReturnExpressionSyntax or ReturnStatementSyntax);
        var property = Assert.IsAssignableFrom<PropertySymbol>(model.GetSymbolInfo(access).Symbol);
        Assert.Empty(property.ExplicitInterfaceImplementations);
        Assert.Equal("Probe", property.ContainingType!.Name);
    }

    private static Compilation Create(TargetPlatform target)
        => Compilation.Create("PropertyForwarding" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText("""
            interface IValue { val Value: int }
            class Probe : IValue {
                private field _depth: int = 0
                val IValue.Value: int {
                    get {
                        _depth += 1
                        if _depth > 1 { return -1 }
                        return Value
                    }
                }
                internal val Value: int => 7
                static func Run() -> int {
                    let value: IValue = Probe()
                    return value.Value
                }
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
