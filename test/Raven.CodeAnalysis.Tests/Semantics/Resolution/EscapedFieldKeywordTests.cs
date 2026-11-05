using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class EscapedFieldKeywordTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("@field", false)]
    [InlineData("@field", true)]
    [InlineData("`field`", false)]
    [InlineData("`field`", true)]
    public void DotNetEscapedFieldUsesLocalBinding(string spelling, bool diagnosticsFirst)
        => AssertBinding(spelling, diagnosticsFirst, TargetPlatform.DotNet);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("@field", false)]
    [InlineData("@field", true)]
    [InlineData("`field`", false)]
    [InlineData("`field`", true)]
    public void NeoClrEscapedFieldUsesLocalBinding(string spelling, bool diagnosticsFirst)
        => AssertBinding(spelling, diagnosticsFirst, TargetPlatform.NeoCLR);

    private static void AssertBinding(string spelling, bool diagnosticsFirst, TargetPlatform target)
    {
        var compilation = CreateCompilation(spelling, target);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        // Host-reference semantic validation also covers neoCLR; no native emission.
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var references = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Where(node => node.Identifier.ValueText == "field").ToArray();
        Assert.Equal(2, references.Length);
        foreach (var reference in references)
        {
            var symbol = Assert.IsAssignableFrom<LocalSymbol>(model.GetSymbolInfo(reference).Symbol);
            Assert.Equal("field", symbol.Name);
        }
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var property = Assert.IsType<SourcePropertySymbol>(model.GetDeclaredSymbol(
            tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single()));
        Assert.Null(property.BackingField);
    }

    internal static Compilation CreateCompilation(string spelling, TargetPlatform target)
        => Compilation.Create("EscapedField", [SyntaxTree.ParseText($$"""
            class Probe {
                val Value: int {
                    get {
                        let {{spelling}} = 42
                        return {{spelling}}
                    }
                }
                static func Run() -> int {
                    let {{spelling}} = 7
                    return Probe().Value + {{spelling}}
                }
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
