using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public class DelegateAccessibilityQueryOrderTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void SignatureDiagnosticsSurviveMemberDiscovery(bool nested, bool queryFirst)
    {
        var declaration = "public delegate Factory(value: Hidden) -> Hidden";
        var source = "internal class Hidden {}\n" + (nested ? "public class Container { " + declaration + " }" : declaration);
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("DelegateAccessibility", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = tree.GetRoot().DescendantNodes().OfType<DelegateDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        MethodSymbol? earlyInvoke = null;
        if (queryFirst)
        {
            var early = Assert.IsAssignableFrom<NamedTypeSymbol>(model.GetDeclaredSymbol(syntax));
            earlyInvoke = Assert.Single(early.GetMembers("Invoke").OfType<MethodSymbol>());
        }
        for (var i = 0; i < 2; i++)
        {
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            Assert.Equal(2, errors.Length);
            Assert.All(errors, d => Assert.Equal("RAV0501", d.Id));
        }
        var symbol = Assert.IsAssignableFrom<NamedTypeSymbol>(model.GetDeclaredSymbol(syntax));
        var invoke = Assert.Single(symbol.GetMembers("Invoke").OfType<MethodSymbol>());
        if (earlyInvoke is not null)
            Assert.Same(earlyInvoke, invoke);
        Assert.Single(symbol.Constructors);
    }
}
