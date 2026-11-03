using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class MacroCanonicalNameTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("", "Identity")]
    [InlineData("Outer", "Outer.Identity")]
    [InlineData("Outer.Inner", "Outer.Inner.Identity")]
    [InlineData("Outer.Inner.Deep", "Outer.Inner.Deep.Identity")]
    public void DotNetMacroCanonicalNameIncludesEveryNamespaceSegment(string scope, string expected)
        => AssertCanonicalName(TargetPlatform.DotNet, "namespace", scope, expected);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("", "Identity")]
    [InlineData("Outer", "Outer.Identity")]
    [InlineData("Outer.Inner", "Outer.Inner.Identity")]
    [InlineData("Outer.Inner.Deep", "Outer.Inner.Deep.Identity")]
    public void NeoClrMacroCanonicalNameIncludesEveryModuleSegment(string scope, string expected)
        => AssertCanonicalName(TargetPlatform.NeoCLR, "module", scope, expected);

    private static void AssertCanonicalName(TargetPlatform target, string keyword, string scope, string expected)
    {
        var prefix = scope.Length == 0 ? string.Empty : $"{keyword} {scope}\n";
        var tree = SyntaxTree.ParseText(prefix + """
            macro Identity() -> Raven.CodeAnalysis.Syntax.ExpressionSyntax {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42")
            }
            """);
        // Host references exercise the shared semantic API, not native runtime execution.
        var compilation = Compilation.Create("MacroCanonicalName",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(MetadataReference.CreateFromFile(typeof(SyntaxNode).Assembly.Location))
            .AddSyntaxTrees(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<MacroDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<MacroDeclarationSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.Equal("Identity", symbol.Name);
        Assert.Equal(scope, symbol.ContainingNamespace!.ToMetadataName());
        Assert.Equal(expected, symbol.CanonicalName);
    }
}
