using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class AttributedExtensionAccessibilityTests
{
    [Fact]
    public void PrivateContainerHelper_RemainsAccessibleFromAttributedExtension()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Runtime.CompilerServices.*
            static class Helpers
            {
                [Extension]
                static func TryNormalize(value: string, out normalized: string) -> bool
                {
                    normalized = Core(value)
                    return true
                }
                private static func Core(value: string) -> string => value.Trim()
            }
            """);
        var compilation = Compilation.Create("ExtensionPrivateHelper", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var call = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression.ToString() == "Core");
        var helper = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(call).Symbol);
        Assert.Equal(Accessibility.Private, helper.DeclaredAccessibility);
        Assert.Equal("Helpers", helper.ContainingType!.Name);
    }

    [Fact]
    public void AttributedExtension_DoesNotGainAccessToReceiverPrivateMembers()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Runtime.CompilerServices.*
            class Receiver { private val Secret: int = 42 }
            static class Helpers
            {
                [Extension]
                static func Read(value: Receiver) -> int => value.Secret
            }
            """);
        var compilation = Compilation.Create("ExtensionReceiverPrivacy", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0500");
    }
}
