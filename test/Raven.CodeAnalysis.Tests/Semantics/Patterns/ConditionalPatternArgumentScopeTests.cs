using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ConditionalPatternArgumentScopeTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void DotNetPatternLocalSurvivesContextualArgumentRebinding(bool bindingPattern, bool diagnosticsFirst)
        => AssertBinding(bindingPattern, diagnosticsFirst, TargetPlatform.DotNet);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void NeoClrPatternLocalSurvivesContextualArgumentRebinding(bool bindingPattern, bool diagnosticsFirst)
        => AssertBinding(bindingPattern, diagnosticsFirst, TargetPlatform.NeoCLR);

    private static void AssertBinding(bool bindingPattern, bool diagnosticsFirst, TargetPlatform target)
    {
        var compilation = CreateCompilation(bindingPattern, target);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var use = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "text" && node.Parent is ArgumentSyntax);
        var local = Assert.IsAssignableFrom<LocalSymbol>(model.GetSymbolInfo(use).Symbol);
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Same(local, model.GetSymbolInfo(use).Symbol);
        Assert.Equal(SpecialType.System_String, model.GetTypeInfo(use).Type!.SpecialType);
    }

    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void ContextualRebindingDoesNotLeakPatternLocalIntoElse(bool bindingPattern)
    {
        var compilation = CreateCompilation(bindingPattern, TargetPlatform.DotNet, "Length(text)");
        var tree = compilation.SyntaxTrees.Single();
        var elseUse = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Last(node => node.Identifier.ValueText == "text");
        var missing = Assert.Single(compilation.GetDiagnostics(), d => d.Id == "RAV0103");
        Assert.Equal(elseUse.Span, missing.Location.SourceSpan);
    }

    internal static Compilation CreateCompilation(bool bindingPattern, TargetPlatform target, string elseExpression = "0")
        => Compilation.Create("ConditionalPatternArgument", [SyntaxTree.ParseText($$"""
            class Probe {
                static func Identity<T>(value: T) -> T => value
                static func Length(text: string) -> int => text.Length
                static func Read(value: object?) -> int {
                    return Identity(if {{(bindingPattern ? "let string text = value" : "value is string text")}} {
                        Length(text)
                    } else { {{elseExpression}} })
                }
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
