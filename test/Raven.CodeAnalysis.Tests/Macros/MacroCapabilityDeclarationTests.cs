using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroCapabilityDeclarationTests
{
    [Fact]
    public void CheckedInSample_ForwardsCapabilitiesToAdjacentNamespaceFunctions()
    {
        var repositoryRoot = Path.GetFullPath(
            Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var sourcePath = Path.Combine(
            repositoryRoot,
            "samples",
            "projects",
            "macro-capabilities",
            "src",
            "Main.rvn");
        var tree = SyntaxTree.ParseText(File.ReadAllText(sourcePath), path: sourcePath);
        var compilation = Compilation.Create(
                "MacroCapabilityDeclarations",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(tree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var macro = Assert.Single(
            compilation.GetMacroRegistry().GetMacros(MacroKind.Freestanding),
            static candidate => candidate.Name == "Show");
        Assert.IsAssignableFrom<IMacroKeywordProvider>(macro);
        Assert.IsAssignableFrom<IMacroTokenKindProvider>(macro);
        Assert.IsAssignableFrom<IMacroTokenClassifier>(macro);
        Assert.IsAssignableFrom<IMacroFragmentProvider>(macro);
        Assert.IsAssignableFrom<IMacroTokenSymbolProvider>(macro);
        Assert.IsAssignableFrom<IMacroCompletionProvider>(macro);
        Assert.IsAssignableFrom<IMacroEmbeddedLanguageProvider>(macro);

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var model = compilation.GetSemanticModel(consumerTree);
        var expansion = Assert.IsType<FreestandingMacroExpansionResult>(
            model.GetMacroExpansion(invocation));
        Assert.Equal("\"capability functions\"", expansion.Expression!.ToString());

        var fragment = Assert.Single(model.GetMacroInputSnapshot(invocation).FragmentRegions);
        Assert.Equal(MacroFragmentKind.Expression, fragment.Kind);
        Assert.Equal("\"capability functions\"", consumerTree.GetText()!.ToString(fragment.Span));

        var projection = Assert.IsType<MacroEmbeddedLanguageProjection>(
            model.GetMacroEmbeddedLanguageProjection(invocation));
        Assert.Equal("plaintext", projection.LanguageId);
        Assert.Equal(invocation.TokenTree!.BodyToken.Text.Trim(), projection.Text.Trim());
    }

    [Fact]
    public void DuplicateCapability_IsDiagnosed()
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Macros.*

            macro Broken(context: TokenTreeMacroContext)
                completion by Complete
                completion by Complete
            {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("0")
            }
            """);
        var compilation = Compilation.Create(
                "DuplicateMacroCapability",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTrees(tree);

        Assert.Contains(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV0939");
    }

    [Fact]
    public void CapabilityWithoutTokenTreeInput_IsDiagnosed()
    {
        var tree = SyntaxTree.ParseText("""
            macro Broken(value: int)
                completion by Complete
            {
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(value.ToString())
            }
            """);
        var compilation = Compilation.Create(
                "MacroCapabilityWithoutTokenTree",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTrees(tree);

        Assert.Contains(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV0940");
    }
}
