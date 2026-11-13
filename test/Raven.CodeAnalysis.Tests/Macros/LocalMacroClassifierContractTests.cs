using System;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class LocalMacroClassifierContractTests
{
    [Theory]
    [InlineData("class M : IMacroExecutor {}", true)]
    [InlineData("class M : IMacroDefinition {}", true)]
    [InlineData("record class M : IMacroDefinition {}", true)]
    [InlineData("struct M : IMacroDefinition {}", true)]
    [InlineData("[LocalMacro] class M {}", true)]
    [InlineData("[LocalMacroAttribute] class M {}", true)]
    [InlineData("class M {}", false)]
    [InlineData("", false)]
    public void WholeFileClassificationPreservesOriginalTree(string source, bool macro)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Equal(macro, LocalMacroSyntaxClassifier.IsLocalMacroTree(tree));
        var partition = LocalMacroSyntaxClassifier.Partition(tree);
        if (macro)
        {
            Assert.Null(partition.ConsumerTree);
            Assert.Same(tree, partition.MacroTree);
        }
        else
        {
            Assert.Same(tree, partition.ConsumerTree);
            Assert.Null(partition.MacroTree);
        }
    }

    [Fact]
    public void PluginMarkerOnlyAppliesAtAssemblyScope()
    {
        var plugin = SyntaxTree.ParseText("[assembly: RavenCompilerPlugin]\nclass Helper {}");
        Assert.True(LocalMacroSyntaxClassifier.IsCompilerPluginTree(plugin));
        Assert.False(LocalMacroSyntaxClassifier.IsLocalMacroTree(plugin));
        Assert.Same(plugin, LocalMacroSyntaxClassifier.Partition(plugin).MacroTree);
        Assert.False(LocalMacroSyntaxClassifier.IsLocalMacroPosition(plugin, plugin.Length - 1));
        Assert.False(LocalMacroSyntaxClassifier.IsCompilerPluginTree(SyntaxTree.ParseText("[RavenCompilerPlugin] class Helper {}")));
    }

    [Fact]
    public void MixedProjectionIncludesTransitiveHelpersAndPreservesCoordinates()
    {
        const string source = """
            import System.*
            func Third() -> int => 42
            func Second() -> int => Third()
            [LocalMacro] class M {
                func Expand() -> int => Second()
            }
            func Consumer() -> int => 7
            """;
        var tree = SyntaxTree.ParseText(source, new ParseOptions { DocumentationMode = false, EnableCharKeyword = false }, path: "partition.rvn");
        var partition = LocalMacroSyntaxClassifier.Partition(tree);
        Assert.NotNull(partition.ConsumerTree);
        Assert.NotNull(partition.MacroTree);
        var macro = partition.MacroTree!.GetText()!.ToString();
        var consumer = partition.ConsumerTree!.GetText()!.ToString();
        Assert.Contains("func Third", macro);
        Assert.Contains("func Second", macro);
        Assert.DoesNotContain("func Consumer", macro);
        Assert.Contains("func Consumer", consumer);
        Assert.DoesNotContain("func Third", consumer);
        Assert.DoesNotContain("class M", consumer);
        Assert.Equal(source.Length, macro.Length);
        Assert.Equal(source.Length, consumer.Length);
        Assert.Equal(tree.FilePath, partition.MacroTree.FilePath);
        Assert.True(tree.Options.IsEquivalentTo(partition.MacroTree.Options));
        for (var index = 0; index < source.Length; index++)
        {
            if (source[index] is '\r' or '\n')
            {
                Assert.Equal(source[index], macro[index]);
                Assert.Equal(source[index], consumer[index]);
            }
        }
        Assert.True(LocalMacroSyntaxClassifier.IsLocalMacroPosition(tree, source.IndexOf("Third()", StringComparison.Ordinal)));
        Assert.False(LocalMacroSyntaxClassifier.IsLocalMacroPosition(tree, source.IndexOf("Consumer()", StringComparison.Ordinal)));
        Assert.False(LocalMacroSyntaxClassifier.IsLocalMacroPosition(tree, tree.Length));
    }

    [Fact]
    public void NamespaceEnvelopeSurvivesProjection()
    {
        const string source = "namespace Example {\n [LocalMacro] class M {}\n class Consumer {}\n}";
        var partition = LocalMacroSyntaxClassifier.Partition(SyntaxTree.ParseText(source));
        var macro = partition.MacroTree!;
        Assert.DoesNotContain(macro.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var declaration = Assert.Single(macro.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.Equal("M", declaration.Identifier.ValueText);
        Assert.IsType<NamespaceDeclarationSyntax>(declaration.Parent);
    }

    [Theory]
    [InlineData("class M : IMacroDefinition { func Expand() {} }", true)]
    [InlineData("class M : IMacroDefinition { func Expand() {} func Expand(x: int) {} }", false)]
    [InlineData("class M { func Expand() {} }", false)]
    public void MethodShapeRequiresExactlyOneExpand(string source, bool expected)
    {
        var declaration = Assert.Single(SyntaxTree.ParseText(source).GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.Equal(expected, LocalMacroSyntaxClassifier.IsMethodShapedMacroClass(declaration));
    }

    [Fact]
    public void InvalidInputsKeepArgumentDiagnostics()
    {
        Assert.Throws<ArgumentNullException>(() => LocalMacroSyntaxClassifier.Partition(null!));
        var tree = SyntaxTree.ParseText("class C {}");
        Assert.Throws<ArgumentOutOfRangeException>(() => LocalMacroSyntaxClassifier.IsLocalMacroPosition(tree, -1));
        Assert.Throws<ArgumentOutOfRangeException>(() => LocalMacroSyntaxClassifier.IsLocalMacroPosition(tree, tree.Length + 1));
    }
}
