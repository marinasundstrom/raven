using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroNamingTests
{
    [Theory]
    [InlineData("Decorate")]
    [InlineData("DecorateMacro")]
    [InlineData("Example.DecorateMacro")]
    public void LocalAttachedMacro_PreservesDeclaredSymbolName(string name)
    {
        var tree = SyntaxTree.ParseText($$"""
            namespace Example
            macro Decorate(on target: Raven.CodeAnalysis.Syntax.ClassDeclarationSyntax) {
                replace target
            }
            class Decorate {}
            #[{{name}}]
            class C {}
            """);
        var compilation = Compilation.Create("Local", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(tree);
        var model = compilation.GetSemanticModel(tree, tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single().Span.Start);
        var attribute = model.SyntaxTree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMacroSymbol>(model.GetSymbolInfo(attribute).Symbol);
        Assert.Equal("Decorate", symbol.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AttachedSuffix_CollisionIsAmbiguous()
    {
        var tree = SyntaxTree.ParseText("#[Example.LegacyMacro] class C {}");
        var compilation = Compilation.Create("Consumer").AddSyntaxTrees(tree);
        var registry = MacroRegistry.Create([new MacroReference(typeof(ShortProvider)), new MacroReference(typeof(SuffixedProvider))]);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        Assert.False(registry.TryResolveAttachedMacro(compilation, attribute, "Example.LegacyMacro", out _, out var ambiguous));
        Assert.True(ambiguous);
    }

    [Theory]
    [InlineData("Answer", true)]
    [InlineData("answerAlias", true)]
    [InlineData("AnswerMacro", false)]
    [InlineData("answerAliasMacro", false)]
    public void FreestandingLookup_DoesNotAddSuffixes(string name, bool expected)
    {
        var tree = SyntaxTree.ParseText("import Example.*\nval x = Answer!()");
        var compilation = Compilation.Create("Consumer").AddSyntaxTrees(tree);
        var registry = MacroRegistry.Create([new MacroReference(typeof(FreeProvider))]);
        var invocation = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        Assert.Equal(expected, registry.TryResolveFreestandingMacro(compilation, invocation, name, out _, out var ambiguous));
        Assert.False(ambiguous);
    }

    public sealed class FreeProvider : IMacroDefinition
    {
        public string Name => "Answer";
        public string Namespace => "Example";
        public string Alias => "answerAlias";
        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context) => FreestandingMacroExpansionResult.Empty;
    }

    public sealed class ShortProvider : IMacroDefinition
    {
        public string Name => "Legacy";
        public string Namespace => "Example";
        public MacroExpansionResult Expand(AttachedMacroContext context) => MacroExpansionResult.Empty;
    }

    public sealed class SuffixedProvider : IMacroDefinition
    {
        public string Name => "LegacyMacro";
        public string Namespace => "Example";
        public MacroExpansionResult Expand(AttachedMacroContext context) => MacroExpansionResult.Empty;
    }
}
