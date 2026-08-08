using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroInputSnapshotTests
{
    [Fact]
    public void GetMacroInputSnapshot_CombinesAuthoredTokensAndFragments()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #snapshot { value }";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroInputSnapshot",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new SnapshotMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var snapshot = compilation.GetMacroInputSnapshot(expression);

        var token = Assert.Single(snapshot.Tokens);
        var region = Assert.Single(snapshot.FragmentRegions);
        Assert.Equal("value", token.Text);
        Assert.Equal(MacroTokenClassification.Identifier, token.Classification);
        Assert.Equal(MacroFragmentKind.Expression, region.Kind);
        Assert.Equal(token.Span, region.Span);
        Assert.True(snapshot.BodySpan.Contains(token.Span));
    }

    private sealed class SnapshotMacro :
        ITokenTreeExpressionMacro,
        IMacroTokenClassifier,
        IMacroFragmentProvider
    {
        public string Name => "snapshot";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public MacroTokenClassification ClassifyToken(
            TokenTreeMacroContext context,
            SyntaxToken token)
            => MacroTokenClassification.Identifier;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(1, "value".Length)),
            ];
    }
}
