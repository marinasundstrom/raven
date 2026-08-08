using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroToolingSnapshotCacheTests
{
    [Fact]
    public void SemanticModel_CachesTokenAndFragmentProviderResultsPerInvocation()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #cached { value }";
        var macro = new CachedToolingMacro();
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroToolingCache",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(macro));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var firstTokens = semanticModel.GetMacroTokens(expression);
        var secondTokens = semanticModel.GetMacroTokens(expression);
        var firstRegions = semanticModel.GetMacroFragmentRegions(expression);
        var secondRegions = semanticModel.GetMacroFragmentRegions(expression);

        Assert.Single(firstTokens);
        Assert.Single(secondTokens);
        Assert.Single(firstRegions);
        Assert.Single(secondRegions);
        Assert.Equal(1, macro.TokenClassificationCount);
        Assert.Equal(1, macro.FragmentProviderCount);
    }

    private sealed class CachedToolingMacro :
        ITokenTreeExpressionMacro,
        IMacroTokenClassifier,
        IMacroFragmentProvider
    {
        public string Name => "cached";

        public int TokenClassificationCount { get; private set; }

        public int FragmentProviderCount { get; private set; }

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public MacroTokenClassification ClassifyToken(
            TokenTreeMacroContext context,
            SyntaxToken token)
        {
            TokenClassificationCount++;
            return MacroTokenClassification.Identifier;
        }

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            FragmentProviderCount++;
            return
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(1, "value".Length)),
            ];
        }
    }
}
