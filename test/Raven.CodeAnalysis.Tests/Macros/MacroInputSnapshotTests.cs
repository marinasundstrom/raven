using System;
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
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = snapshot!{ value }";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroInputSnapshot",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new SnapshotMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroExpressionSyntax>()
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

    [Fact]
    public void FindFragmentRegion_ReturnsMostSpecificRegionIncludingEmptySlot()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = regions!{ outer.inner }";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentLookup",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new RegionLookupMacro()));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroExpressionSyntax>()
            .Single();
        var snapshot = compilation.GetMacroInputSnapshot(expression);
        var innerPosition = code.IndexOf("inner", StringComparison.Ordinal) + 2;
        var endPosition = expression.TokenTree!.CloseBraceToken.SpanStart;

        var inner = snapshot.FindFragmentRegion(innerPosition);
        var empty = snapshot.FindFragmentRegion(endPosition);

        Assert.Equal(MacroFragmentKind.Type, inner!.Kind);
        Assert.Equal("inner", code.Substring(inner.Span.Start, inner.Span.Length));
        Assert.Equal(MacroFragmentKind.Pattern, empty!.Kind);
        Assert.Equal(0, empty.Span.Length);
        Assert.Equal(
            snapshot.FragmentRegions.OrderBy(static region => region.Span.Start)
                .ThenBy(static region => region.Span.Length),
            snapshot.FragmentRegions);
    }

    private sealed class SnapshotMacro :
        IMacroDefinition,
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

    private sealed class RegionLookupMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "regions";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Pattern,
                    new TextSpan(context.BodySpan.Length, 0)),
                context.CreateFragmentRegion(
                    MacroFragmentKind.Type,
                    new TextSpan(7, "inner".Length)),
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(1, "outer.inner".Length)),
            ];
    }
}
