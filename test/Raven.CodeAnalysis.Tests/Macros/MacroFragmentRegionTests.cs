using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroFragmentRegionTests
{
    [Fact]
    public void GetMacroFragmentRegions_MapsBodyRelativeRegionsToAuthoredSource()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #query { from user in users select user.Name }";
        var (compilation, expression) = CreateCompilation(code, new QueryMacro());

        var regions = compilation.GetMacroFragmentRegions(expression);

        Assert.Collection(
            regions,
            region =>
            {
                Assert.Equal(MacroFragmentKind.Expression, region.Kind);
                Assert.Equal("users", code.Substring(region.Span.Start, region.Span.Length));
                Assert.Equal(
                    region.Span.Start - expression.TokenTree!.OpenBraceToken.Span.End,
                    region.BodyRelativeSpan.Start);
                Assert.Equal(region.Span.Length, region.BodyRelativeSpan.Length);
            },
            region =>
            {
                Assert.Equal(MacroFragmentKind.Expression, region.Kind);
                Assert.Equal(0, region.Span.Length);
                Assert.Equal(expression.TokenTree!.CloseBraceToken.SpanStart, region.Span.Start);
            });
    }

    [Fact]
    public void GetMacroFragmentRegions_ReturnsEmptyWhenOptionalProviderFails()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #invalidRegions { value }";
        var (compilation, expression) = CreateCompilation(code, new InvalidRegionMacro());

        var regions = compilation.GetSemanticModel(expression.SyntaxTree!)
            .GetMacroFragmentRegions(expression);

        Assert.Empty(regions);
    }

    private static (Compilation Compilation, FreestandingMacroExpressionSyntax Expression) CreateCompilation(
        string code,
        IMacroDefinition macro)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentRegions",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(macro));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        return (compilation, expression);
    }

    private sealed class QueryMacro : ITokenTreeExpressionMacro, IMacroFragmentProvider
    {
        public string Name => "query";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            var body = context.GetBodyText();
            var expressionStart = body.IndexOf("users", StringComparison.Ordinal);
            return
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(expressionStart, "users".Length)),
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(body.Length, 0)),
            ];
        }
    }

    private sealed class InvalidRegionMacro : ITokenTreeExpressionMacro, IMacroFragmentProvider
    {
        public string Name => "invalidRegions";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            => [context.CreateFragmentRegion(MacroFragmentKind.Expression, new TextSpan(0, context.BodySpan.Length + 1))];
    }
}
