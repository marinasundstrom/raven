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
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = query!{ from user in users select user.Name }";
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
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = invalidRegions!{ value }";
        var (compilation, expression) = CreateCompilation(code, new InvalidRegionMacro());

        var regions = compilation.GetSemanticModel(expression.SyntaxTree!)
            .GetMacroFragmentRegions(expression);

        Assert.Empty(regions);
    }

    [Fact]
    public void GetMacroFragmentRegions_PreservesExpressionTargetType()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = targetTyped!{ (value) => value }";
        var (compilation, expression) = CreateCompilation(code, new TargetTypedMacro());

        var region = Assert.Single(compilation.GetMacroFragmentRegions(expression));

        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(region.TargetType);
        Assert.Equal("Action", targetType.Name);
        var argumentType = Assert.Single(targetType.TypeArguments);
        Assert.Equal(SpecialType.System_Int32, argumentType.SpecialType);
    }

    [Fact]
    public void MacroFragmentContribution_ProjectsThroughGeneratedAdapter()
    {
        const string code = """
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Text.*

            macro RavenExpression(context: TokenTreeMacroContext) {
                let span = TextSpan(0, context.BodySpan.Length)
                let local = context.CreateFragmentLocal(
                    "editorValue",
                    context.Compilation.GetSpecialType(Raven.CodeAnalysis.SpecialType.System_String))
                fragment context.CreateFragmentRegion(MacroFragmentKind.Expression, span, [local])
                expand context.ParseExpression(span)
            }

            func Main() {
                let message = "hello"
                let value = RavenExpression! { message }
            }
            """;
        var authoredTree = SyntaxTree.ParseText(code, path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroFragmentRegions",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var expression = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroExpressionSyntax>()
            .Single();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var regions = compilation.GetMacroFragmentRegions(expression);

        var region = Assert.Single(regions);
        Assert.Equal(MacroFragmentKind.Expression, region.Kind);
        Assert.Equal(" message ", code.Substring(region.Span.Start, region.Span.Length));
        var local = Assert.Single(region.Locals);
        Assert.Equal("editorValue", local.Name);
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);
    }

    private static (Compilation Compilation, InvocableMacroExpressionSyntax Expression) CreateCompilation(
        string code,
        IMacroDefinition macro)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentRegions",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(macro));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroExpressionSyntax>()
            .Single();
        return (compilation, expression);
    }

    private sealed class QueryMacro : IMacroDefinition, IMacroFragmentProvider
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

    private sealed class InvalidRegionMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "invalidRegions";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            => [context.CreateFragmentRegion(MacroFragmentKind.Expression, new TextSpan(0, context.BodySpan.Length + 1))];
    }

    private sealed class TargetTypedMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "targetTyped";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            var definition = context.Compilation.GetTypeByMetadataName("System.Action`1")!;
            var intType = context.Compilation.GetSpecialType(SpecialType.System_Int32);
            return
            [
                context.CreateExpressionFragmentRegion(
                    new TextSpan(0, context.BodySpan.Length),
                    definition.Construct(intType))
            ];
        }
    }
}
