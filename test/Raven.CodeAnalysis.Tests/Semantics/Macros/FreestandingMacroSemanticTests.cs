using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class FreestandingMacroSemanticTests : CompilationTestBase
{
    [Fact]
    public void UnknownFreestandingMacro_ReportsUnknownMacroDiagnostic()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() -> int => #answer()
            """);

        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(static diagnostic => diagnostic.Id == "RAVM010"));
        Assert.Contains("answer", diagnostic.GetMessage());
    }

    [Fact]
    public void GetMacroExpansion_ReturnsFreestandingExpansionResult()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #answer()
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(AnswerMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.IsType<LiteralExpressionSyntax>(expansion!.Expression);
        Assert.Equal("42", expansion.Expression!.ToString());
    }

    [Fact]
    public void TypedFreestandingMacroParameters_BindPositionalAndNamedArguments()
    {
        CapturingFreestandingMacro.LastParameters = null;

        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #repeat(3, Label: "hi")
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CapturingFreestandingMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        var parameters = Assert.IsType<RepeatMacroParameters>(CapturingFreestandingMacro.LastParameters);
        Assert.Equal(3, parameters.Count);
        Assert.Equal("hi", parameters.Label);
    }

    [Fact]
    public void RawFreestandingMacro_ArgumentsRequireExplicitOptIn()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() -> int => #answer(42)
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(AnswerMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM012"));
        Assert.Contains("answer", diagnostic.GetMessage());
    }

    [Fact]
    public void FreestandingMacroReportedArgumentValidationDiagnostic_UsesMacroDiagnosticPath()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #repeat(0)
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ValidatingFreestandingMacroPlugin)));
        var diagnostics = compilation.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics.Where(static d => d.Id == "RAVM021"));
        Assert.Contains("repeat", diagnostic.GetMessage());
        Assert.Contains("REP001: count must be greater than zero", diagnostic.GetMessage(), StringComparison.Ordinal);

        var argument = tree.GetRoot()
            .DescendantNodes()
            .OfType<ArgumentSyntax>()
            .Single();

        Assert.Equal(argument.Span, diagnostic.Location.SourceSpan);
    }

    [Fact]
    public void FreestandingMacro_ReusedLambdaArgument_PreservesContextualParameterType()
    {
        var (compilation, tree) = CreateCompilation("""
            class ObservableInt {
                func Subscribe(handler: (int) -> unit) -> unit { }
            }

            class CounterViewModel {
                var Count: int = 0
                val CountChanged: ObservableInt = ObservableInt()
            }

            class Harness {
                func Consume(value: int) -> unit { }

                func Run(viewModel: CounterViewModel) -> unit {
                    val subscription = #subscribe(viewModel.Count, (value) => {
                        Consume(value)
                    })
                }
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(SubscribeMacroPlugin)));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var lambda = root.DescendantNodes().OfType<FunctionExpressionSyntax>().Single();
        var parameter = root.DescendantNodes()
            .OfType<ParameterSyntax>()
            .Single(candidate => candidate.Ancestors().OfType<FunctionExpressionSyntax>().Any());
        var valueReference = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "value");

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(TypeKind.Struct, lambdaParameter.Type.TypeKind);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);

        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetFunctionExpressionParameterSymbol(parameter));
        Assert.Equal(SpecialType.System_Int32, parameterSymbol.Type.SpecialType);

        var referencedParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(valueReference).Symbol);
        Assert.Equal(SpecialType.System_Int32, referencedParameter.Type.SpecialType);
    }

    public sealed class AnswerMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(AnswerMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new AnswerMacro()];
    }

    public sealed class AnswerMacro : IFreestandingExpressionMacro
    {
        public string Name => "answer";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => new()
            {
                Expression = ParseExpression("42")
            };
    }

    public sealed class CapturingFreestandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(CapturingFreestandingMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new CapturingFreestandingMacro()];
    }

    public sealed class CapturingFreestandingMacro : IFreestandingExpressionMacro<RepeatMacroParameters>
    {
        public static RepeatMacroParameters? LastParameters { get; set; }

        public string Name => "repeat";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext<RepeatMacroParameters> context)
        {
            LastParameters = context.Parameters;
            return new FreestandingMacroExpansionResult
            {
                Expression = ParseExpression(context.Parameters.Count.ToString())
            };
        }
    }

    public sealed class RepeatMacroParameters(int count)
    {
        public int Count { get; } = count;

        public string? Label { get; set; }
    }

    public sealed class ValidatingFreestandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(ValidatingFreestandingMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ValidatingFreestandingMacro()];
    }

    public sealed class ValidatingFreestandingMacroParameters(int count)
    {
        public int Count { get; } = count;
    }

    public sealed class ValidatingFreestandingMacro : IFreestandingExpressionMacro<ValidatingFreestandingMacroParameters>
    {
        public string Name => "repeat";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext<ValidatingFreestandingMacroParameters> context)
        {
            if (context.Parameters.Count <= 0)
            {
                return new FreestandingMacroExpansionResult
                {
                    MacroDiagnostics =
                    [
                        context.CreateArgumentDiagnostic(
                            context.Arguments[0],
                            "count must be greater than zero",
                            code: "REP001")
                    ]
                };
            }

            return new FreestandingMacroExpansionResult
            {
                Expression = ParseExpression("42")
            };
        }
    }

    public sealed class SubscribeMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(SubscribeMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new SubscribeMacro()];
    }

    public sealed class SubscribeMacro : IFreestandingExpressionMacro
    {
        public string Name => "subscribe";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;
        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
        {
            var propertyAccess = Assert.IsType<MemberAccessExpressionSyntax>(context.Arguments[0].Expression);
            var callback = context.Arguments[1].Expression;
            var propertyName = Assert.IsType<IdentifierNameSyntax>(propertyAccess.Name);
            var signalName = propertyName.Identifier.ValueText + "Changed";

            return new FreestandingMacroExpansionResult
            {
                Expression = SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            propertyAccess.Expression,
                            SyntaxFactory.Token(SyntaxKind.DotToken),
                            SyntaxFactory.IdentifierName(signalName)),
                        SyntaxFactory.Token(SyntaxKind.DotToken),
                        SyntaxFactory.IdentifierName("Subscribe")),
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        [
                            new SyntaxNodeOrToken(SyntaxFactory.Argument(callback))
                        ])))
            };
        }
    }

    private static ExpressionSyntax ParseExpression(string expressionText)
    {
        var tree = SyntaxTree.ParseText($$"""
            func Main() -> int => {{expressionText}}
            """);

        return tree.GetRoot()
            .DescendantNodes()
            .OfType<ArrowExpressionClauseSyntax>()
            .Single()
            .Expression;
    }
}
