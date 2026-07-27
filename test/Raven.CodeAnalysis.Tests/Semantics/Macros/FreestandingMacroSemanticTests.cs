using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class FreestandingMacroSemanticTests : CompilationTestBase
{
    [Fact]
    public void MarkedLocalMacroDeclaration_CanShareTreeWithConsumer()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [LocalMacro]
            class ProjectMacros : IRavenMacroPlugin {
                val Name: string => "Local"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [AnswerMacro()]
            }

            [LocalMacro]
            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = #quote { 42 }
                    }
                }
            }

            func Main() -> int => #answer { }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var macroTree = Assert.Single(compilation.MacroSyntaxTrees);
        Assert.Equal(sourceTree.Length, consumerTree.Length);
        Assert.Equal(sourceTree.Length, macroTree.Length);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void GetSemanticModel_AuthoredPositionRoutesWithoutWorkspace()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            [LocalMacro]
            class MacroSupport {
                val Value: int => 42
            }

            func Main() -> int => 0
            """);
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var source = sourceTree.GetText()!.ToString();
        var macroModel = compilation.GetSemanticModel(
            sourceTree,
            source.IndexOf("MacroSupport", StringComparison.Ordinal));
        var consumerModel = compilation.GetSemanticModel(
            sourceTree,
            source.IndexOf("Main", StringComparison.Ordinal));

        Assert.Contains(macroModel.SyntaxTree, compilation.MacroSyntaxTrees);
        Assert.Contains(consumerModel.SyntaxTree, compilation.SyntaxTrees);
        var macroDeclaration = macroModel.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single();
        var mainDeclaration = consumerModel.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single();
        Assert.Equal("MacroSupport", macroModel.GetDeclaredSymbol(macroDeclaration)?.Name);
        Assert.Equal("Main", consumerModel.GetDeclaredSymbol(mainDeclaration)?.Name);
    }

    [Fact]
    public void MarkedLocalMacroPluginTree_IsAutomaticallyPartitioned()
    {
        var macroTree = CreateLocalAnswerMacroTree();
        var consumerTree = SyntaxTree.ParseText("func Main() -> int => #localAnswer { }");
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(macroTree, consumerTree);

        Assert.Equal([consumerTree], compilation.SyntaxTrees);
        Assert.Equal([macroTree], compilation.MacroSyntaxTrees);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void LocalMacroSyntaxTrees_CompileAndExpandWithoutWorkspace()
    {
        var macroTree = CreateLocalAnswerMacroTree();
        var consumerTree = SyntaxTree.ParseText("func Main() -> int => #localAnswer { }");
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroSyntaxTrees(macroTree)
            .AddSyntaxTrees(consumerTree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(macroTree, compilation.SyntaxTrees);
        Assert.Contains(macroTree, compilation.MacroSyntaxTrees);

        var expression = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void InvalidLocalMacroPartition_ReportsItsDiagnosticsAndDoesNotRegisterMacros()
    {
        var macroTree = SyntaxTree.ParseText("""
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            class BrokenMacroPlugin : IRavenMacroPlugin {
                val Name: string => "Broken"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [MissingMacro()]
            }
            """, path: "local-macros.rvn");
        var consumerTree = SyntaxTree.ParseText(
            "func Main() -> int => #localAnswer { }",
            path: "main.rvn");
        var compilation = Compilation.Create(
                "BrokenLocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroSyntaxTrees(macroTree)
            .AddSyntaxTrees(consumerTree);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic =>
                diagnostic.Severity == DiagnosticSeverity.Error &&
                ReferenceEquals(diagnostic.Location.SourceTree, macroTree));
        Assert.Contains(diagnostics, static diagnostic => diagnostic.Id == "RAVM010");
        Assert.Contains(
            compilation.GetDiagnostics(macroTree),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM003");
        Assert.DoesNotContain(
            compilation.GetDiagnostics(consumerTree),
            diagnostic => ReferenceEquals(diagnostic.Location.SourceTree, macroTree));
    }

    [Fact]
    public void MarkedLocalMacroDeclaration_ConsumerDependencyReportsCycle()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            class ConsumerConfiguration {
                static val Answer: int => 42
            }

            [LocalMacro]
            class ProjectMacros : IRavenMacroPlugin {
                val Name: string => "Local"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [AnswerMacro()]
            }

            [LocalMacro]
            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    val answer = ConsumerConfiguration.Answer
                    FreestandingMacroExpansionResult {
                        Expression = #quote { 42 }
                    }
                }
            }

            func Main() -> int => #answer { }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics()
                .Where(static diagnostic => diagnostic.Id == "RAVM003"));

        Assert.Contains("ConsumerConfiguration", diagnostic.GetMessage());
        Assert.Equal("main.rvn", diagnostic.Location.SourceTree?.FilePath);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic =>
                diagnostic.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id &&
                diagnostic.GetMessage().Contains("ConsumerConfiguration", StringComparison.Ordinal));
    }

    [Fact]
    public void DedicatedLocalMacroFile_ConsumerDependencyReportsCycle()
    {
        var macroTree = SyntaxTree.ParseText(
            """
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [LocalMacroPlugin]
            class ProjectMacros : IRavenMacroPlugin {
                val Name: string => "Local"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [AnswerMacro()]
            }

            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    val answer = ConsumerConfiguration.Answer
                    FreestandingMacroExpansionResult {
                        Expression = #quote { 42 }
                    }
                }
            }
            """,
            path: "local-macros.rvn");
        var consumerTree = SyntaxTree.ParseText(
            """
            class ConsumerConfiguration {
                static val Answer: int => 42
            }

            func Main() -> int => #answer { }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTreesWithLocalMacros(macroTree, consumerTree);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics()
                .Where(static diagnostic => diagnostic.Id == "RAVM003"));

        Assert.Contains("ConsumerConfiguration", diagnostic.GetMessage());
        Assert.Same(macroTree, diagnostic.Location.SourceTree);
    }

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

    private static SyntaxTree CreateLocalAnswerMacroTree()
        => SyntaxTree.ParseText("""
            import System.Collections.Immutable.*
            import Raven.CodeAnalysis.Macros.*

            [LocalMacroPlugin]
            class LocalMacroPlugin : IRavenMacroPlugin {
                val Name: string => "Local"

                func GetMacros() -> ImmutableArray<IMacroDefinition>
                    => [LocalAnswerMacro()]
            }

            class LocalAnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Targets: MacroTarget => MacroTarget.None

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = #quote { 42 }
                    }
                }
            }
            """, path: "local-macros.rvn");

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
    public void TypedFreestandingMacroExpansionFailure_ReportsUnderlyingException()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #typedBoom()
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ThrowingTypedFreestandingMacroPlugin)));
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static d => d.Id == "RAVM020"));

        Assert.Contains("typedBoom", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("typed plugin boom", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.DoesNotContain("target of an invocation", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);

        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        Assert.Equal(expression.Name.Span, diagnostic.Location.SourceSpan);
    }

    [Fact]
    public void FreestandingMacroCancellation_PropagatesAndDoesNotCacheFailure()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #cancelRaw()
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CancellingFreestandingMacroPlugin)));
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        using var cancellationSource = new CancellationTokenSource();
        CancellingFreestandingMacro.CancellationSource = cancellationSource;

        Assert.ThrowsAny<OperationCanceledException>(
            () => model.GetMacroExpansion(expression, cancellationSource.Token));

        CancellingFreestandingMacro.CancellationSource = null;
        Assert.NotNull(model.GetMacroExpansion(expression));
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAVM020");
    }

    [Fact]
    public void TypedFreestandingMacroCancellation_PropagatesThroughReflectionAndDoesNotCacheFailure()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #cancelTyped()
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CancellingFreestandingMacroPlugin)));
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        using var cancellationSource = new CancellationTokenSource();
        CancellingTypedFreestandingMacro.CancellationSource = cancellationSource;

        Assert.ThrowsAny<OperationCanceledException>(
            () => model.GetMacroExpansion(expression, cancellationSource.Token));

        CancellingTypedFreestandingMacro.CancellationSource = null;
        Assert.NotNull(model.GetMacroExpansion(expression));
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAVM020");
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
                func WriteLine(value: int) -> unit { }

                func Run(viewModel: CounterViewModel) -> unit {
                    val subscription = #subscribe(viewModel.Count, (value) => {
                        WriteLine(value)
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

    [Fact]
    public void TokenTreeMacro_CanParseEntireBodyAsRavenExpression()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #raven {
                40 + 2
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("40 + 2", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TokenTreeMacro_CanDelegateSelectedDslSpanToRavenParser()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #select {
                query-field ::= {{ 20 + 22 }}
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("20 + 22", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TokenTreeMacro_CanParseEntireBodyAsRavenStatement()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #statement {
                return 42
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TokenTreeMacro_CanDelegateSelectedDslSpanToRavenStatementParser()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #statementSelect {
                action ::= {{ return 42 }}
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TokenTreeMacro_StatementParseResultForwardsNativeDiagnostic()
    {
        const string source = """
            func Main() -> int => #statementResult {
                return value.Equals(1, )
            }
            """;
        var (compilation, tree) = CreateCompilation(source);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV1525"));

        Assert.Same(tree, diagnostic.Location.SourceTree);
        Assert.Equal(")", tree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void TokenTreeMacro_StatementParseResultRejectsTrailingInput()
    {
        const string source = """
            func Main() -> int => #statementResult {
                return 1 return 2
            }
            """;
        var (compilation, tree) = CreateCompilation(source);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV1525"));

        Assert.Same(tree, diagnostic.Location.SourceTree);
        Assert.Equal("return", tree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void TokenTreeMacro_BodyDiagnosticUsesAuthoredBodySpan()
    {
        const string source = """
            func Main() -> int => #reject {
                invalid-dsl-token
            }
            """;
        var (compilation, _) = CreateCompilation(source);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Equal(
            source.IndexOf("invalid-dsl-token", StringComparison.Ordinal),
            diagnostic.Location.SourceSpan.Start);
        Assert.Equal("invalid-dsl-token".Length, diagnostic.Location.SourceSpan.Length);
    }

    [Fact]
    public void TokenTreeMacro_RequiresTokenTreeInvocationForm()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() -> int => #raven()
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM013"));

        Assert.Contains("requires a token-tree body", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void TokenTreeMacro_DefaultStreamAppliesMacroLocalKeywordOverlay()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #keywordStream {
                select value
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenStreamMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TokenTreeMacro_CanReplaceDefaultStreamWithCustomProvider()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #customStream {
                ⟨custom-value⟩
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(TokenStreamMacroPlugin)));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    public sealed class TokenTreeMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(TokenTreeMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            =>
            [
                new RavenBodyMacro(),
                new SelectBodyMacro(),
                new StatementBodyMacro(),
                new StatementSelectBodyMacro(),
                new StatementResultBodyMacro(),
                new RejectBodyMacro()
            ];
    }

    public sealed class RavenBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "raven";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => new()
            {
                Expression = context.ParseExpression()
            };
    }

    public sealed class SelectBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "select";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var body = context.GetBodyText();
            var expressionStart = body.IndexOf("{{", StringComparison.Ordinal) + 2;
            var expressionEnd = body.IndexOf("}}", expressionStart, StringComparison.Ordinal);

            return new FreestandingMacroExpansionResult
            {
                Expression = context.ParseExpression(
                    TextSpan.FromBounds(expressionStart, expressionEnd))
            };
        }
    }

    public sealed class StatementBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "statement";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var statement = context.ParseStatement();
            return new FreestandingMacroExpansionResult
            {
                Expression = Assert.IsType<ReturnStatementSyntax>(statement).Expression
            };
        }
    }

    public sealed class StatementSelectBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "statementSelect";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var body = context.GetBodyText();
            var statementStart = body.IndexOf("{{", StringComparison.Ordinal) + 2;
            var statementEnd = body.IndexOf("}}", statementStart, StringComparison.Ordinal);
            var statement = context.ParseStatement(
                TextSpan.FromBounds(statementStart, statementEnd));

            return new FreestandingMacroExpansionResult
            {
                Expression = Assert.IsType<ReturnStatementSyntax>(statement).Expression
            };
        }
    }

    public sealed class StatementResultBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "statementResult";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var result = context.ParseStatementResult();
            return new FreestandingMacroExpansionResult
            {
                Expression = (result.Syntax as ReturnStatementSyntax)?.Expression,
                Diagnostics = result.Diagnostics
            };
        }
    }

    public sealed class RejectBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "reject";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var body = context.GetBodyText();
            const string invalidToken = "invalid-dsl-token";
            var start = body.IndexOf(invalidToken, StringComparison.Ordinal);

            return new FreestandingMacroExpansionResult
            {
                MacroDiagnostics =
                [
                    context.CreateBodyDiagnostic(
                        new TextSpan(start, invalidToken.Length),
                        "invalid DSL token")
                ]
            };
        }
    }

    public sealed class TokenStreamMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(TokenStreamMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new KeywordStreamMacro(), new CustomStreamMacro()];
    }

    public sealed class KeywordStreamMacro : ITokenTreeExpressionMacro, IMacroKeywordProvider
    {
        private const int SelectKeywordRawKind = 80_001;

        public string Name => "keywordStream";
        public MacroTarget Targets => MacroTarget.None;

        public ImmutableArray<MacroKeyword> Keywords =>
        [
            new("select", SelectKeywordRawKind)
        ];

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var stream = context.CreateTokenStream();
            var select = stream.ReadToken();
            var value = stream.ReadToken();

            var isValid =
                select.Kind == SyntaxKind.IdentifierToken &&
                select.RawKind == SelectKeywordRawKind &&
                select.SpanStart == context.GetBodyText().IndexOf("select", StringComparison.Ordinal) &&
                value.Kind == SyntaxKind.IdentifierToken &&
                value.RawKind == (int)SyntaxKind.IdentifierToken &&
                stream.IsEndOfFile;

            return new FreestandingMacroExpansionResult
            {
                Expression = ParseExpression(isValid ? "42" : "0")
            };
        }
    }

    public sealed class CustomStreamMacro : ITokenTreeExpressionMacro, IMacroTokenStreamProvider
    {
        private const int CustomValueRawKind = 80_002;

        public string Name => "customStream";
        public MacroTarget Targets => MacroTarget.None;

        public IMacroTokenStream CreateTokenStream(MacroTokenStreamContext context)
            => new SingleCustomTokenStream(context, CustomValueRawKind);

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var stream = context.CreateTokenStream();
            var token = stream.ReadToken();
            var isValid =
                token.Kind == SyntaxKind.None &&
                token.RawKind == CustomValueRawKind &&
                token.Text == "⟨custom-value⟩" &&
                token.SpanStart == context.GetBodyText().IndexOf("⟨custom-value⟩", StringComparison.Ordinal) &&
                stream.IsEndOfFile;

            return new FreestandingMacroExpansionResult
            {
                Expression = ParseExpression(isValid ? "42" : "0")
            };
        }
    }

    private sealed class SingleCustomTokenStream : IMacroTokenStream
    {
        private readonly SyntaxToken _token;
        private bool _hasRead;

        public SingleCustomTokenStream(MacroTokenStreamContext context, int rawKind)
        {
            var text = context.BodyText.Trim();
            var position = context.BodyText.IndexOf(text, StringComparison.Ordinal);
            _token = SyntaxFactory.Token(rawKind, text, position);
        }

        public bool IsEndOfFile => _hasRead;

        public SyntaxToken PeekToken(int offset = 0)
        {
            if (offset != 0 || _hasRead)
                throw new ArgumentOutOfRangeException(nameof(offset));

            return _token;
        }

        public SyntaxToken ReadToken()
        {
            if (_hasRead)
                throw new InvalidOperationException("The custom token stream has been consumed.");

            _hasRead = true;
            return _token;
        }
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

    public sealed class ThrowingTypedFreestandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(ThrowingTypedFreestandingMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ThrowingTypedFreestandingMacro()];
    }

    public sealed class ThrowingTypedFreestandingMacroParameters;

    public sealed class ThrowingTypedFreestandingMacro : IFreestandingExpressionMacro<ThrowingTypedFreestandingMacroParameters>
    {
        public string Name => "typedBoom";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(
            FreestandingMacroContext<ThrowingTypedFreestandingMacroParameters> context)
            => throw new InvalidOperationException("typed plugin boom");
    }

    public sealed class CancellingFreestandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(CancellingFreestandingMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new CancellingFreestandingMacro(), new CancellingTypedFreestandingMacro()];
    }

    public sealed class CancellingFreestandingMacro : IFreestandingExpressionMacro
    {
        public static CancellationTokenSource? CancellationSource { get; set; }

        public string Name => "cancelRaw";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
        {
            CancellationSource?.Cancel();
            context.CancellationToken.ThrowIfCancellationRequested();
            return FreestandingMacroExpansionResult.FromExpression(ParseExpression("42"));
        }
    }

    public sealed class CancellingTypedFreestandingMacroParameters;

    public sealed class CancellingTypedFreestandingMacro : IFreestandingExpressionMacro<CancellingTypedFreestandingMacroParameters>
    {
        public static CancellationTokenSource? CancellationSource { get; set; }

        public string Name => "cancelTyped";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(
            FreestandingMacroContext<CancellingTypedFreestandingMacroParameters> context)
        {
            CancellationSource?.Cancel();
            context.CancellationToken.ThrowIfCancellationRequested();
            return FreestandingMacroExpansionResult.FromExpression(ParseExpression("42"));
        }
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
