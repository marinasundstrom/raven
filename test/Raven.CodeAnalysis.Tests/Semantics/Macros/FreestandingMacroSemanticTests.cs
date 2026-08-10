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
    protected override MetadataReference[] GetMetadataReferences()
        => TestMetadataReferences.DefaultWithRavenMacros;

    private new (Compilation Compilation, SyntaxTree Tree) CreateCompilation(
        string source,
        CompilationOptions? options = null,
        MetadataReference[]? references = null,
        string assemblyName = "test")
    {
        var tree = SyntaxTree.ParseText(source);
        var imports = SyntaxTree.ParseText("""
            global {
                import Raven.CodeAnalysis.Tests.Semantics.Macros.*
            }
            """);
        return (
            base.CreateCompilation([imports, tree], options, references, assemblyName),
            tree);
    }

    [Fact]
    public void Macro_CompilesIntoLocalProviderAndExpands()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Add(left: int, right: int = 1) {
                let sum = left + right
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(sum.ToString())
            }

            func Main() -> int => #Add(20, right: 22)
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void NamespacedMacro_AliasIsImportedWithItsNamespace()
    {
        var macroTree = SyntaxTree.ParseText(
            """
            namespace Example.Macros {
                [Raven.CodeAnalysis.Macros.MacroAlias("twiceAlias")]
                macro Double(value: int) {
                    let doubled = value * 2
                    expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(doubled.ToString())
                }
            }
            """,
            path: "macros.rvn");
        var consumerTree = SyntaxTree.ParseText(
            """
            import Example.Macros.*

            func Main() -> int => twiceAlias!(21)
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "NamespacedMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(macroTree, consumerTree);

        var diagnostics = compilation.GetDiagnostics();
        var registeredMacros = compilation.GetMacroRegistry()
            .GetMacros(MacroKind.FreestandingExpression)
            .Where(static macro => macro.Name == "Double")
            .ToArray();
        Assert.True(
            registeredMacros.Length == 1,
            $"Registered: {string.Join(", ", compilation.GetMacroRegistry().GetMacros(MacroKind.FreestandingExpression).Select(static macro => $"{macro.Namespace}.{macro.Name} alias={macro.Alias}"))}{Environment.NewLine}{string.Join(Environment.NewLine, diagnostics)}");
        var registeredMacro = Assert.Single(
            registeredMacros);
        Assert.Equal("Example.Macros", registeredMacro.Namespace);
        Assert.Equal("twiceAlias", registeredMacro.Alias);
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var projectedConsumerTree = compilation.SyntaxTrees.Single(
            static tree => tree.FilePath == "main.rvn");
        var invocation = projectedConsumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(projectedConsumerTree).GetMacroExpansion(invocation);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void Macro_UserParametersDoNotCollideWithLoweringNames()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Add(
                context: int,
                __macroResult: int,
                __macroContext: int,
                __macroResultBuilder: int
            ) {
                let sum = context + __macroResult + __macroContext + __macroResultBuilder
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(sum.ToString())
            }

            func Main() -> int => #Add(10, 10, 10, 12)
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroLoweringNameConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void ExpressionMacro_ProjectsAuthoredArgumentSyntaxAlongsideValues()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Syntax.*

            macro AddOffset(offset: int, expression: LiteralExpressionSyntax) {
                let source = expression.ToString() + " + " + offset.ToString()
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(source)
            }

            func Main() -> int => #AddOffset(2, 40)
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "ExpressionMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.Equal("40 + 2", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TokenStreamMacro_CompilesIntoTypedLocalProviderAndExpands()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            macro FirstTokenLength(tokens: IMacroTokenStream, offset: int) {
                let token = tokens.ReadToken()
                let length = token.Text.Length + offset
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(length.ToString())
            }

            func Main() -> int => FirstTokenLength!(1) { raven }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "TokenStreamMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.Equal("6", expansion!.Expression!.ToString());
    }

    [Fact]
    public void ExpandContribution_ReturnsFromCurrentMacroExecutionPath()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

            macro Choose(first: bool) {
                if first {
                    expand ParseExpression("1")
                }

                expand ParseExpression("2")
            }

            func Main() -> int => Choose!(true)
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "ReturningMacro",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(
            diagnostics.All(static diagnostic => diagnostic.Severity != DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.Equal("1", expansion!.Expression!.ToString());
    }

    [Fact]
    public void ExpandContribution_LowersFromSingleLineMacroBody()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Syntax.SyntaxFactory.*
            macro Answer() { expand ParseExpression("42") }
            func Main() -> int => Answer!()
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "SingleLineMacro",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.Equal(
            "42",
            compilation.GetSemanticModel(consumerTree)
                .GetMacroExpansion(invocation)!
                .Expression!
                .ToString());
    }

    [Fact]
    public void AttachedMacro_CombinesReachedContributions()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Compose(shouldReplace: bool) on property: Property {
                if shouldReplace {
                    replace property
                }
                introduce property
                introduce property
            }

            class Widget {
                #[Compose(true)]
                var Value: int = 1
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "AttachedMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var attribute = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<AttributeSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(attribute);

        Assert.True(
            expansion is not null,
            string.Join(Environment.NewLine, compilation.GetDiagnostics()));
        Assert.IsType<PropertyDeclarationSyntax>(expansion!.ReplacementDeclaration);
        Assert.Equal(2, expansion.IntroducedMembers.Length);
        Assert.All(
            expansion.IntroducedMembers,
            static member => Assert.IsType<PropertyDeclarationSyntax>(member));
    }

    [Fact]
    public void AttachedMacro_AccumulatesReportedDiagnostics()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*

            macro Validate(context: AttachedMacroContext) on Type {
                context.ReportDiagnostic("Types are not accepted here")
                context.ReportDiagnostic("Second problem")
            }

            #[Validate]
            class Widget {}
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "AttachedMacroDiagnostic",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var attribute = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<AttributeSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(attribute);

        Assert.Collection(
            expansion!.MacroDiagnostics,
            diagnostic =>
            {
                Assert.Equal("Types are not accepted here", diagnostic.Message);
                Assert.Equal(attribute.Name.Span, diagnostic.Location.SourceSpan);
            },
            diagnostic =>
            {
                Assert.Equal("Second problem", diagnostic.Message);
                Assert.Equal(attribute.Name.Span, diagnostic.Location.SourceSpan);
            });
    }

    [Fact]
    public void RavenAuthoredMacro_CanRequireSyntaxCategoryWithoutThrowing()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*
            import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

            macro Validate(context: TokenTreeMacroContext) {
                let syntax = context.ParseCompilationUnit()
                let expression = context.RequireSyntax<ExpressionSyntax>(
                    syntax,
                    "Expected an expression body.",
                    "VALIDATE001")
                expand ParseExpression("0")
            }

            func Main() -> int => Validate! {
                class Widget {}
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "SyntaxCategoryValidation",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocation = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(consumerTree).GetMacroExpansion(invocation);

        Assert.NotNull(expansion);
        Assert.Equal("0", expansion!.Expression!.ToString());
        var diagnostic = Assert.Single(expansion.MacroDiagnostics);
        Assert.Equal("Expected an expression body.", diagnostic.Message);
        Assert.Equal("VALIDATE001", diagnostic.Code);
        Assert.Equal(
            "class Widget {}",
            consumerTree.GetText().ToString(diagnostic.Location!.SourceSpan).Trim());
    }

    [Fact]
    public void MarkedLocalMacroDeclaration_CanShareTreeWithConsumer()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression

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
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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
    public void DirectMacroTree_IsAutomaticallyPartitioned()
    {
        var macroTree = CreateLocalAnswerMacroTree();
        var consumerTree = SyntaxTree.ParseText("func Main() -> int => #localAnswer { }");
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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
            import Raven.CodeAnalysis.Macros.*

            class BrokenMacro : ITokenTreeExpressionMacro {
                val Name: string => "broken"
                val Kind: MacroKind => MacroKind.FreestandingExpression
                val Missing: MissingMacro
            }
            """, path: "local-macros.rvn");
        var consumerTree = SyntaxTree.ParseText(
            "func Main() -> int => #localAnswer { }",
            path: "main.rvn");
        var compilation = Compilation.Create(
                "BrokenLocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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
    public void InvalidMacro_DoesNotPreventValidSiblingFromExpanding()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Syntax.*

            macro Broken(expression: ExpressionSyntax) {
                match expression {
                }
                expand expression
            }

            macro Double(value: int) {
                let doubled = value * 2
                expand SyntaxFactory.ParseExpression(doubled.ToString())
            }

            func Main() -> int {
                let broken = Broken!(40)
                Double!(21)
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "PartiallyInvalidMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDiagnostics();
        var documentDiagnostics = compilation.GetDocumentDiagnostics(sourceTree);
        var exhaustivenessDiagnostics = diagnostics
            .Where(static diagnostic => diagnostic.Id == "RAV2100")
            .ToArray();
        Assert.NotEmpty(exhaustivenessDiagnostics);
        Assert.All(
            exhaustivenessDiagnostics,
            diagnostic =>
            {
                Assert.Equal(sourceTree.FilePath, diagnostic.Location.SourceTree?.FilePath);
                Assert.Equal(3, diagnostic.Location.GetLineSpan().StartLinePosition.Line);
            });
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM010");
        Assert.Contains(documentDiagnostics, static diagnostic => diagnostic.Id == "RAV2100");

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var invocations = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .ToArray();
        var brokenInvocation = Assert.Single(invocations, expression =>
            expression.Name.ToString() == "Broken");
        var doubleInvocation = Assert.Single(invocations, expression =>
            expression.Name.ToString() == "Double");
        var semanticModel = compilation.GetSemanticModel(consumerTree);
        var brokenSymbol = semanticModel.GetSymbolInfo(brokenInvocation).Symbol;
        var expansion = semanticModel.GetMacroExpansion(doubleInvocation);

        Assert.IsAssignableFrom<IMacroDeclarationSymbol>(brokenSymbol);
        Assert.Equal("Broken", brokenSymbol.Name);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void InvalidGenericMacroConstraint_DoesNotInvalidateDeclarationsOrValidSibling(bool diagnosticsFirst)
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Syntax.*

            macro Broken<T>(value: T)
                where U: struct {
                expand value
            }

            macro Double(value: int) {
                let doubled = value * 2
                expand SyntaxFactory.ParseExpression(doubled.ToString())
            }

            func Main() -> int {
                let broken = Broken<int>!(40)
                Double!(21)
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "InvalidGenericMacroConstraintConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var consumerModel = compilation.GetSemanticModel(consumerTree);
        var macroModel = compilation.GetSemanticModel(
            sourceTree,
            sourceTree.GetText()!.ToString().IndexOf("macro Broken", StringComparison.Ordinal));
        var declarations = macroModel.SyntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<MacroDeclarationSyntax>()
            .ToDictionary(static declaration => declaration.Identifier.ValueText);
        var invocations = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .ToArray();
        var brokenInvocation = Assert.Single(invocations, expression =>
            expression.Name.ToString() == "Broken<int>");
        var doubleInvocation = Assert.Single(invocations, expression =>
            expression.Name.ToString() == "Double");

        var brokenDeclaration = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            macroModel.GetDeclaredSymbol(declarations["Broken"]));
        var doubleDeclaration = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            macroModel.GetDeclaredSymbol(declarations["Double"]));
        var brokenInvocationSymbol = Assert.IsAssignableFrom<IMacroDeclarationSymbol>(
            consumerModel.GetSymbolInfo(brokenInvocation).Symbol);
        var expansion = consumerModel.GetMacroExpansion(doubleInvocation);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Equal("Broken", brokenDeclaration.Name);
        Assert.Equal("Broken", brokenInvocationSymbol.Name);
        Assert.Equal("Double", doubleDeclaration.Name);
        Assert.Equal("42", expansion!.Expression!.ToString());
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnknownTypeParameterInConstraintClause);
        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Id == "RAVM010");
    }

    [Fact]
    public void Macro_LetElseWithNonTerminatingLoop_UsesOrdinaryFlowAnalysis()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Select(value: int?) {
                let actual: int = value else {
                    loop {
                    }
                }

                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(actual.ToString())
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroFlowAnalysis",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDocumentDiagnostics(sourceTree);

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.LetElseClauseMustNotCompleteNormally);
    }

    [Fact]
    public void Macro_LetElseWithConstantTrueWhileLoop_UsesOrdinaryFlowAnalysis()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Select(value: int?) {
                let actual: int = value else {
                    while true {
                    }
                }

                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(actual.ToString())
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroWhileFlowAnalysis",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDocumentDiagnostics(sourceTree);

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.LetElseClauseMustNotCompleteNormally);
    }

    [Fact]
    public void Macro_LetElseWithExhaustiveAbruptMatch_UsesOrdinaryFlowAnalysis()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            macro Select(value: int?, fallback: bool) {
                let actual: int = value else {
                    match fallback {
                        true => throw System.Exception()
                        false => throw System.Exception()
                    }
                }

                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(actual.ToString())
            }
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroMatchFlowAnalysis",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var diagnostics = compilation.GetDocumentDiagnostics(sourceTree);

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.LetElseClauseMustNotCompleteNormally);
    }

    [Fact]
    public void MarkedLocalMacroDeclaration_ConsumerDependencyReportsCycle()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*

            class ConsumerConfiguration {
                static val Answer: int => 42
            }

            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    let answer = ConsumerConfiguration.Answer
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
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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
            import Raven.CodeAnalysis.Macros.*

            class AnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "answer"
                val Kind: MacroKind => MacroKind.FreestandingExpression

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    let answer = ConsumerConfiguration.Answer
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
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
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

    [Fact]
    public void MacroAlias_RequiresItsNamespaceImport()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() -> int => answerAlias! { }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(NamespacedAnswerMacro)));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM010"));
        Assert.Contains("answerAlias", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void CompilerProvidedMacroAlias_RequiresRavenMacrosImport()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() => quote! { 42 }
            """);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM010"));
        Assert.Contains("quote", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void CompilerProvidedMacro_CanUseCanonicalQualifiedNameWithoutImport()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() => Raven.Macros.Quote! { 42 }
            """);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Id == "RAVM010");
    }

    [Fact]
    public void MacroAlias_IsAvailableThroughNamespaceImport()
    {
        var tree = SyntaxTree.ParseText("""
            import Example.Macros.*

            func Main() -> int => answerAlias! { }
            """);
        var compilation = base.CreateCompilation(tree).AddMacroReferences(
            new MacroReference(typeof(NamespacedAnswerMacro)));

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(tree).GetMacroExpansion(expression);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void Macro_CanBeInvokedByCanonicalQualifiedNameWithoutImport()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => Example.Macros.Answer! { }
            """);
        var compilation = base.CreateCompilation(tree).AddMacroReferences(
            new MacroReference(typeof(NamespacedAnswerMacro)));

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(tree).GetMacroExpansion(expression);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void LocalValue_ShadowsImportedMacroAlias()
    {
        var (compilation, _) = CreateCompilation("""
            import Example.Macros.*

            func Main() -> int {
                let answerAlias = 0
                return answerAlias! { }
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(NamespacedAnswerMacro)));

        Assert.Contains(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Id == CompilerDiagnostics.InvalidInvocation.Id);
    }

    private static SyntaxTree CreateLocalAnswerMacroTree()
        => SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            class LocalAnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.FreestandingExpression

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(AnswerMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CapturingFreestandingMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ThrowingTypedFreestandingMacro)));
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

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(CancellingFreestandingMacro)),
            new MacroReference(typeof(CancellingTypedFreestandingMacro)));
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

        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(CancellingFreestandingMacro)),
            new MacroReference(typeof(CancellingTypedFreestandingMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(AnswerMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(ValidatingFreestandingMacro)));
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
                    let subscription = #subscribe(viewModel.Count, (value) => {
                        WriteLine(value)
                    })
                }
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(SubscribeMacro)));

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

        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetFunctionExpressionParameterSymbol(parameter));
        Assert.Equal(SpecialType.System_Int32, parameterSymbol.Type.SpecialType);

        var referencedParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetSymbolInfo(valueReference).Symbol);
        Assert.Equal(SpecialType.System_Int32, referencedParameter.Type.SpecialType);

        var lambdaType = model.GetTypeInfo(lambda);
        Assert.Equal(TypeKind.Delegate, lambdaType.ConvertedType?.TypeKind);

        var boundLambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(lambda));
        var lambdaParameter = Assert.Single(boundLambda.Parameters);
        Assert.Equal(TypeKind.Struct, lambdaParameter.Type.TypeKind);
        Assert.Equal(SpecialType.System_Int32, lambdaParameter.Type.SpecialType);
    }

    [Fact]
    public void TokenTreeMacro_CanParseEntireBodyAsRavenExpression()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #raven {
                40 + 2
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(RavenBodyMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(SelectBodyMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(StatementBodyMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(StatementSelectBodyMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(StatementResultBodyMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(StatementResultBodyMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(RejectBodyMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(RavenBodyMacro)));
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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(KeywordStreamMacro)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CustomStreamMacro)));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void TypedTokenTreeMacroParameters_BindAlongsideRawBody()
    {
        CapturingTokenTreeMacro.LastParameters = null;
        CapturingTokenTreeMacro.LastBody = null;

        var (compilation, tree) = CreateCompilation("""
            func Main() -> int => #typedBody(3, Label: "item") {
                custom content
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(CapturingTokenTreeMacro)));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var expansion = model.GetMacroExpansion(expression);

        Assert.NotNull(expansion);
        var parameters = Assert.IsType<RepeatMacroParameters>(CapturingTokenTreeMacro.LastParameters);
        Assert.Equal(3, parameters.Count);
        Assert.Equal("item", parameters.Label);
        Assert.Contains("custom content", CapturingTokenTreeMacro.LastBody);
        Assert.Equal("42", expansion!.Expression!.ToString());
    }

    [Fact]
    public void UntypedTokenTreeMacro_RejectsArguments()
    {
        var (compilation, _) = CreateCompilation("""
            func Main() -> int => #raven(3) {
                42
            }
            """);

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(RavenBodyMacro)));
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM012"));

        Assert.Contains("does not accept arguments", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    public sealed class RavenBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "raven";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => new()
            {
                Expression = context.ParseExpression()
            };
    }

    [MacroAlias("answerAlias")]
    public sealed class NamespacedAnswerMacro : ITokenTreeExpressionMacro
    {
        public string Namespace => "Example.Macros";

        public string Name => "Answer";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.FromExpression(ParseExpression("42"));
    }

    public sealed class CapturingTokenTreeMacro : ITokenTreeExpressionMacro<RepeatMacroParameters>
    {
        public static RepeatMacroParameters? LastParameters { get; set; }

        public static string? LastBody { get; set; }

        public string Name => "typedBody";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext<RepeatMacroParameters> context)
        {
            LastParameters = context.Parameters;
            LastBody = context.GetBodyText();
            return FreestandingMacroExpansionResult.FromExpression(ParseExpression("42"));
        }
    }

    public sealed class SelectBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "select";

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

    public sealed class KeywordStreamMacro : ITokenTreeExpressionMacro, IMacroKeywordProvider
    {
        private const int SelectKeywordRawKind = 80_001;

        public string Name => "keywordStream";

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

    public sealed class AnswerMacro : IFreestandingExpressionMacro
    {
        public string Name => "answer";
        public MacroKind Kind => MacroKind.FreestandingExpression;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => new()
            {
                Expression = ParseExpression("42")
            };
    }

    public sealed class CapturingFreestandingMacro : IFreestandingExpressionMacro<RepeatMacroParameters>
    {
        public static RepeatMacroParameters? LastParameters { get; set; }

        public string Name => "repeat";
        public MacroKind Kind => MacroKind.FreestandingExpression;

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

    public sealed class ThrowingTypedFreestandingMacroParameters;

    public sealed class ThrowingTypedFreestandingMacro : IFreestandingExpressionMacro<ThrowingTypedFreestandingMacroParameters>
    {
        public string Name => "typedBoom";

        public FreestandingMacroExpansionResult Expand(
            FreestandingMacroContext<ThrowingTypedFreestandingMacroParameters> context)
            => throw new InvalidOperationException("typed plugin boom");
    }

    public sealed class CancellingFreestandingMacro : IFreestandingExpressionMacro
    {
        public static CancellationTokenSource? CancellationSource { get; set; }

        public string Name => "cancelRaw";

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

        public FreestandingMacroExpansionResult Expand(
            FreestandingMacroContext<CancellingTypedFreestandingMacroParameters> context)
        {
            CancellationSource?.Cancel();
            context.CancellationToken.ThrowIfCancellationRequested();
            return FreestandingMacroExpansionResult.FromExpression(ParseExpression("42"));
        }
    }

    public sealed class ValidatingFreestandingMacroParameters(int count)
    {
        public int Count { get; } = count;
    }

    public sealed class ValidatingFreestandingMacro : IFreestandingExpressionMacro<ValidatingFreestandingMacroParameters>
    {
        public string Name => "repeat";
        public MacroKind Kind => MacroKind.FreestandingExpression;

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

    public sealed class SubscribeMacro : IFreestandingExpressionMacro
    {
        public string Name => "subscribe";
        public MacroKind Kind => MacroKind.FreestandingExpression;
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
