using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AliasResolutionTest : DiagnosticTestBase
{
    [Fact]
    public void AliasDirective_UsesAlias()
    {
        string testCode =
            """
            alias SB = System.Text.StringBuilder

            let sb: SB = SB()
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().Last();
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("SB", alias.Name);
        Assert.Equal("StringBuilder", alias.UnderlyingSymbol.Name);
    }

    [Fact]
    public void AliasDirective_UsesAlias_Generic()
    {
        string testCode =
            """
            alias IntList = System.Collections.Generic.List<int>

            let list: IntList = IntList()
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_AsTypeAnnotation()
    {
        string testCode =
            """
            alias StringList = System.Collections.Generic.List<string>

            let list: StringList = StringList()
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_Tuple()
    {
        string testCode =
            """
            alias Pair = (x: int, y: int)

            let p: Pair = (1, 2)
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().First(id => id.Identifier.Text == "Pair");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
    }

    [Fact]
    public void AliasDirective_UsesAlias_Tuple_WithNamedLiteral()
    {
        string testCode =
            """
            alias Pair = (x: int, y: int)

            let p: Pair = (x: 1, y: 2)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_Tuple_TypeMismatch_ReportsDiagnostic()
    {
        string testCode =
            """
            alias Pair = (x: int, y: int)

            let p: Pair = (1, "")
            """;

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)]);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_Union()
    {
        string testCode =
            """
            alias Number = int | string

            let n: Number = 1
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().First(id => id.Identifier.Text == "Number");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
    }

    [Fact]
    public void AliasDirective_UsesAlias_Literal()
    {
        string testCode =
            """
            alias Five = 5

            let x: Five = 5
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().First(id => id.Identifier.Text == "Five");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.IsType<LiteralTypeSymbol>(alias.UnderlyingSymbol);
    }

    [Fact]
    public void AliasDirective_UsesMemberAlias_Method()
    {
        string testCode =
            """
            alias PrintLine = System.Console.WriteLine

            PrintLine(123)
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();
        var symbol = model.GetSymbolInfo(invocation).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("WriteLine", alias.UnderlyingSymbol.Name);
    }

    [Fact]
    public void AliasDirective_UsesNamespaceAlias()
    {
        string testCode =
            """
            alias ST = System.Text

            let sb: ST.StringBuilder = ST.StringBuilder()
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.Text == "ST");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("ST", alias.Name);
        Assert.Equal(SymbolKind.Namespace, alias.UnderlyingSymbol.Kind);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();
        var ctorSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol!);
        Assert.Equal(".ctor", ctorSymbol.Name);
        Assert.Equal("StringBuilder", ctorSymbol.ContainingType!.Name);
    }

    [Fact]
    public void AliasDirective_UsesAlias_InsideClass()
    {
        string testCode =
            """
            alias SB = System.Text.StringBuilder

            class C
            {
                let sb: SB = SB()
            }
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.Text == "SB");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("StringBuilder", alias.UnderlyingSymbol.Name);
    }

    [Fact]
    public void AliasDirective_UsesAlias_PredefinedType()
    {
        string testCode =
            """
            alias MyInt = int

            let x: MyInt = 0
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().First(id => id.Identifier.Text == "MyInt");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
    }

    [Fact]
    public void AliasDirective_InvalidTypeSyntax_ReportsDiagnostic()
    {
        string testCode = "alias Bad = notatype";

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [new DiagnosticResult("RAV2020").WithSeverity(DiagnosticSeverity.Error).WithSpan(1, 13, 1, 21)],
            disabledDiagnostics: ["RAV0103"]);

        verifier.Verify();
    }
}
