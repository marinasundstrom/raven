using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

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
        var identifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(identifier => identifier.Identifier.Text == "SB");
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
            expectedDiagnostics: [new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                .WithArguments("string", "int")]);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_Union()
    {
        string testCode =
            """
            alias Number = int | string

            func Accept(value: Number) -> unit {
            }
            """;

        var tree = SyntaxTree.ParseText(testCode);
        var compilation = Compilation.Create(
                "AliasUnion",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenCore);

        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var parameter = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParameterSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameter));
        Assert.True(symbol.Type.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol.Type);
        Assert.Equal("Number", alias.Name);
        var union = Assert.IsAssignableFrom<INamedTypeSymbol>(alias.UnderlyingSymbol);
        Assert.Equal("Union", union.Name);
        Assert.Equal("System.Union`2", union.OriginalDefinition.ToFullyQualifiedMetadataName());
        Assert.Collection(
            union.TypeArguments,
            argument => Assert.Equal(SpecialType.System_Int32, argument.SpecialType),
            argument => Assert.Equal(SpecialType.System_String, argument.SpecialType));
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
    public void AliasDirective_RepeatedMethodAlias_FormsOverloadSet()
    {
        string testCode =
            """
            alias Print = System.Console.WriteLine
            alias Print = System.Console.WriteLine

            Print()
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = model.GetSymbolInfo(invocation).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("Print", alias.Name);
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
                val sb: SB = SB()
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
            .First(identifier => identifier.Identifier.Text == "SB");
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("SB", alias.Name);
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
            disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }
}
