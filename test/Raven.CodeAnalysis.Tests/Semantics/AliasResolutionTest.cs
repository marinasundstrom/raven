using System.Linq;

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

            SB
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

            IntList
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

            let list: StringList
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
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

            ST.StringBuilder
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();
        verifier.Verify();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var identifier = (IdentifierNameSyntax)((QualifiedNameSyntax)tree.GetRoot().DescendantNodes().OfType<QualifiedNameSyntax>().First()).Left;
        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.NotNull(symbol);
        Assert.True(symbol!.IsAlias);
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);
        Assert.Equal("ST", alias.Name);
        Assert.Equal(SymbolKind.Namespace, alias.UnderlyingSymbol.Kind);
    }
}
