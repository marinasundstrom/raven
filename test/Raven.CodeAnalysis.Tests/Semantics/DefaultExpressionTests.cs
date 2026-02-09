using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DefaultExpressionTests : CompilationTestBase
{
    [Fact]
    public void DefaultExpression_WithExplicitType_UsesType()
    {
        const string code = """
        val value: int = default(int)
        """;

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        Assert.Equal(SpecialType.System_Int32, type.SpecialType);
    }

    [Fact]
    public void DefaultLiteral_TargetTyped_UsesContextualType()
    {
        const string code = """
        val text: string = default
        """;

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        Assert.Equal(SpecialType.System_String, type.SpecialType);
    }
}

public class DefaultExpressionDiagnosticTests : DiagnosticTestBase
{
    [Fact]
    public void DefaultLiteral_WithoutTargetType_ReportsDiagnostic()
    {
        const string code = """
        val value = default
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV2011").WithSpan(1, 13, 1, 20)
        ]);

        verifier.Verify();
    }
}
