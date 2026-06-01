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
    public void DefaultLiteral_TargetTyped_NonNullableReference_UsesNullableContextualType()
    {
        const string code = """
        val text: string = default
        """;

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        var nullable = Assert.IsType<NullableTypeSymbol>(type);
        Assert.Equal(SpecialType.System_String, nullable.UnderlyingType.SpecialType);
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

    [Fact]
    public void DefaultLiteral_ReturnedAsNonNullableReference_ReportsDiagnostic()
    {
        const string code = """
        import System.*

        func Test2() -> IDisposable {
            return default
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotAssignNullToType.Id)
                .WithAnySpan()
                .WithArguments("IDisposable")
        ]);

        verifier.Verify();
    }

    [Fact]
    public void DefaultLiteral_ReturnedAsNonNullableReference_AllowsNullForgiving()
    {
        const string code = """
        import System.*

        func Test2() -> IDisposable {
            return default!
        }
        """;

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NullableSuppressionUsed.Id)
                    .WithSpan(4, 12, 4, 20)
                    .WithSeverity(DiagnosticSeverity.Warning)
            ]);

        verifier.Verify();
    }
}
