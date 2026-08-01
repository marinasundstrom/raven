using System.Linq;

using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class StringExpressionErrorRecoveryTests : DiagnosticTestBase
{
    [Fact]
    public void InterpolatedString_WithUndefinedExpression_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            func Test() -> unit {
                let value = "Value: ${missing}"
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("missing")
        ]);

        verifier.Verify();
    }

    [Fact]
    public void StringConcatenation_WithUndefinedExpression_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            func Test() -> unit {
                let value = "Value: " + missing
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("missing")
        ]);

        verifier.Verify();
    }

}

public class StringExpressionSemanticRecoveryTests : CompilationTestBase
{
    [Fact]
    public void IncompleteInterpolation_BindsWithoutThrowing()
    {
        const string code = """
            func Main() {
                let value = "Value: ${missing
            }
            """;
        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var interpolatedString = tree.GetRoot()
            .DescendantNodes()
            .OfType<InterpolatedStringExpressionSyntax>()
            .Single();

        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.NotNull(model.GetBoundNode(interpolatedString));
        Assert.Equal(SpecialType.System_String, model.GetTypeInfo(interpolatedString).Type?.SpecialType);
    }
}
