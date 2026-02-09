using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExceptionHandlingTests : DiagnosticTestBase
{
    [Fact]
    public void TryStatement_WithoutCatchOrFinally_ReportsDiagnostic()
    {
        var code = "try { }";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1015").WithSpan(1, 7, 1, 8)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void CatchClause_WithNonExceptionType_ReportsDiagnostic()
    {
        var code = """
try {
}
catch (int ex) {
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1016").WithSpan(3, 8, 3, 11).WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_InferredTypeIncludesException()
    {
        var code = """
val value = try int.Parse("foo")
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "value");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);

        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);

        var exceptionType = result.Compilation.GetTypeByMetadataName("System.Exception");
        Assert.NotNull(exceptionType);
        Assert.Contains(union.Types, t => SymbolEqualityComparer.Default.Equals(TypeSymbolNormalization.NormalizeForInference(t), exceptionType));

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_NestedTryReportsDiagnostic()
    {
        var code = "val value = try try 1";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1906").WithSpan(1, 17, 1, 20)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_WithQuestionMark_PropagatesResultPayload()
    {
        var code = """
func ParseFlag(text: string) -> Result<bool, string> {
    val flag = try? bool.Parse(text)
    return .Ok(flag)
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var local = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(node => node.Identifier.Text == "flag");

        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(local));
        Assert.Equal(SpecialType.System_Boolean, localSymbol.Type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_WithQuestionMark_DisallowsMatch()
    {
        var code = """
func ParseFlag(text: string) -> Result<bool, string> {
    val flag = try? bool.Parse(text) match {
        true => true
        false => false
    }

    return .Ok(flag)
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1908").WithSpan(2, 38, 2, 42)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_WithAwait_IncludesExceptionInResultUnion()
    {
        var code = """
import System.Threading.Tasks.*

class C {
    async Work() {
        val attempt = try await Task.FromResult(1)
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(node => node.Identifier.Text == "attempt");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);

        Assert.Contains(
            union.Types,
            type => type.SpecialType == SpecialType.System_Int32);

        var exceptionType = result.Compilation.GetTypeByMetadataName("System.Exception");
        Assert.NotNull(exceptionType);
        Assert.Contains(
            union.Types,
            type => SymbolEqualityComparer.Default.Equals(
                TypeSymbolNormalization.NormalizeForInference(type),
                exceptionType));

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_WithAwait_AllowsPatternMatching()
    {
        var code = """
import System.*
import System.Threading.Tasks.*

class C {
    async Work() -> Task<string> {
        return try await Task.FromResult(1) match {
            int value => value.ToString()
            Exception ex => ex.Message
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var tryExpression = root
            .DescendantNodes()
            .OfType<TryExpressionSyntax>()
            .Single();

        var tryType = Assert.IsAssignableFrom<ITypeUnionSymbol>(model.GetTypeInfo(tryExpression).Type);
        Assert.Contains(
            tryType.Types,
            type => type.SpecialType == SpecialType.System_Int32);

        var exceptionType = result.Compilation.GetTypeByMetadataName("System.Exception");
        Assert.NotNull(exceptionType);
        Assert.Contains(
            tryType.Types,
            type => SymbolEqualityComparer.Default.Equals(
                TypeSymbolNormalization.NormalizeForInference(type),
                exceptionType));

        var matchExpression = root
            .DescendantNodes()
            .OfType<MatchExpressionSyntax>()
            .Single();

        var matchType = Assert.IsAssignableFrom<ITypeSymbol>(model.GetTypeInfo(matchExpression).Type);
        Assert.Equal(SpecialType.System_String, matchType.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_WithAwaitAndQuestionMark_PropagatesInAsyncMethod()
    {
        var code = """
import System.*
import System.Threading.Tasks.*

class C {
    async Work() -> Task<Result<int, Exception>> {
        val value = try? await Task.FromResult(1)
        return .Ok(value)
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(node => node.Identifier.Text == "value");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);

        verifier.Verify();
    }

}
