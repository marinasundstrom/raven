using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

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
    public void TryExpression_InferredType_IsCurrentlyErrorType()
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
        Assert.IsType<ErrorTypeSymbol>(local.Type);
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
    public void TryExpression_WithQuestionMark_PayloadType_IsCurrentlyUnresolved()
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
        Assert.Equal(SpecialType.None, localSymbol.Type.SpecialType);
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

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "try_question_mark_disallows_match",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV1908");
    }

    [Fact]
    public void TryExpression_WithAwait_TypeIsCurrentlyErrorType()
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
        Assert.IsType<ErrorTypeSymbol>(local.Type);
    }

    [Fact]
    public void TryExpression_WithAwait_PatternMatchingTypeFlow_IsCurrentlyUnresolved()
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

        Assert.IsType<ErrorTypeSymbol>(model.GetTypeInfo(tryExpression).Type);

        var matchExpression = root
            .DescendantNodes()
            .OfType<MatchExpressionSyntax>()
            .Single();

        Assert.Equal(SpecialType.System_String, model.GetTypeInfo(matchExpression).Type!.SpecialType);
    }

    [Fact]
    public void TryExpression_WithAwaitAndQuestionMark_LocalType_IsCurrentlyUnresolved()
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
        Assert.Equal(SpecialType.None, local.Type.SpecialType);
    }

}
