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
let value = try int.Parse("foo")
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "value");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        var union = Assert.IsAssignableFrom<IUnionTypeSymbol>(local.Type);

        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);

        var exceptionType = result.Compilation.GetTypeByMetadataName("System.Exception");
        Assert.NotNull(exceptionType);
        Assert.Contains(union.Types, t => SymbolEqualityComparer.Default.Equals(TypeSymbolNormalization.NormalizeForInference(t), exceptionType));

        verifier.Verify();
    }

    [Fact]
    public void TryExpression_NestedTryReportsDiagnostic()
    {
        var code = "let value = try try 1";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1906").WithSpan(1, 17, 1, 20)
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
        let attempt = try await Task.FromResult(1)
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
        var union = Assert.IsAssignableFrom<IUnionTypeSymbol>(local.Type);

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
}
