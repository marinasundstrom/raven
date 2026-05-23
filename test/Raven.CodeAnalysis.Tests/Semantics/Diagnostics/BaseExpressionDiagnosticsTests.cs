using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BaseExpressionDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void BaseExpression_InStaticMethod_ReportsDiagnostic()
    {
        const string source = """
class C {
    static func M() -> int {
        return base.GetHashCode()
    }
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.BaseExpressionNotAvailable.Id).WithSpan(3, 16, 3, 20)])
            .Verify();
    }

    [Fact]
    public void BaseExpression_InInterfaceMethod_ReportsDiagnostic()
    {
        const string source = """
interface I {
    public func M() -> int {
        return base.GetHashCode()
    }
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.BaseExpressionNotAvailable.Id).WithSpan(3, 16, 3, 20)])
            .Verify();
    }
}
