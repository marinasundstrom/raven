using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IsPatternExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void EnumMemberChecks_WithJsonValueKind_AcceptQualifiedEqualityAndPatternForms()
    {
        const string code = """
import System.Text.Json.*

func Test(element: JsonElement) -> bool {
    return element.ValueKind == JsonValueKind.True ||
        element.ValueKind is JsonValueKind.True ||
        element.ValueKind is .True
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void EnumMemberChecks_WithJsonValueKind_RejectsTargetTypedEqualityShorthand()
    {
        const string code = """
import System.Text.Json.*

func Test(element: JsonElement) -> bool {
    return element.ValueKind == .True
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2010").WithAnySpan().WithArguments("True")]);

        verifier.Verify();
    }

    [Fact]
    public void ConstantMemberCheck_WithMathPi_AcceptsQualifiedPatternForm()
    {
        const string code = """
import System.*

func Test(value: double) -> bool {
    return value is Math.PI
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void IsPattern_WithIncompatibleLiteralPattern_ReportsDiagnostic()
    {
        const string code = """
record class Foo(Value: bool, Data: (int, int))

func Test(x: object?) {
    if x is Foo(_, true) {
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("bool", "(int, int)")]);

        verifier.Verify();
    }
}
