using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class BindingErrorCascadeSuppressionTests : DiagnosticTestBase
{
    [Fact]
    public void CastWithMissingTargetType_DoesNotReportConversionDiagnostic()
    {
        const string code = """
        let value = (MissingType)1
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("MissingType"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void AsWithMissingTargetType_DoesNotReportConversionDiagnostic()
    {
        const string code = """
        let value = 1 as MissingType
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("MissingType"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void IfConditionWithMissingName_DoesNotReportBoolConversionDiagnostic()
    {
        const string code = """
        let value = if missing {
            1
        } else {
            0
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("missing"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void InvocationWithMissingReceiverType_DoesNotReportMemberDiagnostic()
    {
        const string code = """
        let value = MissingType.Parse("")
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("MissingType"),
        ]);

        verifier.Verify();
    }
}
