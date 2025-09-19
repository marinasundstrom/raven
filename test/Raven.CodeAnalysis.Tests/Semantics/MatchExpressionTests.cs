using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MatchExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void MatchExpression_WithTypeArms_MissingDefaultReportsDiagnostic()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    string text => text
    object obj => obj.ToString()
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("_")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDefaultArm_AllowsAssignment()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    string text => text
    object => value.ToString()
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithGuard_UsesDesignation()
    {
        const string code = """
func describe(value: object) -> string? {
    match value {
        string text when text.Length > 3 => text
        string text => text.ToUpper()
        object obj => obj.ToString()
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("_")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_AllCasesCovered()
    {
        const string code = """
let state: "on" | "off" = "on"

let result = match state {
    "on" => 1
    "off" => 0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_MissingArmReportsDiagnostic()
    {
        const string code = """
let state: "on" | "off" = "on"

let result = match state {
    "on" => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("\"off\"")]);

        verifier.Verify();
    }
}

