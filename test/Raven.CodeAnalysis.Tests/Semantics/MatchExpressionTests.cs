using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

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
    public void MatchExpression_WithDiscardArm_BindsDesignation()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    string text => text
    object obj => obj.ToString()
    _ => ""
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTypedDiscardArm_IsCatchAll()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    string text => text
    object _ => value.ToString()
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDiscardArmNotLast_ReportsDiagnostic()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    _ => ""
    string text => text
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2101").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTypedDiscardArmNotLast_ReportsDiagnostic()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    object _ => value.ToString()
    string text => text
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2101").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_DiscardArm_BindsToDiscardPattern()
    {
        const string code = """
let value: object = "hello"

let result = match value {
    string text => text
    object obj => obj.ToString()
    _ => "None"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "discard_match",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        Assert.IsType<BoundDiscardPattern>(bound.Arms.Last().Pattern);
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
    public void MatchExpression_WithNullArm_BindsToConstantPattern()
    {
        const string code = """
let value: string | null = null

let result = match value {
    null => "empty"
    string text => text
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_null_arm",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var constantPattern = Assert.IsType<BoundConstantPattern>(bound.Arms.First().Pattern);
        Assert.Null(constantPattern.ConstantValue);
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

    [Fact]
    public void MatchExpression_WithUnionScrutinee_MultipleMissingArmsReportDiagnostics()
    {
        const string code = """
let state: "on" | "off" | "unknown" = "on"

let result = match state {
    "on" => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("\"off\""),
                new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("\"unknown\""),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_RedundantCatchAllReportsDiagnostic()
    {
        const string code = """
let state: "on" | "off" = "on"

let result = match state {
    "on" => 1
    "off" => 0
    _ => -1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan().WithSeverity(DiagnosticSeverity.Warning)]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_CatchAllWithGuardDoesNotReportDiagnostic()
    {
        const string code = """
let state: "on" | "off" = "on"

let result = match state {
    "on" => 1
    "off" when false => 0
    _ => -1
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutineeAndGuard_NotExhaustiveWithoutCatchAll()
    {
        const string code = """
let input: string | int | null = ""

let result = match input {
    null => "Nothing to report."
    string text when text.Length > 0 => "Saw \"${text}\""
    int number => "Counted ${number}"
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("string")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutineeIncludingNull_DoesNotReportMissingNull()
    {
        const string code = """
let input: string | null = null

let result = match input {
    null => "Nothing to report."
    string text => text
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTuplePattern_BindsTupleElements()
    {
        const string code = """
let pair: object = (1, "two")

let result = match pair {
    (int first, string second) => second
    _ => ""
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "tuple_match",
                [tree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var tuplePattern = Assert.IsType<BoundTuplePattern>(boundMatch.Arms[0].Pattern);
        Assert.Equal(2, tuplePattern.Elements.Length);

        var firstElement = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstElement.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);

        var secondElement = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondElement.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);

        var tupleType = Assert.IsAssignableFrom<ITupleTypeSymbol>(tuplePattern.Type);
        Assert.Equal(2, tupleType.TupleElements.Length);
    }

    [Fact]
    public void MatchExpression_WithTuplePatternLengthMismatch_ReportsDiagnostic()
    {
        const string code = """
let pair: (int, int) = (1, 2)

let result = match pair {
    (int a, int b, int c) => c
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("for type '(int, int, int)'", "(int, int)")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithIncompatiblePattern_ReportsDiagnostic()
    {
        const string code = """
let value: int = 0

let result = match value {
    string text => text
    _ => ""
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("for type 'string'", "int")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutineeAndIncompatiblePattern_ReportsDiagnostic()
    {
        const string code = """
let value: "on" | "off" = "on"

let result = match value {
    bool flag => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("for type 'bool'", "\"on\" | \"off\"")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithIncompatibleLiteralPattern_ReportsDiagnostic()
    {
        const string code = """
let value: int = 0

let result = match value {
    "foo" => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("\"foo\"", "int")]);

        verifier.Verify();
    }
}

