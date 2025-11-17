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

let result = value match {
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

let result = value match {
    string text => text
    object => value.ToString()
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithBooleanLiteralArms_IsExhaustive()
    {
        const string code = """
let value: bool = true

let result = value match {
    true => "true"
    false => "false"
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithBooleanLiteralArmsOnUnion_IsExhaustive()
    {
        const string code = """
let value: bool | (flag: bool, text: string) = false

let result = value match {
    true => "true"
    false => "false"
    (flag: bool, text: string) => "tuple ${text}"
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithNegativeNumericPattern_AllowsConstantArm()
    {
        const string code = """
let value: int = -1

let result = value match {
    -1 => "minus one"
    _ => "other"
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

let result = value match {
    string text => text
    object obj => obj.ToString()
    _ => ""
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDiscardArmOnNewLine_DoesNotInsertEmptyArm()
    {
        const string code = """
let result = false match {
    _ => "none"
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

let result = value match {
    string text => text
    object _ => value.ToString()
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTuplePatternOnUnion_BindsElementDesignations()
    {
        const string code = """
let x: bool | (a: int, b: string) = false

let result = x match {
    true => "hej"
    (a: int, b: string) => "tuple ${a} ${b}"
    _ => "none"
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "tuple_union_match",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var tupleArm = bound.Arms[1];
        var tuplePattern = Assert.IsType<BoundTuplePattern>(tupleArm.Pattern);

        Assert.Collection(tuplePattern.Elements,
            element =>
            {
                var declaration = Assert.IsType<BoundDeclarationPattern>(element);
                var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);
                Assert.Equal("a", designator.Local.Name);
            },
            element =>
            {
                var declaration = Assert.IsType<BoundDeclarationPattern>(element);
                var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);
                Assert.Equal("b", designator.Local.Name);
            });
    }

    [Fact]
    public void MatchExpression_WithUnionCasePatterns_BindsPayloads()
    {
        const string code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

func describe(result: Result<int>) -> string {
    result match {
        .Ok(let payload) => $"ok {payload}",
        .Error(let message) => $"error {message}",
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var okPattern = Assert.IsType<BoundUnionCasePattern>(bound.Arms[0].Pattern);
        Assert.Equal("Ok", okPattern.CaseType.Name);
        Assert.Single(okPattern.Arguments);
        var okArgument = Assert.IsType<BoundDeclarationPattern>(okPattern.Arguments[0]);
        var okDesignator = Assert.IsType<BoundSingleVariableDesignator>(okArgument.Designator);
        Assert.Equal("payload", okDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, okDesignator.Local.Type.SpecialType);

        var errorPattern = Assert.IsType<BoundUnionCasePattern>(bound.Arms[1].Pattern);
        Assert.Equal("Error", errorPattern.CaseType.Name);
        var errorArgument = Assert.IsType<BoundDeclarationPattern>(errorPattern.Arguments[0]);
        var errorDesignator = Assert.IsType<BoundSingleVariableDesignator>(errorArgument.Designator);
        Assert.Equal("message", errorDesignator.Local.Name);
        Assert.Equal(SpecialType.System_String, errorDesignator.Local.Type.SpecialType);
    }

    [Fact]
    public void MatchExpression_WithMissingUnionCase_ReportsDiagnostic()
    {
        const string code = """
union Result<T> {
    Ok(value: T)
    Error(message: string)
}

func describe(result: Result<int>) -> string {
    result match {
        .Ok(let payload) => payload.ToString()
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithArguments("Result<int>.Error").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionCaseOnNonUnion_ReportsDiagnostic()
    {
        const string code = """
let value = 42

let result = value match {
    .Ok => value
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV0403").WithArguments("Ok", "int").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnknownUnionCase_ReportsDiagnostic()
    {
        const string code = """
union Maybe<T> {
    Some(value: T)
}

func describe(value: Maybe<int>) -> int {
    value match {
        .None => 0
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV0404").WithArguments("Maybe<int>", "None").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionCaseArgumentCountMismatch_ReportsDiagnostic()
    {
        const string code = """
union Maybe<T> {
    Some(value: T)
}

func describe(value: Maybe<int>) -> int {
    value match {
        .Some(let value, let extra) => extra
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV0405").WithArguments("Some", "1", "2").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithAmbiguousUnionCase_ReportsDiagnostic()
    {
        const string code = """
union Alpha {
    Ready
}

union Beta {
    Ready
}

let choice: Alpha | Beta = Alpha.Ready()

let result = choice match {
    .Ready => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV0406").WithArguments("Ready").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDiscardArmNotLast_ReportsDiagnostic()
    {
        const string code = """
let value: object = "hello"

let result = value match {
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

let result = value match {
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

let result = value match {
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
    public void MatchExpression_WithVariablePattern_BindsDesignation()
    {
        const string code = """
let value: object = "hello"

let result = value match {
    let text => text
    _ => ""
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var variableArm = bound.Arms[0];
        var declaration = Assert.IsType<BoundDeclarationPattern>(variableArm.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        Assert.Equal("text", designator.Local.Name);
        Assert.False(designator.Local.IsMutable);
    }

    [Fact]
    public void MatchExpression_WithVarPattern_BindsMutableDesignation()
    {
        const string code = """
let value: object = "hello"

let result = value match {
    var text => text
    _ => ""
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var variableArm = bound.Arms[0];
        var declaration = Assert.IsType<BoundDeclarationPattern>(variableArm.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        Assert.Equal("text", designator.Local.Name);
        Assert.True(designator.Local.IsMutable);
    }

    [Fact]
    public void MatchExpression_WithTypedVariablePattern_UsesAnnotation()
    {
        const string code = """
let value: object = "hello"

let result = value match {
    let text: string => text
    _ => ""
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var variableArm = bound.Arms[0];
        var declaration = Assert.IsType<BoundDeclarationPattern>(variableArm.Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        var stringType = result.Compilation.GetSpecialType(SpecialType.System_String);
        Assert.True(SymbolEqualityComparer.Default.Equals(designator.Local.Type, stringType));
    }

    [Fact]
    public void MatchExpression_WithArrayTypePattern_BindsArrayType()
    {
        const string code = """
let value: object = [1, 2, 3]

let result = value match {
    int[] numbers => numbers.Length
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var declarationPattern = Assert.IsType<BoundDeclarationPattern>(bound.Arms[0].Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declarationPattern.Designator);
        Assert.Equal("numbers", designator.Local.Name);

        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(declarationPattern.Type);
        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(arrayType.ElementType, intType));
    }

    [Fact]
    public void MatchExpression_WithGuard_UsesDesignation()
    {
        const string code = """
func describe(value: object) -> string? {
    value match {
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

let result = state match {
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

let result = value match {
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
    public void MatchExpression_AfterIfExpression_EvaluatesScrutineeOnce()
    {
        const string code = """
func describe(input: bool) -> string {
    if input {
        1
    } else {
        2
    } match {
        1 => "one"
        _ => "two"
    }
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

let result = state match {
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

let result = state match {
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

let result = state match {
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

let result = state match {
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

let result = input match {
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

let result = input match {
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

let result = pair match {
    (first: int, second: string) => second
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

let result = pair match {
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

let result = value match {
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

let result = value match {
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

let result = value match {
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

