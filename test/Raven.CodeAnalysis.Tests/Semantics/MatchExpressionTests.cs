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
    public void MatchExpression_InValuePosition_BindsDirectlyAsBoundMatchExpression()
    {
        const string code = """
val result = 1 match {
    1 => 10
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_bound_shape",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));
        Assert.Equal(2, bound.Arms.Length);
    }

    [Fact]
    public void MatchExpression_WithTypeArms_MissingDefaultReportsDiagnostic()
    {
        const string code = """
val value: object = "hello"

val result = value match {
    string text => text
    object obj => obj.ToString()
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDefaultArm_AllowsAssignment()
    {
        const string code = """
val value: object = "hello"

val result = value match {
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
val value: bool = true

val result = value match {
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
val value: bool | (flag: bool, text: string) = false

val result = value match {
    true => "true"
    false => "false"
    (val flag: bool, val text: string) => "tuple ${text}"
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithNegativeNumericPattern_AllowsConstantArm()
    {
        const string code = """
val value: int = -1

val result = value match {
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
val value: object = "hello"

val result = value match {
    string text => text
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
val result = false match {
    _ => "none"
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTargetTypedMemberPattern_ResolvesAgainstInputType()
    {
        const string code = """
enum Species {
    Human,
    Dog
}

class Character(name: string, species: Species, age: int) {
    public Name: string => name

    public Species: Species => species

    public Age: int => age
}

val character = new Character("Rex", .Dog, 4)

val result = character match {
    { Age: not > 34, Species: .Dog } => true
    _ => false
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithEnumArms_MissingCaseReportsDiagnostic()
    {
        const string code = """
enum Color {
    Red,
    Green,
    Blue
}

val value: Color = .Red

val result = value match {
    .Red => 1
    .Green => 2
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("Blue")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTypedDiscardArm_IsCatchAll()
    {
        const string code = """
val value: object = "hello"

val result = value match {
    string text => text
    object _ => value.ToString()
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV0134").WithAnySpan(),
                new DiagnosticResult("RAV0103").WithAnySpan().WithArguments(string.Empty),
                new DiagnosticResult("RAV2101").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithPositionalPatternOnUnion_BindsElementDesignations()
    {
        const string code = """
val x: bool | (a: int, b: string) = false

val result = x match {
    true => "hej"
    (val a: int, val b: string) => "tuple ${a} ${b}"
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
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(tupleArm.Pattern);

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
    public void MatchExpression_WithAbruptArms_DoesNotPolluteValueTypeInference()
    {
        const string code = """
import System.*

func Test(y: int) -> int {
    val r = y match {
        0 => return 0
        1 => 42
        _ => throw Exception("x")
    }

    return r + 1
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithCollectionPatternOnArray_BindsElementDesignations()
    {
        const string code = """
val items: int[] = [1, 2]

val result = items match {
    [val first, val second] => first + second
    _ => 0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "array_collection_match",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal("int[]", collectionPattern.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));

        Assert.Collection(collectionPattern.Elements,
            element =>
            {
                var declaration = Assert.IsType<BoundDeclarationPattern>(element);
                var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);
                Assert.Equal("first", designator.Local.Name);
            },
            element =>
            {
                var declaration = Assert.IsType<BoundDeclarationPattern>(element);
                var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);
                Assert.Equal("second", designator.Local.Name);
            });
    }

    [Fact]
    public void MatchExpression_WithDiscardArmNotLast_ReportsDiagnostic()
    {
        const string code = """
val value: object = "hello"

val result = value match {
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
val value: object = "hello"

val result = value match {
    object _ => value.ToString()
    string text => text
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV0134").WithAnySpan(),
                new DiagnosticResult("RAV0103").WithAnySpan().WithArguments(string.Empty),
                new DiagnosticResult("RAV2101").WithAnySpan(),
                new DiagnosticResult("RAV2101").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_DiscardArm_BindsToDiscardPattern()
    {
        const string code = """
val value: object = "hello"

val result = value match {
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
val value: object = "hello"

val result = value match {
    val text => text
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
val value: object = "hello"

val result = value match {
    var text => text
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
val value: object = "hello"

val result = value match {
    val text: string => text
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
val value: object = [1, 2, 3]

val result = value match {
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

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_AllCasesCovered()
    {
        const string code = """
val state: "on" | "off" = "on"

val result = state match {
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
val value: string | null = null

val result = value match {
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
    (if input {
        1
    } else {
        2
    }) match {
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
val state: "on" | "off" = "on"

val result = state match {
    "on" => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("string")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_MultipleMissingArmsReportDiagnostics()
    {
        const string code = """
val state: "on" | "off" | "unknown" = "on"

val result = state match {
    "on" => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("string")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_RedundantCatchAllReportsDiagnostic()
    {
        const string code = """
val state: "on" | "off" = "on"

val result = state match {
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
val state: "on" | "off" = "on"

val result = state match {
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
val input: string | int | null = ""

val result = input match {
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
val input: string | null = null

val result = input match {
    null => "Nothing to report."
    string text => text
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithPositionalPattern_BindsTupleElements()
    {
        const string code = """
val pair: object = (1, "two")

val result = pair match {
    (val first: int, val second: string) => second
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

        var tuplePattern = Assert.IsType<BoundPositionalPattern>(boundMatch.Arms[0].Pattern);
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
    public void MatchExpression_WithPositionalPatternLengthMismatch_ReportsDiagnostic()
    {
        const string code = """
val pair: (int, int) = (1, 2)

val result = pair match {
    (int a, int b, int c) => c
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("for type '(int, int, int)'", "(int, int)"),
                new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("_"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithIncompatiblePattern_ReportsDiagnostic()
    {
        const string code = """
val value: int = 0

val result = value match {
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
val value: "on" | "off" = "on"

val result = value match {
    bool flag => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("for type 'bool'", "string | string")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithIncompatibleLiteralPattern_ReportsDiagnostic()
    {
        const string code = """
val value: int = 0

val result = value match {
    "foo" => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("string", "int"),
                new DiagnosticResult("RAV2101").WithAnySpan(),
            ]);

        verifier.Verify();
    }
}
