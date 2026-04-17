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
    public void MatchExpression_WithOpenGenericDeclarationPattern_InfersTypeArgumentsFromScrutinee()
    {
        const string code = """
class Box<T> {}

val value: Box<int> = Box<int>()

val result = value match {
    Box box => 1
}
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);
        Assert.DoesNotContain(
            run.Compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);

        var tree = run.Compilation.SyntaxTrees.First(tree => tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Any());
        var model = run.Compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var declaration = Assert.IsType<BoundDeclarationPattern>(bound.Arms[0].Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        Assert.Equal("Box<int>", declaration.DeclaredType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Box<int>", designator.Local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
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
    public void MatchExpression_WithDictionaryPattern_BindsEntries()
    {
        const string code = """
import System.Collections.Generic.*

val values: Dictionary<string, int> = !["a": 1, "b": 2]

val result = values match {
    ["a": val first, "b": 2] => first
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.First(tree => tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Any());
        var model = run.Compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var dictionaryPattern = Assert.IsType<BoundDictionaryPattern>(bound.Arms[0].Pattern);
        Assert.Equal(2, dictionaryPattern.Entries.Length);
        Assert.Equal(SpecialType.System_String, dictionaryPattern.KeyType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryPattern.ValueType.SpecialType);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(dictionaryPattern.Entries[0].Pattern);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, firstDesignator.Local.Type.SpecialType);

        Assert.IsType<BoundConstantPattern>(dictionaryPattern.Entries[1].Pattern);
    }

    [Fact]
    public void MatchExpression_WithDictionaryPatternOnNonDictionaryType_ReportsDictionaryDiagnostic()
    {
        const string code = """
val value = 42

val result = value match {
    ["a": 1] => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.DictionaryPatternRequiresDictionaryType);
    }

    [Fact]
    public void MatchExpression_WithDuplicateDictionaryPatternKeys_ReportsDuplicateKey()
    {
        const string code = """
import System.Collections.Generic.*

val values: Dictionary<string, int> = !["a": 1]

val result = values match {
    ["a": val first, "a": 1] => first
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.DuplicateDictionaryKey);
    }

    [Fact]
    public void MatchExpression_WithDuplicatePropertyPatternMembers_ReportsDiagnostic()
    {
        const string code = """
class Box {
    val Value: int
}

val value = Box(Value: 1)

val result = value match {
    Box { Value: 1, Value: val other } => other
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.DuplicatePropertyPatternMember);
    }

    [Fact]
    public void MatchExpression_WithUnknownNamedNominalDeconstructionMember_ReportsDiagnostic()
    {
        const string code = """
val value: object = Person("Ada", 42)

val result = value match {
    Person(Height: 170, Name: val name) => name
    _ => ""
}

record class Person(Name: string, Age: int)
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PropertyPatternMemberNotFound);
    }

    [Fact]
    public void MatchExpression_WithUndefinedNestedNominalDeconstructionPattern_ReportsInvalidArmPattern()
    {
        const string code = """
union UserOrError {
    Ok(value: int)
    Error(error: string)
}

func GetUser() -> UserOrError {
    return .Ok(1)
}

val result = GetUser() match {
    .Ok(User(val name, val isActive)) => 1
    .Error(val error) => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                    .WithAnySpan()
                    .WithArguments("User"),
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmPatternInvalid.Id)
                    .WithAnySpan()
                    .WithArguments("for type 'User'", "int"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithMismatchedNestedNominalDeconstructionPattern_ReportsTypeMismatch()
    {
        const string code = """
union UserOrError {
    Ok(value: int)
    Error(error: string)
}

func GetUser() -> UserOrError {
    return .Ok(1)
}

val result = GetUser() match {
    .Ok(User(val name, val isActive)) => 1
    .Error(val error) => 0
}

record class User(Name: string, IsActive: bool);
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id)
                    .WithAnySpan()
                    .WithArguments("Ok"),
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmPatternInvalid.Id)
                    .WithAnySpan()
                    .WithArguments("for type 'User'", "int"),
                new DiagnosticResult(CompilerDiagnostics.NominalDeconstructionPatternTypeMismatch.Id)
                    .WithAnySpan()
                    .WithArguments("int", "User"),
            ]);

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
union Value {
    Flag(value: bool)
    Pair(flag: bool, text: string)
}

val value: Value = .Flag(value: false)

val result = value match {
    .Flag(val flag) => if flag { "true" } else { "false" }
    .Pair(val flag, val text) => "tuple ${text}"
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithBarSeparatedConstantPatterns_BindsAsAlternative()
    {
        const string code = """
func ping(name: string) -> string {
    return name match {
        "Bob" | "bob" => "pong"
        _ => "invalid"
    }
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
    val Name: string {
        get { return name }
    }

    val Species: Species {
        get { return species }
    }

    val Age: int {
        get { return age }
    }
}

val character = Character("Rex", .Dog, 4)

val result = character match {
    { Age: not > 34, Species: .Dog } => true
    _ => false
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithTargetTypedSealedHierarchyCasePatterns_ResolvesAgainstInputType()
    {
        const string code = """
sealed interface Expr<T> {
    record NumericalExpr(Value: float) : Expr<float>
    record AddExpr(Left: Expr<float>, Right: Expr<float>) : Expr<float>
}

func Main() {
}

func Evaluate(expr: Expr<float>) -> int {
    match expr {
        .NumericalExpr(val value) => 1
        .AddExpr(val left, val right) => 2
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "sealed_hierarchy_target_typed_member_pattern",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var match = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        Assert.Equal(2, match.Arms.Count);

        var model = compilation.GetSemanticModel(tree);
        var valueDesignation = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(designation => designation.Identifier.ValueText == "value");
        var valueLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(valueDesignation));

        Assert.Equal(SpecialType.System_Single, valueLocal.Type.SpecialType);
    }

    [Fact]
    public void MatchExpression_WithConstrainedGenericSealedHierarchyCases_BindsAgainstScrutineeTypeArguments()
    {
        const string code = """
import System.Numerics.*

sealed interface Expr<T>
    where T: INumber<T> {
    record Literal<T>(Value: T) : Expr<T>
        where T: INumber<T>

    record Add<T>(Left: Expr<T>, Right: Expr<T>) : Expr<T>
        where T: INumber<T>
}

func Evaluate<T>(expr: Expr<T>) -> T
    where T: INumber<T> {
    return expr match {
        .Literal(val value) => value
        .Add(val left, val right) => Evaluate(left) + Evaluate(right)
    }
}

func Main() {
    val expr = Expr.Add<int>(Expr.Literal<int>(40), Expr.Literal<int>(2))
    Evaluate(expr)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "sealed_hierarchy_constrained_generic_cases",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
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
    public void MatchExpression_WithEnumArms_MissingCase_ReportsDiagnostic()
    {
        const string code = """
class Program {
    func eval(color: Color) -> int {
        return color match {
            .Red => 1
        }
    }
}

enum Color {
    Red
    Blue
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_enum_missing_case_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor.Id == "RAV2100");
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
                new DiagnosticResult(CompilerDiagnostics.DiscardExpressionNotAllowed.Id).WithAnySpan(),
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments(string.Empty),
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_InReturnContext_TargetTypesArmMemberBindings()
    {
        const string code = """
enum PingStatus {
    Ok,
    Error
}

func ping(name: string) -> PingStatus {
    return name match {
        "Bob" | "bob" => .Ok
        _ => .Error
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithPositionalPatternOnUnion_BindsElementDesignations()
    {
        const string code = """
union Value {
    Bool(flag: bool)
    Pair(a: int, b: string)
}

val x: Value = .Bool(flag: false)

val result = x match {
    .Bool(val flag) => "hej"
    .Pair(val a, val b) => "tuple ${a} ${b}"
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

        Assert.Equal(2, bound.Arms.Length);
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
    public void MatchExpression_WithCollectionPatternRestOnArray_BindsRestDesignation()
    {
        const string code = """
val items: int[] = [1, 2, 3, 4]

val result = items match {
    [val first, ..val middle, val last] => first + middle[0] + last
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "array_collection_match_rest",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal(1, collectionPattern.RestIndex);

        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("middle", restDesignator.Local.Name);
        Assert.True(restDesignator.Local.Type is IArrayTypeSymbol { ElementType.SpecialType: SpecialType.System_Int32 });
    }

    [Fact]
    public void MatchExpression_WithCollectionPatternRestOnList_PreservesRestDesignationType()
    {
        const string code = """
import System.Collections.Generic.*

val items: List<int> = [1, 2, 3, 4]

val result = items match {
    [val first, ..val middle, val last] => first + middle[0] + last
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "list_collection_match_rest",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal(1, collectionPattern.RestIndex);

        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("middle", restDesignator.Local.Name);
        Assert.Equal("System.Collections.Generic.List`1", ((INamedTypeSymbol)restDesignator.Local.Type).OriginalDefinition.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void MatchExpression_WithCollectionPatternRestOnImmutableList_PreservesRestDesignationType()
    {
        const string code = """
import System.Collections.Immutable.*

val items: ImmutableList<int> = [1, 2, 3, 4]

val result = items match {
    [val first, ..val middle, val last] => first + middle[0] + last
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "immutable_list_collection_match_rest",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal(1, collectionPattern.RestIndex);

        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("middle", restDesignator.Local.Name);
        Assert.Equal("System.Collections.Immutable.ImmutableList`1", ((INamedTypeSymbol)restDesignator.Local.Type).OriginalDefinition.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void MatchExpression_WithCollectionPatternRestOnFixedArray_BindsFixedSizeRestDesignation()
    {
        const string code = """
val items: int[4] = [1, 2, 3, 4]

val result = items match {
    [val first, val second, ...val rest] => first + second + rest.Length
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "fixed_array_collection_match_rest",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[2]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("rest", restDesignator.Local.Name);
        Assert.True(restDesignator.Local.Type is IArrayTypeSymbol
        {
            ElementType.SpecialType: SpecialType.System_Int32,
            FixedLength: 2
        });
    }

    [Fact]
    public void MatchExpression_WithTrailingTripleDotCollectionPattern_BindsDiscardRest()
    {
        const string code = """
val items: int[] = [1, 2, 3, 4]

val result = items match {
    [val first, ...] => first
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "array_collection_match_trailing_discard_rest",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal(1, collectionPattern.RestIndex);
        Assert.IsType<BoundDiscardPattern>(collectionPattern.Elements[1]);
    }

    [Fact]
    public void MatchExpression_WithMiddleTripleDotCollectionPattern_BindsDiscardRest()
    {
        const string code = """
val items: int[] = [1, 2, 3, 4]

val result = items match {
    [val first, ..., val last] => first + last
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "array_collection_match_middle_discard_rest",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal(1, collectionPattern.RestIndex);
        Assert.IsType<BoundDiscardPattern>(collectionPattern.Elements[1]);
    }

    [Fact]
    public void MatchExpression_WithStringCollectionFixedSegment_BindsStringSliceDesignation()
    {
        const string code = """
val text = "rune"

val result = text match {
    [val first, ..2 val middle, val last] => middle
    _ => ""
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "string_collection_match_fixed_segment",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));

        var collectionPattern = Assert.IsType<BoundPositionalPattern>(bound.Arms[0].Pattern);
        Assert.Equal(BoundPositionalPattern.SequenceElementKind.Single, collectionPattern.ElementKinds[0]);
        Assert.Equal(BoundPositionalPattern.SequenceElementKind.FixedSegment, collectionPattern.ElementKinds[1]);
        Assert.Equal(BoundPositionalPattern.SequenceElementKind.Single, collectionPattern.ElementKinds[2]);

        var middlePattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var middleDesignator = Assert.IsType<BoundSingleVariableDesignator>(middlePattern.Designator);
        Assert.Equal("middle", middleDesignator.Local.Name);
        Assert.Equal(SpecialType.System_String, middleDesignator.Local.Type.SpecialType);
    }

    [Fact]
    public void MatchExpression_WithCollectionPatternOnEnumerable_ReportsDiagnostic()
    {
        const string code = """
import System.Collections.Generic.*
import System.Linq.*

val items: IEnumerable<int> = [1, 2, 3].Where(v => v > 0)

val result = items match {
    [val first, val second] => first + second
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmPatternInvalid.Id)
                    .WithAnySpan()
                    .WithArguments("for a sequence pattern", "IEnumerable<int>")
            ]);

        verifier.Verify();
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
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan()]);

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
                new DiagnosticResult(CompilerDiagnostics.DiscardExpressionNotAllowed.Id).WithAnySpan(),
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments(string.Empty),
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan(),
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDuplicateCasePattern_ReportsDiagnostic()
    {
        const string code = """
union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

val value: Result<int, string> = Ok(2)

val result = value match {
    Ok(2) => "Lucky you!"
    Ok(2) => "Still lucky!"
    Ok(val payload) => payload.ToString()
    Error(val err) => err
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan()]);

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
union State {
    On
    Off
}

val state: State = .On

val result = state match {
    .On => 1
    .Off => 0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithNullArm_BindsToConstantPattern()
    {
        const string code = """
val value: string? = null

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
union State {
    On
    Off
}

val state: State = .On

val result = state match {
    .On => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("Off")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDiscriminatedUnionScrutinee_MissingArmOmitsCaseGenericTypeArgumentsInDiagnostic()
    {
        const string code = """
val value: Result<int, string> = Ok(1)

val result = value match {
    Ok(val payload) => payload
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("Error")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_MissingArmReportsDiagnosticAtMatchKeyword()
    {
        const string code = """
union State {
    On
    Off
}

val state: State = .On

val result = state match {
    .On => 1
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_missing_arm_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2100"));
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(match.MatchKeyword.GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_MultipleMissingArmsReportDiagnostics()
    {
        const string code = """
union State {
    On
    Off
    Unknown
}

val state: State = .On

val result = state match {
    .On => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("Off"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_MultipleMissingArmsReportDiagnosticsAtMatchKeyword()
    {
        const string code = """
union State {
    On
    Off
    Unknown
}

val state: State = .On

val result = state match {
    .On => 1
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_union_multiple_missing_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2100").ToArray();
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var expectedLocation = match.MatchKeyword.GetLocation();

        Assert.NotEmpty(diagnostics);
        Assert.All(diagnostics, d => Assert.Equal(expectedLocation, d.Location));
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_RedundantCatchAllReportsDiagnostic()
    {
        const string code = """
union State {
    On
    Off
}

val state: State = .On

val result = state match {
    .On => 1
    .Off => 0
    _ => -1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan().WithSeverity(DiagnosticSeverity.Warning)]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_RedundantCatchAllReportsDiagnosticAtCatchAllPattern()
    {
        const string code = """
union State {
    On
    Off
}

val state: State = .On

val result = state match {
    .On => 1
    .Off => 0
    _ => -1
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_redundant_catch_all_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2103"));
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(match.Arms[2].Pattern.GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void MatchExpression_WithUnionScrutinee_CatchAllWithGuardDoesNotReportDiagnostic()
    {
        const string code = """
union State {
    On
    Off
}

val state: State = .On

val result = state match {
    .On => 1
    .Off when false => 0
    _ => -1
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDiscriminatedUnionScrutinee_RedundantCatchAllReportsDiagnostic()
    {
        const string code = """
val result: Result<int> = .Ok(value: 1)

val value = result match {
    .Ok(val payload) => payload
    .Error(val message) => 0
    _ => -1
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan().WithSeverity(DiagnosticSeverity.Warning)]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithDiscriminatedUnionScrutinee_RedundantCatchAllReportsDiagnosticAtCatchAllPattern()
    {
        const string code = """
val result: Result<int> = .Ok(value: 1)

val value = result match {
    .Ok(val payload) => payload
    .Error(val message) => 0
    _ => -1
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_du_redundant_catch_all_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2103"));
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(match.Arms[2].Pattern.GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void MatchExpression_WithBodyFormUnionCasePatterns_IsExhaustive()
    {
        const string code = """
union Response<T> {
    Success(value: T)
    Failure(message: string)
}

func Describe(result: Response<int>) -> string {
    return result match {
        Success(val value) => value.ToString()
        Failure(val message) => message
    }
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithPureDeconstructionInsideUnionCase_RedundantCatchAllReportsDiagnostic()
    {
        const string code = """
import System.*

val result: Result<string, Exception> = .Ok(value: "ok")

val value = result match {
    .Ok(val text) => text
    .Error((val message)) => message
    _ => ""
}

extension ExceptionExt for Exception {
    func Deconstruct(out message: string) -> unit {
        message = self.Message
    }
}

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan().WithSeverity(DiagnosticSeverity.Warning)]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutineeAndGuard_NotExhaustiveWithoutCatchAll()
    {
        const string code = """
union Input {
    Text(value: string)
    Number(value: int)
    Empty
}

val input: Input = .Text(value: "")

val result = input match {
    .Text(val text) when text.Length > 0 => "Saw \"${text}\""
    .Number(val number) => "Counted ${number}"
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("Text")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithUnionScrutineeAndGuard_NotExhaustiveWithoutCatchAll_ReportsAtMatchKeyword()
    {
        const string code = """
union Input {
    Text(value: string)
    Number(value: int)
    Empty
}

val input: Input = .Text(value: "")

val result = input match {
    .Text(val text) when text.Length > 0 => "Saw \"${text}\""
    .Number(val number) => "Counted ${number}"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_union_guard_missing_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2100"));
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(match.MatchKeyword.GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void MatchExpression_WithUnionScrutineeIncludingNull_DoesNotReportMissingNull()
    {
        const string code = """
val input: string? = null

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
    public void MatchExpression_WithPositionalPattern_ExplicitBindingAndEqualityPattern_BindsCorrectly()
    {
        const string code = """
val existingValue = 2
val pair: (int, int) = (1, 2)

val result = pair match {
    (val a, == existingValue) => a
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "tuple_match_explicit_value_pattern",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(boundMatch.Arms[0].Pattern);

        var firstElement = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstElement.Designator);
        Assert.Equal("a", firstDesignator.Local.Name);

        var second = Assert.IsType<BoundComparisonPattern>(tuplePattern.Elements[1]);
        Assert.Equal(BoundComparisonPatternOperator.Equals, second.Operator);
    }

    [Fact]
    public void MatchExpression_WithPositionalPattern_WithoutBindingKeyword_TreatsIdentifierAsValuePattern()
    {
        const string code = """
val a = 1
val existingValue = 2
val pair: (int, int) = (1, 2)

val result = pair match {
    (a, == existingValue) => 1
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "tuple_match_value_pattern",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(match));
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(boundMatch.Arms[0].Pattern);

        Assert.IsType<BoundConstantPattern>(tuplePattern.Elements[0]);
        var second = Assert.IsType<BoundComparisonPattern>(tuplePattern.Elements[1]);
        Assert.Equal(BoundComparisonPatternOperator.Equals, second.Operator);
    }

    [Fact]
    public void MatchExpression_WithOuterValSequencePattern_BindsImplicitCaptures()
    {
        const string code = """
val input = [1, 2, 3, 4]

val result = input match {
    val [first, second, ...rest] => first + second + rest.Count
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);
    }

    [Fact]
    public void MatchExpression_WithOuterValNominalPattern_BindsImplicitCaptures()
    {
        const string code = """
union Option<T> {
    Some(value: T)
    None
}

val value: Option<(int, int)> = .Some((1, 2))

val result = value match {
    val Some((x, y)) => x + y
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);
    }

    [Fact]
    public void MatchExpression_WithOuterAndInlineBindingKeywords_ReportsConflict()
    {
        const string code = """
val input = [1, 2, 3]

val result = input match {
    val [val first, second, ...rest] => first
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics.Where(d => d.Descriptor != CompilerDiagnostics.PatternDeclarationBindingKeywordConflict));
        Assert.Contains(result.Compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.PatternDeclarationBindingKeywordConflict);
    }

    [Fact]
    public void MatchExpression_WithComparisonPatternOfDifferentType_ReportsDiagnostic()
    {
        const string code = """
val pair: (int, int) = (1, 2)

val result = pair match {
    (1, > 0.5) => 1
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "tuple_match_comparison_type_mismatch",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV1606");
    }

    [Fact]
    public void MatchExpression_WithRangePatternOfDifferentType_ReportsDiagnostic()
    {
        const string code = """
val value: int = 2

val result = value match {
    0..0.5 => 1
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "range_pattern_type_mismatch",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Id == "RAV1606");
    }

    [Fact]
    public void MatchExpression_WithNestedCaseNominalSequenceAndWholeDesignation_BindsAllLocals()
    {
        const string code = """
union Option<T> {
    Some(value: T)
    None
}

class C {
    func Run(value: Option<(string, int)>) -> int {
        return value match {
            val Some((first, >= 18)) whole => first.Length
            _ => 0
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var first = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<SingleVariableDesignationSyntax>().Single(d => d.Identifier.ValueText == "first")));
        var whole = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<SingleVariableDesignationSyntax>().Single(d => d.Identifier.ValueText == "whole")));

        Assert.Equal("string", first.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Some<(string, int)>", whole.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
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
    public void MatchExpression_WithPositionalPatternLengthMismatch_ReportsExhaustivenessAtMatchKeyword()
    {
        const string code = """
val pair: (int, int) = (1, 2)

val result = pair match {
    (int a, int b, int c) => c
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_tuple_length_mismatch_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2100"));
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        Assert.Equal(match.MatchKeyword.GetLocation(), diagnostic.Location);
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
union State {
    On
    Off
}

val value: State = .On

val result = value match {
    bool flag => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("for type 'bool'", "State")]);

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
                new DiagnosticResult(CompilerDiagnostics.MatchExpressionArmUnreachable.Id).WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_CasePatternDeclaredSymbols_UseCaseParameterTypes()
    {
        const string code = """
abstract class Expr

record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr

func Evaluate(expr: Expr) -> int {
    return expr match {
        Add(val left, val right) => 0
        _ => 0
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var designators = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Where(d => d.Identifier.ValueText is "left" or "right")
            .ToArray();

        Assert.Equal(2, designators.Length);

        var exprType = Assert.IsAssignableFrom<INamedTypeSymbol>(result.Compilation.GetTypeByMetadataName("Expr"));
        foreach (var designator in designators)
        {
            var symbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designator));
            Assert.True(SymbolEqualityComparer.Default.Equals(exprType, symbol.Type));
        }
    }

    [Fact]
    public void MatchExpression_WithExclusiveRangePattern_BindsExclusiveUpperBound()
    {
        const string code = """
val value: int = 9

val result = value match {
    2..<10 => 1
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

        var rangePattern = Assert.IsType<BoundRangePattern>(bound.Arms[0].Pattern);
        Assert.True(rangePattern.IsUpperExclusive);
    }
}
