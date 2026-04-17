using System;
using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PatternAssignmentSemanticTests : DiagnosticTestBase
{
    private const string PositionalPatternAssignmentSemanticSkipReason =
        "Positional pattern assignment semantic coverage is currently unstable and tracked separately.";

    [Fact]
    public void LetPositionalPatternAssignment_BindsLocals()
    {
        const string source = """
val (first, second, _) = (1, 2, 3)
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(3, tuplePattern.Elements.Length);

        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local.Type, intType));
        Assert.False(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(secondDesignator.Local.Type, intType));
        Assert.False(secondDesignator.Local.IsMutable);

        Assert.IsType<BoundDiscardPattern>(tuplePattern.Elements[2]);
    }

    [Fact]
    public void LetPositionalPatternAssignment_WithNamedElements_BindsDeconstructArgumentsByName()
    {
        const string source = """
record class Person(Name: string, Age: int, Items: string[])

val person = Person("Ada", 42, ["tea"])
val (Items: items, Name: name, Age: age) = person
name.Length + age + items.Length
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Last();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var deconstructPattern = Assert.IsType<BoundDeconstructPattern>(patternAssignment.Pattern);

        Assert.Equal(3, deconstructPattern.Arguments.Length);

        var namePattern = Assert.IsType<BoundDeclarationPattern>(deconstructPattern.Arguments[0]);
        var nameDesignator = Assert.IsType<BoundSingleVariableDesignator>(namePattern.Designator);
        Assert.Equal("name", nameDesignator.Local.Name);
        Assert.Equal(SpecialType.System_String, nameDesignator.Local.Type.SpecialType);

        var agePattern = Assert.IsType<BoundDeclarationPattern>(deconstructPattern.Arguments[1]);
        var ageDesignator = Assert.IsType<BoundSingleVariableDesignator>(agePattern.Designator);
        Assert.Equal("age", ageDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, ageDesignator.Local.Type.SpecialType);

        var itemsPattern = Assert.IsType<BoundDeclarationPattern>(deconstructPattern.Arguments[2]);
        var itemsDesignator = Assert.IsType<BoundSingleVariableDesignator>(itemsPattern.Designator);
        Assert.Equal("items", itemsDesignator.Local.Name);
        Assert.True(itemsDesignator.Local.Type is IArrayTypeSymbol { ElementType.SpecialType: SpecialType.System_String });
    }

    [Fact]
    public void LetPositionalPatternAssignment_WithUnknownNamedElement_ReportsDiagnostic()
    {
        const string source = """
record class Person(Name: string, Age: int)

val person = Person("Ada", 42)
val (Height: height, Name: name) = person
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PropertyPatternMemberNotFound);
    }

    [Fact]
    public void LetCollectionPatternAssignment_BindsLocals()
    {
        const string source = """
val values: int[] = [1, 2, 3]
val [first, second, _] = values
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Last();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(3, collectionPattern.Elements.Length);
        Assert.True(collectionPattern.Type is IArrayTypeSymbol);

        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local.Type, intType));
        Assert.False(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(secondDesignator.Local.Type, intType));
        Assert.False(secondDesignator.Local.IsMutable);

        Assert.IsType<BoundDiscardPattern>(collectionPattern.Elements[2]);
    }

    [Fact]
    public void LetCollectionPatternAssignment_WithList_BindsLocals()
    {
        const string source = """
import System.Collections.Generic.*

val values: List<int> = [1, 2, 3]
val [first, second, _] = values
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Last();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(3, collectionPattern.Elements.Length);
        Assert.True(collectionPattern.Type is INamedTypeSymbol named && named.Name == "List");

        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local.Type, intType));
        Assert.False(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(secondDesignator.Local.Type, intType));
        Assert.False(secondDesignator.Local.IsMutable);

        Assert.IsType<BoundDiscardPattern>(collectionPattern.Elements[2]);
    }

    [Fact]
    public void LetDictionaryPatternAssignment_BindsLocals()
    {
        const string source = """
import System.Collections.Immutable.*

val values: ImmutableDictionary<string, int> = ["a": 1, "b": 2]
val ["a": first, "b": second] = values
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Last();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var dictionaryPattern = Assert.IsType<BoundDictionaryPattern>(patternAssignment.Pattern);

        Assert.Equal(2, dictionaryPattern.Entries.Length);
        Assert.Equal(SpecialType.System_String, dictionaryPattern.KeyType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryPattern.ValueType.SpecialType);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(dictionaryPattern.Entries[0].Pattern);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, firstDesignator.Local.Type.SpecialType);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(dictionaryPattern.Entries[1].Pattern);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, secondDesignator.Local.Type.SpecialType);
    }

    [Fact]
    public void SequencePatternAssignment_OnNonSequenceType_ReportsSequenceDiagnostic()
    {
        const string source = """
val value = 42
val [first] = value
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.SequenceDeconstructionRequiresSequenceType);
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PositionalDeconstructionRequiresDeconstructableType);
    }

    [Fact]
    public void DictionaryPatternAssignment_OnNonDictionaryType_ReportsDictionaryDiagnostic()
    {
        const string source = """
val value = 42
val ["a": first] = value
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.DictionaryDeconstructionRequiresDictionaryType);
    }

    [Fact]
    public void DictionaryPatternAssignment_WithDuplicateConstantKeys_ReportsDuplicateKey()
    {
        const string source = """
import System.Collections.Immutable.*

val values: ImmutableDictionary<string, int> = ["a": 1]
val ["a": first, "a": second] = values
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();
        var diagnostics = run.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.DuplicateDictionaryKey);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithVal_BindsImmutableLocals()
    {
        const string source = """
val values: int[] = [1, 2, 3]
val [first, second, _] = values
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Last();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.False(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.False(secondDesignator.Local.IsMutable);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithVar_BindsMutableLocals()
    {
        const string source = """
val values: int[] = [1, 2, 3]
var [first, second, _] = values
first = 42
second = 21
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .First();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.True(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.True(secondDesignator.Local.IsMutable);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithOuterAndInlineBinding_ReportsDiagnostic()
    {
        const string source = """
val values: int[] = [1, 2, 3]
val [val first, val second, _] = values
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PatternDeclarationBindingKeywordConflict.Id)
                    .WithAnySpan()
                    .WithArguments("val", "val")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PositionalPatternDeclarationShorthand_WithOuterAndInlineBinding_ReportsDiagnostic()
    {
        const string source = """
val obj = (1, "x")
val (val id, val name) = obj
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PatternDeclarationBindingKeywordConflict.Id)
                    .WithAnySpan()
                    .WithArguments("val", "val")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithMiddleRest_BindsArraySlice()
    {
        const string source = """
val values: int[] = [1, 2, 3, 4]
val [first, ..middle, last] = values
first + middle[0] + last
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(1, collectionPattern.RestIndex);

        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("middle", restDesignator.Local.Name);
        Assert.True(restDesignator.Local.Type is IArrayTypeSymbol { ElementType.SpecialType: SpecialType.System_Int32 });
        Assert.False(restDesignator.Local.IsMutable);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithMiddleRest_OnList_PreservesListSlice()
    {
        const string source = """
import System.Collections.Generic.*

val values: List<int> = [1, 2, 3, 4]
val [first, ..middle, last] = values
first + middle[0] + last
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(1, collectionPattern.RestIndex);

        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("middle", restDesignator.Local.Name);
        Assert.Equal("System.Collections.Generic.List`1", ((INamedTypeSymbol)restDesignator.Local.Type).OriginalDefinition.ToFullyQualifiedMetadataName());
        Assert.False(restDesignator.Local.IsMutable);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithTrailingTripleDot_DoesNotCapture()
    {
        const string source = """
val values: int[] = [1, 2, 3, 4]
val [first, ...] = values
first
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(1, collectionPattern.RestIndex);
        Assert.IsType<BoundDiscardPattern>(collectionPattern.Elements[1]);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithMiddleTripleDot_DoesNotCapture()
    {
        const string source = """
val values: int[] = [1, 2, 3, 4, 5]
val [first, ..., last] = values
last
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(1, collectionPattern.RestIndex);
        Assert.IsType<BoundDiscardPattern>(collectionPattern.Elements[1]);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithFixedSegment_OnImmutableList_PreservesSliceLocalType()
    {
        const string source = """
import System.Collections.Immutable.*

val values: ImmutableList<int> = [1, 2, 3]
val [..2 start, tail] = values
tail
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(BoundPositionalPattern.SequenceElementKind.FixedSegment, collectionPattern.ElementKinds[0]);
        Assert.Equal(2, collectionPattern.ElementWidths[0]);

        var startPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var startDesignator = Assert.IsType<BoundSingleVariableDesignator>(startPattern.Designator);
        Assert.Equal("start", startDesignator.Local.Name);
        Assert.Equal("System.Collections.Immutable.ImmutableList`1", ((INamedTypeSymbol)startDesignator.Local.Type).OriginalDefinition.ToFullyQualifiedMetadataName());

        var tailPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var tailDesignator = Assert.IsType<BoundSingleVariableDesignator>(tailPattern.Designator);
        Assert.Equal("tail", tailDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, tailDesignator.Local.Type.SpecialType);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithMiddleRest_OnImmutableArray_PreservesImmutableArraySlice()
    {
        const string source = """
import System.Collections.Immutable.*

val values: ImmutableArray<int> = [1, 2, 3, 4]
val [first, ..middle, last] = values
first + middle[0] + last
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(1, collectionPattern.RestIndex);

        var restPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var restDesignator = Assert.IsType<BoundSingleVariableDesignator>(restPattern.Designator);
        Assert.Equal("middle", restDesignator.Local.Name);
        Assert.Equal("System.Collections.Immutable.ImmutableArray`1", ((INamedTypeSymbol)restDesignator.Local.Type).OriginalDefinition.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_WithFixedArrayRest_BindsFixedSizeArraySlice()
    {
        const string source = """
val values: int[4] = [1, 2, 3, 4]
val [first, second, ...rest] = values
rest
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

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
    public void CollectionPatternDeclarationShorthand_WithFixedArrayFixedSegment_BindsFixedSizeArraySlice()
    {
        const string source = """
val values: int[3] = [1, 2, 3]
val [..2 start, tail] = values
start
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        var startPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var startDesignator = Assert.IsType<BoundSingleVariableDesignator>(startPattern.Designator);
        Assert.Equal("start", startDesignator.Local.Name);
        Assert.True(startDesignator.Local.Type is IArrayTypeSymbol
        {
            ElementType.SpecialType: SpecialType.System_Int32,
            FixedLength: 2
        });
    }

    [Fact]
    public void StringPatternDeclarationShorthand_WithFixedSegment_BindsCharAndStringLocals()
    {
        const string source = """
val text = "rune"
val [first, ..2 middle, last] = text
middle
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var collectionPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(BoundPositionalPattern.SequenceElementKind.Single, collectionPattern.ElementKinds[0]);
        Assert.Equal(BoundPositionalPattern.SequenceElementKind.FixedSegment, collectionPattern.ElementKinds[1]);
        Assert.Equal(BoundPositionalPattern.SequenceElementKind.Single, collectionPattern.ElementKinds[2]);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[0]);
        var middlePattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[1]);
        var lastPattern = Assert.IsType<BoundDeclarationPattern>(collectionPattern.Elements[2]);

        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        var middleDesignator = Assert.IsType<BoundSingleVariableDesignator>(middlePattern.Designator);
        var lastDesignator = Assert.IsType<BoundSingleVariableDesignator>(lastPattern.Designator);

        Assert.Equal(SpecialType.System_Char, firstDesignator.Local.Type.SpecialType);
        Assert.Equal(SpecialType.System_String, middleDesignator.Local.Type.SpecialType);
        Assert.Equal(SpecialType.System_Char, lastDesignator.Local.Type.SpecialType);
    }

    [Fact]
    public void CollectionPatternDeclarationShorthand_OnEnumerable_ReportsDiagnostic()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

val values: IEnumerable<int> = [1, 2, 3, 4].Where(v => v > 0)
val [first, second, _] = values
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.SequenceDeconstructionRequiresSequenceType.Id)
                    .WithAnySpan()
                    .WithArguments("IEnumerable<int>")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void DiscardAssignmentStatement_BindsDiscardPattern()
    {
        const string source = "_ = 1";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        Assert.True(assignment.IsDiscard);
        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        Assert.IsType<BoundDiscardPattern>(patternAssignment.Pattern);

        Assert.NotNull(patternAssignment.Type);
    }

    [Fact]
    public void NestedPatternAssignment_WithExistingMutableLocals_ReusesBindings()
    {
        const string source = """
var nested = ((1, 2), [3, 4, 5])
var first = 0
var second = 0
var head = 0
var tail = [0]
((first, second), [head, ..tail]) = nested
first + second + head + tail[0]
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var declaredLocals = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Where(static declarator => declarator.Identifier.ValueText is "first" or "second" or "head" or "tail")
            .ToDictionary(
                declarator => declarator.Identifier.ValueText,
                declarator => Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator)),
                StringComparer.Ordinal);

        var assignment = root
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single(statement => statement.Left is PositionalPatternSyntax);

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var outerPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        var tuplePattern = Assert.IsType<BoundPositionalPattern>(outerPattern.Elements[0]);
        var sequencePattern = Assert.IsType<BoundPositionalPattern>(outerPattern.Elements[1]);
        Assert.Equal(1, sequencePattern.RestIndex);

        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]).Designator);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]).Designator);
        var headDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(sequencePattern.Elements[0]).Designator);
        var tailDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(sequencePattern.Elements[1]).Designator);

        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local, declaredLocals["first"]));
        Assert.True(SymbolEqualityComparer.Default.Equals(secondDesignator.Local, declaredLocals["second"]));
        Assert.True(SymbolEqualityComparer.Default.Equals(headDesignator.Local, declaredLocals["head"]));
        Assert.True(SymbolEqualityComparer.Default.Equals(tailDesignator.Local, declaredLocals["tail"]));
    }

    [Fact]
    public void NestedPatternDeclarationAssignment_WithNestedSequencePatterns_BindsLocals()
    {
        const string source = """
val input = ([1, 2, 3, 4], "rune")
val ([head, ..middle, last], [first, ..2 chunk, final]) = input
head + middle[0] + last + first.ToString().Length + chunk.Length + final.ToString().Length
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<PatternDeclarationAssignmentStatementSyntax>()
            .Single();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var outerPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);
        var arrayPattern = Assert.IsType<BoundPositionalPattern>(outerPattern.Elements[0]);
        var stringPattern = Assert.IsType<BoundPositionalPattern>(outerPattern.Elements[1]);

        var headPattern = Assert.IsType<BoundDeclarationPattern>(arrayPattern.Elements[0]);
        var middlePattern = Assert.IsType<BoundDeclarationPattern>(arrayPattern.Elements[1]);
        var lastPattern = Assert.IsType<BoundDeclarationPattern>(arrayPattern.Elements[2]);
        var firstPattern = Assert.IsType<BoundDeclarationPattern>(stringPattern.Elements[0]);
        var chunkPattern = Assert.IsType<BoundDeclarationPattern>(stringPattern.Elements[1]);
        var finalPattern = Assert.IsType<BoundDeclarationPattern>(stringPattern.Elements[2]);

        var headDesignator = Assert.IsType<BoundSingleVariableDesignator>(headPattern.Designator);
        var middleDesignator = Assert.IsType<BoundSingleVariableDesignator>(middlePattern.Designator);
        var lastDesignator = Assert.IsType<BoundSingleVariableDesignator>(lastPattern.Designator);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        var chunkDesignator = Assert.IsType<BoundSingleVariableDesignator>(chunkPattern.Designator);
        var finalDesignator = Assert.IsType<BoundSingleVariableDesignator>(finalPattern.Designator);

        Assert.Equal("head", headDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, headDesignator.Local.Type.SpecialType);
        Assert.Equal("middle", middleDesignator.Local.Name);
        Assert.Equal("System.Collections.Immutable.ImmutableList`1", ((INamedTypeSymbol)middleDesignator.Local.Type).OriginalDefinition.ToFullyQualifiedMetadataName());
        Assert.Equal("last", lastDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Int32, lastDesignator.Local.Type.SpecialType);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Char, firstDesignator.Local.Type.SpecialType);
        Assert.Equal("chunk", chunkDesignator.Local.Name);
        Assert.Equal(SpecialType.System_String, chunkDesignator.Local.Type.SpecialType);
        Assert.Equal("final", finalDesignator.Local.Name);
        Assert.Equal(SpecialType.System_Char, finalDesignator.Local.Type.SpecialType);
    }

    [Fact]
    public void NestedPatternAssignment_WithNestedSequencePatterns_ReusesExistingBindings()
    {
        const string source = """
val input = ([1, 2, 3, 4], "rune")
var head = 0
var middle = [0]
var last = 0
var first = 'a'
var chunk = ""
var final = 'a'
([head, ..middle, last], [first, ..2 chunk, final]) = input
head + middle[0] + last + first.ToString().Length + chunk.Length + final.ToString().Length
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var headLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "head")));
        var middleLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "middle")));
        var lastLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "last")));
        var firstLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "first")));
        var chunkLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "chunk")));
        var finalLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "final")));

        var assignment = root
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single(statement => statement.Left is PositionalPatternSyntax);

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var outerPattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);
        var arrayPattern = Assert.IsType<BoundPositionalPattern>(outerPattern.Elements[0]);
        var stringPattern = Assert.IsType<BoundPositionalPattern>(outerPattern.Elements[1]);

        var headDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(arrayPattern.Elements[0]).Designator);
        var middleDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(arrayPattern.Elements[1]).Designator);
        var lastDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(arrayPattern.Elements[2]).Designator);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(stringPattern.Elements[0]).Designator);
        var chunkDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(stringPattern.Elements[1]).Designator);
        var finalDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(stringPattern.Elements[2]).Designator);

        Assert.True(SymbolEqualityComparer.Default.Equals(headDesignator.Local, headLocal));
        Assert.True(SymbolEqualityComparer.Default.Equals(middleDesignator.Local, middleLocal));
        Assert.True(SymbolEqualityComparer.Default.Equals(lastDesignator.Local, lastLocal));
        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local, firstLocal));
        Assert.True(SymbolEqualityComparer.Default.Equals(chunkDesignator.Local, chunkLocal));
        Assert.True(SymbolEqualityComparer.Default.Equals(finalDesignator.Local, finalLocal));
    }

    [Fact]
    public void PositionalPatternAssignment_WithRecordDeconstruct_ReusesExistingLocals()
    {
        const string source = """
record class Pair(Left: int, Right: int)

var left = 0
var right = 0
(left, right) = Pair(1, 2)
left + right
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var leftLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "left")));
        var rightLocal = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetDeclaredSymbol(root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.ValueText == "right")));

        var assignment = root
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single(statement => statement.Left is PositionalPatternSyntax);

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var deconstructPattern = Assert.IsType<BoundDeconstructPattern>(patternAssignment.Pattern);

        Assert.Equal("Deconstruct", deconstructPattern.DeconstructMethod.Name);
        Assert.Equal(2, deconstructPattern.Arguments.Length);

        var leftDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(deconstructPattern.Arguments[0]).Designator);
        var rightDesignator = Assert.IsType<BoundSingleVariableDesignator>(
            Assert.IsType<BoundDeclarationPattern>(deconstructPattern.Arguments[1]).Designator);

        Assert.True(SymbolEqualityComparer.Default.Equals(leftDesignator.Local, leftLocal));
        Assert.True(SymbolEqualityComparer.Default.Equals(rightDesignator.Local, rightLocal));
    }

    [Fact]
    public void NestedPatternAssignment_WithMissingNestedIdentifier_ReportsPreciseDiagnostic()
    {
        const string source = """
val nested = ((1, 2), [3, 4, 5])
var first = 0
var second = 0
var head = 0
((first, second), [head, ..missing]) = nested
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                    .WithAnySpan()
                    .WithArguments("missing")
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_WithExistingLocals_ReusesBindings()
    {
        const string source = """
var first = 0
var second = 0
(first, second) = (1, 2)
first + second
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id)
                    .WithSpan(3, 1, 3, 16)
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_ToImmutableLocal_ReportsDiagnostic()
    {
        const string source = """
val first = 0
var second = 0
(first, second) = (1, 2)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id)
                    .WithSpan(3, 1, 3, 16)
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_WithExistingLocals_TypeMismatch_ReportsDiagnostic()
    {
        const string source = """
var first = 0
var second = 0
(first, second) = (1.5, 2.5)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id)
                    .WithSpan(3, 1, 3, 16),
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_AfterMultilineInitializer_ParsesCorrectly()
    {
        const string source = """
var tuple =
    (1, 2)
var first = 0
var second = 0
(first, second) = tuple
first + second
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id)
                    .WithSpan(5, 1, 5, 16)
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void VarPositionalPatternAssignment_WithTypedDesignation_UsesDeclaredTypes()
    {
        const string source = """
var (first: double, second, _) = (1, 2, 3)
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .First();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var tuplePattern = Assert.IsType<BoundPositionalPattern>(patternAssignment.Pattern);

        Assert.Equal(3, tuplePattern.Elements.Length);

        var doubleType = result.Compilation.GetSpecialType(SpecialType.System_Double);
        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local.Type, doubleType));
        Assert.True(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(secondDesignator.Local.Type, intType));
        Assert.True(secondDesignator.Local.IsMutable);

        Assert.IsType<BoundDiscardPattern>(tuplePattern.Elements[2]);
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void MixedPositionalPatternAssignment_BindsNestedPatterns()
    {
        const string source = """
(val first, var second: double, _) = (1, 2, 3)
second = 4.5
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.ConsecutiveStatementsMustBeSeparatedBySemicolon.Id)
                    .WithAnySpan(),
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                    .WithArguments("second")
                    .WithAnySpan()
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_NonTupleRight_ReportsDiagnostic()
    {
        const string source = """
val (first, second, _) = 1
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PositionalDeconstructionRequiresDeconstructableType.Id)
                    .WithSpan(1, 5, 1, 23)
                    .WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_ArityMismatch_ReportsDiagnostic()
    {
        const string source = """
val (first, second, third) = (1, 2)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PositionalDeconstructionElementCountMismatch.Id)
                    .WithSpan(1, 5, 1, 27)
                    .WithArguments(3, 2)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void CollectionPatternAssignment_FixedArrayLengthMismatch_ReportsDiagnostic()
    {
        const string source = """
val values: int[3] = [1, 2, 3]
val [first, second] = values
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PositionalDeconstructionElementCountMismatch.Id)
                    .WithAnySpan()
                    .WithArguments(2, 3)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void FixedArrayCollectionExpression_WithFixedSpreadLengthMismatch_ReportsDiagnostic()
    {
        const string source = """
val values: int[2] = [1, 2]
val result: int[4] = [...values, 3]
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PositionalDeconstructionElementCountMismatch.Id)
                    .WithAnySpan()
                    .WithArguments(3, 4)
            ]);

        verifier.Verify();
    }

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_UsesExtensionDeconstruct()
    {
        const string source = """
class Widget {}

extension WidgetExtensions for Widget {
    func Deconstruct(out var first: int, out var second: string) -> unit {
        first = 1
        second = "ok"
    }
}

val widget = Widget()
val (first, second) = widget
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Last();

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var deconstructPattern = Assert.IsType<BoundDeconstructPattern>(patternAssignment.Pattern);

        Assert.True(deconstructPattern.DeconstructMethod.IsExtensionMethod);
        Assert.Equal("Deconstruct", deconstructPattern.DeconstructMethod.Name);
        Assert.Equal(2, deconstructPattern.Arguments.Length);
    }
}
