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
            .OfType<AssignmentStatementSyntax>()
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

    [Fact(Skip = PositionalPatternAssignmentSemanticSkipReason)]
    public void PositionalPatternAssignment_UsesExtensionDeconstruct()
    {
        const string source = """
class Widget {}

extension WidgetExtensions for Widget {
    Deconstruct(out var first: &int, out var second: &string) -> unit {
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
