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
    [Fact]
    public void LetTuplePatternAssignment_BindsLocals()
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
        var tuplePattern = Assert.IsType<BoundTuplePattern>(patternAssignment.Pattern);

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

        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, patternAssignment.Type));
    }

    [Fact]
    public void TuplePatternAssignment_WithExistingLocals_ReusesBindings()
    {
        const string source = """
var first = 0
var second = 0
(first, second) = (1, 2)
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignmentStatements = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .ToArray();
        var assignment = assignmentStatements.Single();

        var declarators = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .ToArray();

        var firstLocal = (ILocalSymbol)model.GetDeclaredSymbol(declarators[0])!;
        var secondLocal = (ILocalSymbol)model.GetDeclaredSymbol(declarators[1])!;

        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(assignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var tuplePattern = Assert.IsType<BoundTuplePattern>(patternAssignment.Pattern);

        Assert.Equal(2, tuplePattern.Elements.Length);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.True(SymbolEqualityComparer.Default.Equals(firstLocal, firstDesignator.Local));

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.True(SymbolEqualityComparer.Default.Equals(secondLocal, secondDesignator.Local));
    }

    [Fact]
    public void TuplePatternAssignment_ToImmutableLocal_ReportsDiagnostic()
    {
        const string source = """
val first = 0
var second = 0
(first, second) = (1, 2)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.ThisValueIsNotMutable.Id)
                    .WithSpan(3, 2, 3, 7)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TuplePatternAssignment_WithExistingLocals_TypeMismatch_ReportsDiagnostic()
    {
        const string source = """
var first = 0
var second = 0
(first, second) = (1.5, 2.5)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id)
                    .WithSpan(3, 2, 3, 7)
                    .WithArguments("'double'", "'int'"),
                new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id)
                    .WithSpan(3, 9, 3, 15)
                    .WithArguments("'double'", "'int'"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TuplePatternAssignment_AfterMultilineInitializer_ParsesCorrectly()
    {
        const string source = """
var tuple =
    (1, 2)
var first = 0
var second = 0
(first, second) = tuple
first + second
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);

        var assignmentStatements = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .ToArray();

        Assert.Equal(2, assignmentStatements.Length);

        var tupleAssignment = assignmentStatements[1];
        var boundAssignment = Assert.IsType<BoundAssignmentStatement>(model.GetBoundNode(tupleAssignment));
        var patternAssignment = Assert.IsType<BoundPatternAssignmentExpression>(boundAssignment.Expression);
        var tuplePattern = Assert.IsType<BoundTuplePattern>(patternAssignment.Pattern);

        Assert.Equal(2, tuplePattern.Elements.Length);
    }

    [Fact]
    public void VarTuplePatternAssignment_WithTypedDesignation_UsesDeclaredTypes()
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
        var tuplePattern = Assert.IsType<BoundTuplePattern>(patternAssignment.Pattern);

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

    [Fact]
    public void MixedTuplePatternAssignment_BindsNestedPatterns()
    {
        const string source = """
(val first, var second: double, _) = (1, 2, 3)
second = 4.5
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
        var tuplePattern = Assert.IsType<BoundTuplePattern>(patternAssignment.Pattern);

        Assert.Equal(3, tuplePattern.Elements.Length);

        var intType = result.Compilation.GetSpecialType(SpecialType.System_Int32);
        var doubleType = result.Compilation.GetSpecialType(SpecialType.System_Double);

        var firstPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[0]);
        var firstDesignator = Assert.IsType<BoundSingleVariableDesignator>(firstPattern.Designator);
        Assert.Equal("first", firstDesignator.Local.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(firstDesignator.Local.Type, intType));
        Assert.False(firstDesignator.Local.IsMutable);

        var secondPattern = Assert.IsType<BoundDeclarationPattern>(tuplePattern.Elements[1]);
        var secondDesignator = Assert.IsType<BoundSingleVariableDesignator>(secondPattern.Designator);
        Assert.Equal("second", secondDesignator.Local.Name);
        Assert.True(secondDesignator.Local.IsMutable);
        Assert.True(SymbolEqualityComparer.Default.Equals(secondDesignator.Local.Type, doubleType));

        Assert.IsType<BoundDiscardPattern>(tuplePattern.Elements[2]);
    }

    [Fact]
    public void TuplePatternAssignment_NonTupleRight_ReportsDiagnostic()
    {
        const string source = """
val (first, second, _) = 1
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TupleDeconstructionRequiresTupleType.Id)
                    .WithSpan(1, 5, 1, 23)
                    .WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TuplePatternAssignment_ArityMismatch_ReportsDiagnostic()
    {
        const string source = """
val (first, second, third) = (1, 2)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TupleDeconstructionElementCountMismatch.Id)
                    .WithSpan(1, 5, 1, 27)
                    .WithArguments(3, 2)
            ]);

        verifier.Verify();
    }
}
