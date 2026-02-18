using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IteratorLowererTests : CompilationTestBase
{
    [Fact]
    public void ShouldRewrite_WhenMethodContainsYieldReturn()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator() -> IEnumerable<int> {
        yield return 1
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        Assert.True(IteratorLowerer.ShouldRewrite(methodSymbol, boundBody));
    }

    [Fact]
    public void Rewrite_AttachesStateMachineMetadata()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerable<int> {
        yield return count
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var rewritten = IteratorLowerer.Rewrite(methodSymbol, boundBody);
        Assert.NotSame(boundBody, rewritten);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);

        Assert.True(methodSymbol.IsIterator);
        Assert.Equal(IteratorMethodKind.Enumerable, methodSymbol.IteratorKind);
        Assert.Equal(methodSymbol.IteratorElementType, stateMachine.ElementType);

        var iterators = compilation.GetSynthesizedIteratorTypes().ToArray();
        Assert.Contains(stateMachine, iterators);

        Assert.Equal("_state", stateMachine.StateField.Name);
        Assert.Equal(compilation.GetSpecialType(SpecialType.System_Int32), stateMachine.StateField.Type);

        Assert.Equal("_current", stateMachine.CurrentField.Name);
        Assert.Equal(stateMachine.ElementType, stateMachine.CurrentField.Type);

        var parameterCapture = Assert.Single(stateMachine.ParameterFields);
        Assert.Equal("_count", parameterCapture.Name);

        var parameterMapEntry = Assert.Single(stateMachine.ParameterFieldMap);
        Assert.Equal("count", parameterMapEntry.Key.Name);
        Assert.Equal(parameterCapture, parameterMapEntry.Value);

        Assert.NotNull(stateMachine.ThisField);
        Assert.Equal(methodSymbol.ContainingType, stateMachine.ThisField!.Type);

        Assert.NotNull(stateMachine.Constructor);
        Assert.NotNull(stateMachine.CurrentProperty.GetMethod);
        Assert.NotNull(stateMachine.NonGenericCurrentProperty.GetMethod);
        Assert.NotNull(stateMachine.DisposeMethod);
        Assert.NotNull(stateMachine.ResetMethod);
        Assert.NotNull(stateMachine.MoveNextMethod);
        Assert.NotNull(stateMachine.GenericGetEnumeratorMethod);
        Assert.NotNull(stateMachine.NonGenericGetEnumeratorMethod);

        var enumerableGeneric = (INamedTypeSymbol)compilation
            .GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T)
            .Construct(stateMachine.ElementType);
        Assert.Contains(enumerableGeneric, stateMachine.Interfaces, SymbolEqualityComparer.Default);

        var enumeratorGeneric = (INamedTypeSymbol)compilation
            .GetSpecialType(SpecialType.System_Collections_Generic_IEnumerator_T)
            .Construct(stateMachine.ElementType);
        Assert.Contains(enumeratorGeneric, stateMachine.Interfaces, SymbolEqualityComparer.Default);
    }

    [Fact]
    public void Rewrite_RewritesMethodBodyToInstantiateStateMachine()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerable<int> {
        yield return count
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var rewritten = IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);
        var statements = rewritten.Statements.ToArray();
        Assert.Equal(5, statements.Length);

        var declaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        var local = declaration.Declarators.Single().Local;
        AssertFieldAssignmentStatement(statements[1], stateMachine.ThisField!);

        var parameterFieldAssignment = AssertFieldAssignmentStatement(statements[2], stateMachine.ParameterFields[0]);
        Assert.Equal(stateMachine.ParameterFields[0], parameterFieldAssignment.Field);
        Assert.IsType<BoundParameterAccess>(parameterFieldAssignment.Right);

        AssertFieldAssignmentStatement(statements[3], stateMachine.StateField, expectedValue: 0);

        var returnStatement = Assert.IsType<BoundReturnStatement>(statements[4]);
        var cast = Assert.IsType<BoundConversionExpression>(returnStatement.Expression);
        var returnLocal = Assert.IsType<BoundLocalAccess>(cast.Expression);
        Assert.Equal(local, returnLocal.Local);
    }

    [Fact]
    public void Rewrite_PopulatesMoveNextBody_ForYieldReturn()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerable<int> {
        yield return count
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);

        var statements = moveNextBody.Statements.ToArray();
        Assert.True(statements.Length >= 6);

        var dispatches = statements.OfType<BoundIfStatement>().Take(2).ToArray();
        Assert.Equal(2, dispatches.Length);

        var stateDispatch0 = dispatches[0];
        var stateDispatch1 = dispatches[1];

        AssertStateDispatch(stateDispatch0, 0, stateMachine.StateField);
        AssertStateDispatch(stateDispatch1, 1, stateMachine.StateField);

        var entryLabel = Assert.IsType<BoundLabeledStatement>(statements.OfType<BoundLabeledStatement>().First());
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);
        var entryStatements = entryBlock.Statements.ToArray();
        var yieldBlock = Assert.Single<BoundStatement>(entryStatements);
        var yieldStatements = Assert.IsType<BoundBlockStatement>(yieldBlock).Statements.ToArray();

        AssertFieldAssignmentStatement(yieldStatements[0], stateMachine.CurrentField);

        AssertFieldAssignmentStatement(yieldStatements[1], stateMachine.StateField, expectedValue: 1);

        AssertFieldAssignmentStatement(
            statements.First(statement => IsFieldAssignment(statement, stateMachine.StateField, expectedValue: -1)),
            stateMachine.StateField,
            expectedValue: -1);
    }

    [Fact]
    public void Rewrite_DoesNotCaptureThis_ForStaticIterator()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    static Iterator(count: int) -> IEnumerable<int> {
        yield return count
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var rewritten = IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);
        Assert.Null(stateMachine.ThisField);

        var statements = rewritten.Statements.ToArray();
        Assert.Equal(4, statements.Length);
        Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        var returnStatement = Assert.IsType<BoundReturnStatement>(statements[3]);
        var cast = Assert.IsType<BoundConversionExpression>(returnStatement.Expression);
        Assert.IsType<BoundLocalAccess>(cast.Expression);
    }

    [Fact]
    public void ShouldNotRewrite_WhenMethodHasNoYield()
    {
        const string source = """
class C {
    Test() {
        val value = 1
        value
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        Assert.False(IteratorLowerer.ShouldRewrite(methodSymbol, boundBody));
    }

    [Fact]
    public void ShouldNotRewrite_ForYieldInsideNestedFunction()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Test() {
        func nested() -> IEnumerable<int> {
            yield return 1
        }

        val enumerator = nested()
        ()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Test");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        Assert.False(IteratorLowerer.ShouldRewrite(methodSymbol, boundBody));
    }

    [Fact]
    public void Rewrite_PopulatesIteratorHelperBodies_ForEnumerableIterator()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerable<int> {
        yield return count
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);

        var currentBody = Assert.IsType<BoundBlockStatement>(stateMachine.CurrentGetterBody);
        var currentReturn = Assert.IsType<BoundReturnStatement>(Assert.Single(currentBody.Statements));
        var currentAccess = Assert.IsType<BoundFieldAccess>(currentReturn.Expression);
        Assert.Equal(stateMachine.CurrentField, currentAccess.Field);

        var nonGenericBody = Assert.IsType<BoundBlockStatement>(stateMachine.NonGenericCurrentGetterBody);
        var nonGenericReturn = Assert.IsType<BoundReturnStatement>(Assert.Single(nonGenericBody.Statements));
        var nonGenericCast = Assert.IsType<BoundConversionExpression>(nonGenericReturn.Expression);
        Assert.Equal(stateMachine.NonGenericCurrentProperty.Type, nonGenericCast.Type);
        var nonGenericSource = Assert.IsType<BoundFieldAccess>(nonGenericCast.Expression);
        Assert.Equal(stateMachine.CurrentField, nonGenericSource.Field);

        var disposeBody = Assert.IsType<BoundBlockStatement>(stateMachine.DisposeBody);
        var disposeStatements = disposeBody.Statements.ToArray();
        Assert.Equal(2, disposeStatements.Length);
        var disposeFieldAssignment = AssertFieldAssignmentStatement(disposeStatements[0], stateMachine.StateField);
        Assert.Equal(stateMachine.StateField, disposeFieldAssignment.Field);
        var disposeLiteral = Assert.IsType<BoundLiteralExpression>(disposeFieldAssignment.Right);
        Assert.Equal(-1, disposeLiteral.Value);
        Assert.IsType<BoundReturnStatement>(disposeStatements[1]);

        var resetBody = Assert.IsType<BoundBlockStatement>(stateMachine.ResetBody);
        var resetStatement = Assert.Single(resetBody.Statements);
        var throwStatement = Assert.IsType<BoundThrowStatement>(resetStatement);
        Assert.IsType<BoundObjectCreationExpression>(throwStatement.Expression);

        var genericBody = Assert.IsType<BoundBlockStatement>(stateMachine.GenericGetEnumeratorBody);
        var genericStatements = genericBody.Statements.ToArray();
        Assert.Equal(2, genericStatements.Length);
        var resetFieldAssignment = AssertFieldAssignmentStatement(genericStatements[0], stateMachine.StateField);
        Assert.Equal(stateMachine.StateField, resetFieldAssignment.Field);
        var resetLiteral = Assert.IsType<BoundLiteralExpression>(resetFieldAssignment.Right);
        Assert.Equal(0, resetLiteral.Value);
        var genericReturn = Assert.IsType<BoundReturnStatement>(genericStatements[1]);
        var genericCast = Assert.IsType<BoundConversionExpression>(genericReturn.Expression);
        Assert.Equal(stateMachine.GenericGetEnumeratorMethod!.ReturnType, genericCast.Type);
        Assert.IsType<BoundSelfExpression>(genericCast.Expression);

        var nonGenericGetEnumeratorBody = Assert.IsType<BoundBlockStatement>(stateMachine.NonGenericGetEnumeratorBody);
        var nonGenericStatements = nonGenericGetEnumeratorBody.Statements.ToArray();
        var nonGenericReturnStmt = Assert.IsType<BoundReturnStatement>(Assert.Single(nonGenericStatements));
        var nonGenericReturnExpr = Assert.IsType<BoundConversionExpression>(nonGenericReturnStmt.Expression);
        var invocation = Assert.IsType<BoundInvocationExpression>(nonGenericReturnExpr.Expression);
        Assert.Equal(stateMachine.GenericGetEnumeratorMethod, invocation.Method);
    }

    [Fact]
    public void Rewrite_WrapsFinallyBlockWithStateGuard()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerable<int> {
        try {
            yield return count
        } finally {
            val disposed = count
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var statements = moveNextBody.Statements.ToArray();

        var guard = FindDescendantStatements(moveNextBody)
            .OfType<BoundIfStatement>()
            .Single(ifStatement => ifStatement.Condition is BoundBinaryExpression { Operator.OperatorKind: BinaryOperatorKind.LessThan });
        var condition = Assert.IsType<BoundBinaryExpression>(guard.Condition);

        var stateAccess = Assert.IsType<BoundFieldAccess>(condition.Left);
        Assert.Equal(stateMachine.StateField, stateAccess.Field);

        var zeroLiteral = Assert.IsType<BoundLiteralExpression>(condition.Right);
        Assert.Equal(0, zeroLiteral.Value);
        Assert.Equal(BinaryOperatorKind.LessThan, condition.Operator.OperatorKind);

        var guardedBlock = Assert.IsType<BoundBlockStatement>(guard.ThenNode);
        var guardedStatements = guardedBlock.Statements.ToArray();
        Assert.Single(guardedStatements);
    }

    [Fact]
    public void Rewrite_DisposeRunsPendingFinalizers()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerable<int> {
        try {
            yield return count
        } finally {
            val disposed = count
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);

        var disposeBody = Assert.IsType<BoundBlockStatement>(stateMachine.DisposeBody);
        var disposeStatements = disposeBody.Statements.ToArray();
        Assert.Equal(3, disposeStatements.Length);

        var conditional = Assert.IsType<BoundIfStatement>(disposeStatements[0]);
        var condition = Assert.IsType<BoundBinaryExpression>(conditional.Condition);

        var stateAccess = Assert.IsType<BoundFieldAccess>(condition.Left);
        Assert.Equal(stateMachine.StateField, stateAccess.Field);

        var resumeLiteral = Assert.IsType<BoundLiteralExpression>(condition.Right);
        Assert.Equal(1, resumeLiteral.Value);

        var thenBlock = Assert.IsType<BoundBlockStatement>(conditional.ThenNode);
        var thenStatements = thenBlock.Statements.ToArray();
        Assert.True(thenStatements.Length >= 3);

        AssertFieldAssignmentStatement(thenStatements[0], stateMachine.StateField, expectedValue: -1);

        var finalizerBlock = Assert.IsType<BoundBlockStatement>(thenStatements[1]);

        var moveNextStatements = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody).Statements.ToArray();
        var entryLabel = Assert.IsType<BoundLabeledStatement>(moveNextStatements.OfType<BoundLabeledStatement>().First());
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(entryBlock.Statements));
        var finallyBlock = Assert.IsType<BoundBlockStatement>(tryStatement.FinallyBlock);
        Assert.Same(finallyBlock, finalizerBlock);

        AssertFieldAssignmentStatement(disposeStatements[1], stateMachine.StateField, expectedValue: -1);

        var finalReturn = Assert.IsType<BoundReturnStatement>(disposeStatements[2]);
        Assert.Null(finalReturn.Expression);
    }

    [Fact]
    public void Rewrite_DoesNotCreateGetEnumeratorBodies_ForEnumeratorIterator()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator(count: int) -> IEnumerator<int> {
        yield return count
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        IteratorLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedIteratorTypeSymbol>(methodSymbol.IteratorStateMachine);
        Assert.Null(stateMachine.GenericGetEnumeratorMethod);
        Assert.Null(stateMachine.NonGenericGetEnumeratorMethod);
        Assert.Null(stateMachine.GenericGetEnumeratorBody);
        Assert.Null(stateMachine.NonGenericGetEnumeratorBody);
    }

    private static BoundFieldAssignmentExpression AssertFieldAssignmentStatement(BoundStatement statement, IFieldSymbol expectedField, int? expectedValue = null)
    {
        var assignment = statement switch
        {
            BoundAssignmentStatement assignmentStatement => Assert.IsType<BoundFieldAssignmentExpression>(assignmentStatement.Expression),
            BoundExpressionStatement expressionStatement => Assert.IsType<BoundFieldAssignmentExpression>(expressionStatement.Expression),
            _ => throw new Xunit.Sdk.XunitException($"Unexpected assignment statement kind: {statement.GetType().Name}"),
        };

        Assert.Equal(expectedField, assignment.Field);

        if (expectedValue is int value)
        {
            var literal = Assert.IsType<BoundLiteralExpression>(assignment.Right);
            Assert.Equal(BoundLiteralExpressionKind.NumericLiteral, literal.Kind);
            Assert.Equal(value, literal.Value);
        }

        return assignment;
    }

    private static bool IsFieldAssignment(BoundStatement statement, IFieldSymbol expectedField, int expectedValue)
    {
        var assignment = statement switch
        {
            BoundAssignmentStatement assignmentStatement => assignmentStatement.Expression as BoundFieldAssignmentExpression,
            BoundExpressionStatement expressionStatement => expressionStatement.Expression as BoundFieldAssignmentExpression,
            _ => null,
        };

        if (assignment is null || !Equals(assignment.Field, expectedField))
        {
            return false;
        }

        return assignment.Right is BoundLiteralExpression literal
               && Equals(literal.Value, expectedValue);
    }

    private static IEnumerable<BoundStatement> FindDescendantStatements(BoundStatement root)
    {
        yield return root;

        switch (root)
        {
            case BoundBlockStatement block:
                foreach (var statement in block.Statements)
                {
                    foreach (var descendant in FindDescendantStatements(statement))
                    {
                        yield return descendant;
                    }
                }
                break;
            case BoundLabeledStatement labeled:
                foreach (var descendant in FindDescendantStatements(labeled.Statement))
                {
                    yield return descendant;
                }
                break;
            case BoundIfStatement ifStatement:
                foreach (var descendant in FindDescendantStatements(ifStatement.ThenNode))
                {
                    yield return descendant;
                }

                if (ifStatement.ElseNode is not null)
                {
                    foreach (var descendant in FindDescendantStatements(ifStatement.ElseNode))
                    {
                        yield return descendant;
                    }
                }
                break;
            case BoundTryStatement tryStatement:
                foreach (var descendant in FindDescendantStatements(tryStatement.TryBlock))
                {
                    yield return descendant;
                }

                foreach (var catchClause in tryStatement.CatchClauses)
                {
                    foreach (var descendant in FindDescendantStatements(catchClause.Block))
                    {
                        yield return descendant;
                    }
                }

                if (tryStatement.FinallyBlock is not null)
                {
                    foreach (var descendant in FindDescendantStatements(tryStatement.FinallyBlock))
                    {
                        yield return descendant;
                    }
                }
                break;
        }
    }

    private static void AssertFalseLiteral(BoundExpression? expression)
    {
        var literal = Assert.IsType<BoundLiteralExpression>(expression);
        Assert.Equal(BoundLiteralExpressionKind.FalseLiteral, literal.Kind);
    }

    private static void AssertStateDispatch(BoundIfStatement dispatch, int expectedState, IFieldSymbol stateField)
    {
        var condition = Assert.IsType<BoundBinaryExpression>(dispatch.Condition);
        var left = Assert.IsType<BoundFieldAccess>(condition.Left);
        Assert.Equal(stateField, left.Field);

        var right = Assert.IsType<BoundLiteralExpression>(condition.Right);
        Assert.Equal(expectedState, right.Value);

        var thenBlock = Assert.IsType<BoundBlockStatement>(dispatch.ThenNode);
        var thenStatements = thenBlock.Statements.ToArray();
        Assert.Equal(2, thenStatements.Length);

        AssertFieldAssignmentStatement(thenStatements[0], stateField, expectedValue: -1);

        Assert.IsType<BoundGotoStatement>(thenStatements[1]);
    }
}
