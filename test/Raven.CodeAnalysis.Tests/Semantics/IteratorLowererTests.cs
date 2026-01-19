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
        var thisAssignment = Assert.IsType<BoundExpressionStatement>(statements[1]);
        AssertFieldAssignment(thisAssignment.Expression, stateMachine.ThisField!);

        var parameterAssignment = Assert.IsType<BoundExpressionStatement>(statements[2]);
        var parameterFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(parameterAssignment.Expression);
        Assert.Equal(stateMachine.ParameterFields[0], parameterFieldAssignment.Field);
        Assert.IsType<BoundParameterAccess>(parameterFieldAssignment.Right);

        var stateAssignment = Assert.IsType<BoundExpressionStatement>(statements[3]);
        AssertFieldAssignment(stateAssignment.Expression, stateMachine.StateField, expectedValue: 0);

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
        Assert.Equal(6, statements.Length);

        var stateDispatch0 = Assert.IsType<BoundIfStatement>(statements[0]);
        var stateDispatch1 = Assert.IsType<BoundIfStatement>(statements[1]);

        AssertStateDispatch(stateDispatch0, 0, stateMachine.StateField);
        AssertStateDispatch(stateDispatch1, 1, stateMachine.StateField);

        var defaultReturn = Assert.IsType<BoundReturnStatement>(statements[2]);
        AssertFalseLiteral(defaultReturn.Expression);

        var entryLabel = Assert.IsType<BoundLabeledStatement>(statements[3]);
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);
        var entryStatements = entryBlock.Statements.ToArray();
        var yieldBlock = Assert.Single<BoundStatement>(entryStatements);
        var yieldStatements = Assert.IsType<BoundBlockStatement>(yieldBlock).Statements.ToArray();

        var assignCurrent = Assert.IsType<BoundExpressionStatement>(yieldStatements[0]);
        AssertFieldAssignment(assignCurrent.Expression, stateMachine.CurrentField);

        var assignState = Assert.IsType<BoundExpressionStatement>(yieldStatements[1]);
        AssertFieldAssignment(assignState.Expression, stateMachine.StateField, expectedValue: 1);

        var returnTrue = Assert.IsType<BoundReturnStatement>(yieldStatements[2]);
        Assert.True(((BoundLiteralExpression)returnTrue.Expression!).Kind == BoundLiteralExpressionKind.TrueLiteral);

        Assert.IsType<BoundLabeledStatement>(yieldStatements[3]);

        var finalStateAssignment = Assert.IsType<BoundExpressionStatement>(statements[4]);
        AssertFieldAssignment(finalStateAssignment.Expression, stateMachine.StateField, expectedValue: -1);

        var finalReturn = Assert.IsType<BoundReturnStatement>(statements[5]);
        AssertFalseLiteral(finalReturn.Expression);
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
        let value = 1
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

        let enumerator = nested()
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
        var disposeAssignment = Assert.IsType<BoundExpressionStatement>(disposeStatements[0]);
        var disposeFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(disposeAssignment.Expression);
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
        var resetAssignment = Assert.IsType<BoundExpressionStatement>(genericStatements[0]);
        var resetFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(resetAssignment.Expression);
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
            let disposed = count
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

        var entryLabel = Assert.IsType<BoundLabeledStatement>(statements[3]);
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(entryBlock.Statements));

        var finallyBlock = Assert.IsType<BoundBlockStatement>(tryStatement.FinallyBlock);
        var guard = Assert.IsType<BoundIfStatement>(Assert.Single(finallyBlock.Statements));
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
            let disposed = count
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

        var assignPending = Assert.IsType<BoundExpressionStatement>(thenStatements[0]);
        AssertFieldAssignment(assignPending.Expression, stateMachine.StateField, expectedValue: -1);

        var finalizerBlock = Assert.IsType<BoundBlockStatement>(thenStatements[1]);

        var moveNextStatements = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody).Statements.ToArray();
        var entryLabel = Assert.IsType<BoundLabeledStatement>(moveNextStatements.OfType<BoundLabeledStatement>().First());
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(entryBlock.Statements));
        var finallyBlock = Assert.IsType<BoundBlockStatement>(tryStatement.FinallyBlock);
        Assert.Same(finallyBlock, finalizerBlock);

        var finalAssignment = Assert.IsType<BoundExpressionStatement>(disposeStatements[1]);
        AssertFieldAssignment(finalAssignment.Expression, stateMachine.StateField, expectedValue: -1);

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

    private static void AssertFieldAssignment(BoundExpression expression, IFieldSymbol expectedField, int? expectedValue = null)
    {
        var assignment = Assert.IsType<BoundFieldAssignmentExpression>(expression);
        Assert.Equal(expectedField, assignment.Field);

        if (expectedValue is int value)
        {
            var literal = Assert.IsType<BoundLiteralExpression>(assignment.Right);
            Assert.Equal(BoundLiteralExpressionKind.NumericLiteral, literal.Kind);
            Assert.Equal(value, literal.Value);
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

        var stateReset = Assert.IsType<BoundExpressionStatement>(thenStatements[0]);
        AssertFieldAssignment(stateReset.Expression, stateField, expectedValue: -1);

        Assert.IsType<BoundGotoStatement>(thenStatements[1]);
    }
}
