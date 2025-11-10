using System;
using System.Collections.Generic;
using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class AsyncLowererTests : CompilationTestBase
{
    [Fact]
    public void Analyze_AsyncMethodWithAwait_RequiresStateMachine()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        await Task.CompletedTask
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

        var analysis = AsyncLowerer.Analyze(methodSymbol, boundBody);

        Assert.True(analysis.ContainsAwait);
        Assert.True(analysis.RequiresStateMachine);
        Assert.True(methodSymbol.ContainsAwait);
    }

    [Fact]
    public void Analyze_AsyncMethodWithoutAwait_DoesNotRequireStateMachine()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        return Task.CompletedTask
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

        var analysis = AsyncLowerer.Analyze(methodSymbol, boundBody);

        Assert.False(analysis.ContainsAwait);
        Assert.False(analysis.RequiresStateMachine);
        Assert.False(methodSymbol.ContainsAwait);
    }

    [Fact]
    public void Analyze_TopLevelAsyncFunction_RequiresStateMachine()
    {
        const string source = """
import System.Threading.Tasks.*

async func Test(value: int) -> Task<Int32> {
    await Task.Delay(1)
    return value
}

let result = await Test(42)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var functionSyntax = root
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "Test");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(functionSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(functionSyntax.Body!));

        var analysis = AsyncLowerer.Analyze(methodSymbol, boundBody);

        Assert.True(analysis.ContainsAwait);
        Assert.True(analysis.RequiresStateMachine);
        Assert.True(methodSymbol.ContainsAwait);
    }

    [Fact]
    public void Rewrite_TopLevelAsyncFunction_UpdatesCachedBody()
    {
        const string source = """
import System.Threading.Tasks.*

async func Test(value: int) -> Task<Int32> {
    await Task.Delay(1)
    return value
}

let result = await Test(42)
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var functionSyntax = root
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "Test");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(functionSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(functionSyntax.Body!));

        var rewritten = AsyncLowerer.Rewrite(methodSymbol, boundBody);
        model.CacheBoundNode(functionSyntax.Body!, rewritten);

        var rebound = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(functionSyntax.Body!));
        Assert.Same(rewritten, rebound);
    }

    [Fact]
    public void Rewrite_AwaitInReturnExpression_LowersToBlockExpression()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task<Int32> {
        return await Task.FromResult(42)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Work");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryLabel = Assert.IsType<BoundLabeledStatement>(tryStatements[^1]);
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);

        var returnBlock = Assert.IsType<BoundBlockStatement>(entryBlock.Statements
            .OfType<BoundBlockStatement>()
            .Single(block => block.Statements.Last() is BoundReturnStatement));

        var blockStatements = returnBlock.Statements.ToArray();
        Assert.True(blockStatements.Length >= 7);
        Assert.IsType<BoundLocalDeclarationStatement>(blockStatements[0]);
        Assert.IsType<BoundAssignmentStatement>(blockStatements[1]);
        Assert.IsType<BoundIfStatement>(blockStatements[2]);

        var resumeLabel = Assert.IsType<BoundLabeledStatement>(blockStatements[3]);
        var resumeBlock = Assert.IsType<BoundBlockStatement>(resumeLabel.Statement);
        var resumeStatements = resumeBlock.Statements.ToArray();
        Assert.Equal(2, resumeStatements.Length);
        Assert.IsType<BoundAssignmentStatement>(resumeStatements[0]);
        var storeResult = Assert.IsType<BoundExpressionStatement>(resumeStatements[1]);
        var assignment = Assert.IsType<BoundLocalAssignmentExpression>(storeResult.Expression);
        Assert.StartsWith("<>awaitResult", assignment.Local.Name, StringComparison.Ordinal);

        var completionState = Assert.IsType<BoundAssignmentStatement>(blockStatements[^3]);
        var stateFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(completionState.Expression);
        Assert.Same(stateMachine.StateField, stateFieldAssignment.Field);

        var setResultStatement = Assert.IsType<BoundExpressionStatement>(blockStatements[^2]);
        var setResultInvocation = Assert.IsType<BoundInvocationExpression>(setResultStatement.Expression);
        Assert.Equal("SetResult", setResultInvocation.Method.Name);
        var resultArgument = Assert.Single(setResultInvocation.Arguments);
        var resultAccess = Assert.IsType<BoundLocalAccess>(resultArgument);
        Assert.Equal(assignment.Local, resultAccess.Local);

        var returnStatement = Assert.IsType<BoundReturnStatement>(blockStatements[^1]);
        Assert.Null(returnStatement.Expression);
    }

    [Fact]
    public void Rewrite_ExpressionBodiedAsyncMethod_RewritesAwait()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task<Int32> => await Task.FromResult(1)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Work");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var bound = model.GetBoundNode(methodSyntax.ExpressionBody!);
        var boundBody = ToBlock(methodSymbol, bound);

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);

        var collector = new AwaitCollector();
        collector.Visit(stateMachine.MoveNextBody!);
        Assert.Empty(collector.Awaits);
    }

    [Fact]
    public void Rewrite_AsyncAccessor_RewritesAwait()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    private let backing: Int32

    public Value: Task<Int32> {
        async get => await Task.FromResult(backing)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var accessorSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<AccessorDeclarationSyntax>()
            .Single();

        var accessorSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(accessorSyntax));
        var bound = accessorSyntax.Body is not null
            ? model.GetBoundNode(accessorSyntax.Body)
            : model.GetBoundNode(accessorSyntax.ExpressionBody!);
        var boundBody = ToBlock(accessorSymbol, bound);

        AsyncLowerer.Rewrite(accessorSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(accessorSymbol.AsyncStateMachine);
        Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);

        var collector = new AwaitCollector();
        collector.Visit(stateMachine.MoveNextBody!);
        Assert.Empty(collector.Awaits);
    }

    [Fact]
    public void Rewrite_AwaitInsideBinaryExpression_LowersRightOperand()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task<Int32> {
        return 1 + await Task.FromResult(2)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Work");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryBlock = Assert.IsType<BoundBlockStatement>(Assert.IsType<BoundLabeledStatement>(tryStatements[^1]).Statement);

        var returnBlock = Assert.IsType<BoundBlockStatement>(entryBlock.Statements
            .OfType<BoundBlockStatement>()
            .Single(block => block.Statements.Last() is BoundReturnStatement));

        var statements = returnBlock.Statements.ToArray();
        var setResultStatement = Assert.IsType<BoundExpressionStatement>(statements[^2]);
        var invocation = Assert.IsType<BoundInvocationExpression>(setResultStatement.Expression);
        var binary = Assert.IsType<BoundBinaryExpression>(Assert.Single(invocation.Arguments));
        Assert.IsType<BoundLiteralExpression>(binary.Left);
        var awaitOperand = Assert.IsType<BoundBlockExpression>(binary.Right);
        Assert.IsType<BoundExpressionStatement>(awaitOperand.Statements.Last());
    }

    [Fact]
    public void Rewrite_AwaitInInvocationArgument_LowersArgument()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task<Int32> {
        return Wrap(await Task.FromResult(5))
    }

    func Wrap(value: Int32) -> Int32 {
        return value
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Work");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryBlock = Assert.IsType<BoundBlockStatement>(Assert.IsType<BoundLabeledStatement>(tryStatements[^1]).Statement);

        var returnBlock = Assert.IsType<BoundBlockStatement>(entryBlock.Statements
            .OfType<BoundBlockStatement>()
            .Single(block => block.Statements.Last() is BoundReturnStatement));

        var statements = returnBlock.Statements.ToArray();
        var setResultStatement = Assert.IsType<BoundExpressionStatement>(statements[^2]);
        var invocation = Assert.IsType<BoundInvocationExpression>(setResultStatement.Expression);

        var argument = Assert.Single(invocation.Arguments);
        var argumentBlock = Assert.IsType<BoundBlockExpression>(argument);
        Assert.IsType<BoundExpressionStatement>(argumentBlock.Statements.Last());
    }

    [Fact]
    public void Rewrite_AwaitWithinIfBranches_RemovesAwaitNodes()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work(flag: Bool) -> Task {
        if flag {
            await Task.CompletedTask
        } else {
            await Task.CompletedTask
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var originalAwaits = CollectAwaitExpressions(boundBody);
        Assert.Equal(2, originalAwaits.Count);

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        Assert.Empty(CollectAwaitExpressions(moveNextBody));

        var awaiterFields = stateMachine.HoistedLocals
            .Where(field => field.Name.Contains("<>awaiter", StringComparison.Ordinal))
            .ToArray();
        Assert.Equal(2, awaiterFields.Length);

        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryBlock = Assert.IsType<BoundBlockStatement>(Assert.IsType<BoundLabeledStatement>(tryStatements[^1]).Statement);
        var userIf = entryBlock.Statements.OfType<BoundIfStatement>().First(stmt => stmt.ElseNode is not null);
        Assert.IsType<BoundBlockStatement>(userIf.ThenNode);
        Assert.IsType<BoundBlockStatement>(userIf.ElseNode);
    }

    [Fact]
    public void Rewrite_AsyncMethodWithMultipleAwaits_HoistsAwaiterFields()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        let first = await Task.FromResult(1)
        let second = await Task.FromResult(first + 2)
        let third = await Task.FromResult(second + 3)
        backing = third
    }

    private var backing: Int32
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Work");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var awaiterFields = stateMachine.HoistedLocals
            .Where(field => field.Name.StartsWith("<>awaiter", StringComparison.Ordinal))
            .ToArray();

        Assert.Equal(3, awaiterFields.Length);
    }

    [Fact]
    public void Rewrite_AsyncMethodWithThrow_AddsSetException()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        await Task.CompletedTask
        throw InvalidOperationException("boom")
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var catchClause = Assert.Single(tryStatement.CatchClauses);

        var setExceptionInvocation = catchClause.Block.Statements
            .OfType<BoundExpressionStatement>()
            .Select(statement => statement.Expression)
            .OfType<BoundInvocationExpression>()
            .First(invocation => invocation.Method.Name == "SetException");

        var receiver = Assert.IsType<BoundMemberAccessExpression>(setExceptionInvocation.Receiver);
        Assert.Same(stateMachine.BuilderField, Assert.IsAssignableFrom<IFieldSymbol>(receiver.Member));
    }

    [Fact]
    public void Rewrite_AsyncAccessor_EmitsSetResult()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    private var backing: Int32

    public Value: Task {
        async get {
            await Task.CompletedTask
            backing = backing + 1
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var accessorSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<AccessorDeclarationSyntax>()
            .Single();

        var accessorSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(accessorSyntax));
        var bound = accessorSyntax.Body is not null
            ? model.GetBoundNode(accessorSyntax.Body)
            : model.GetBoundNode(accessorSyntax.ExpressionBody!);
        var boundBody = ToBlock(accessorSymbol, bound);

        AsyncLowerer.Rewrite(accessorSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(accessorSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var entryLabel = Assert.IsType<BoundLabeledStatement>(tryStatement.TryBlock.Statements.Last());
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabel.Statement);

        var setResultInvocation = entryBlock.Statements
            .OfType<BoundExpressionStatement>()
            .Select(statement => statement.Expression)
            .OfType<BoundInvocationExpression>()
            .First(invocation => invocation.Method.Name == "SetResult");

        var receiver = Assert.IsType<BoundMemberAccessExpression>(setResultInvocation.Receiver);
        Assert.Same(stateMachine.BuilderField, Assert.IsAssignableFrom<IFieldSymbol>(receiver.Member));
    }

    [Fact]
    public void Rewrite_AwaitInWhileLoop_RemovesAwaitNodes()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        var i = 0
        while i < 3 {
            await Task.CompletedTask
            i = i + 1
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var originalAwaits = CollectAwaitExpressions(boundBody);
        Assert.Single(originalAwaits);

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        Assert.Empty(CollectAwaitExpressions(moveNextBody));

        Assert.Contains(stateMachine.HoistedLocals, field => field.Name.Contains("<>awaiter", StringComparison.Ordinal));
    }

    [Fact]
    public void Rewrite_AwaitInTryCatch_RemovesAwaitNodes()
    {
        const string source = """
import System.Threading.Tasks.*
import System.*

class C {
    async Work() -> Task {
        try {
            await Task.CompletedTask
        } catch (Exception ex) {
            await Task.CompletedTask
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var originalAwaits = CollectAwaitExpressions(boundBody);
        Assert.Equal(2, originalAwaits.Count);

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        Assert.Empty(CollectAwaitExpressions(moveNextBody));

        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryBlock = Assert.IsType<BoundBlockStatement>(Assert.IsType<BoundLabeledStatement>(tryStatements[^1]).Statement);
        var nestedTry = entryBlock.Statements.OfType<BoundTryStatement>().Single();
        var catchClause = Assert.Single(nestedTry.CatchClauses);
        Assert.Empty(CollectAwaitExpressions(catchClause.Block));
    }

    [Fact]
    public void Rewrite_AwaitInTryExpression_RemovesAwaitNodes()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        var attempt = try await Task.FromResult(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var originalAwaits = CollectAwaitExpressions(boundBody);
        Assert.Single(originalAwaits);

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);

        Assert.Empty(CollectAwaitExpressions(moveNextBody));
    }

    [Fact]
    public void Rewrite_AsyncMethodWithAwait_SynthesizesStateMachine()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Work() -> Task {
        await Task.CompletedTask
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

        var rewritten = AsyncLowerer.Rewrite(methodSymbol, boundBody);

        Assert.NotSame(boundBody, rewritten);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        Assert.Same(boundBody, stateMachine.OriginalBody);

        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var outerStatements = moveNextBody.Statements.ToArray();
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(outerStatements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        Assert.Equal(4, tryStatements.Length);

        var dispatch = Assert.IsType<BoundIfStatement>(tryStatements[0]);
        var dispatchCondition = Assert.IsType<BoundBinaryExpression>(dispatch.Condition);
        var stateAccess = Assert.IsType<BoundFieldAccess>(dispatchCondition.Left);
        Assert.Same(stateMachine.StateField, stateAccess.Field);
        var stateLiteral = Assert.IsType<BoundLiteralExpression>(dispatchCondition.Right);
        Assert.Equal(-1, Assert.IsType<int>(stateLiteral.Value));

        var dispatchThen = Assert.IsType<BoundBlockStatement>(dispatch.ThenNode);
        var dispatchGoto = Assert.IsType<BoundGotoStatement>(Assert.Single(dispatchThen.Statements));

        var resumeDispatch = Assert.IsType<BoundIfStatement>(tryStatements[1]);
        var resumeCondition = Assert.IsType<BoundBinaryExpression>(resumeDispatch.Condition);
        var resumeStateAccess = Assert.IsType<BoundFieldAccess>(resumeCondition.Left);
        Assert.Same(stateMachine.StateField, resumeStateAccess.Field);
        var resumeStateLiteral = Assert.IsType<BoundLiteralExpression>(resumeCondition.Right);
        Assert.Equal(0, Assert.IsType<int>(resumeStateLiteral.Value));
        var resumeThen = Assert.IsType<BoundBlockStatement>(resumeDispatch.ThenNode);
        var resumeGoto = Assert.IsType<BoundGotoStatement>(Assert.Single(resumeThen.Statements));

        var fallthroughGoto = Assert.IsType<BoundGotoStatement>(tryStatements[2]);

        var entryLabeled = Assert.IsType<BoundLabeledStatement>(tryStatements[^1]);
        Assert.Equal(dispatchGoto.Target, entryLabeled.Label);
        Assert.Equal(fallthroughGoto.Target, entryLabeled.Label);

        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabeled.Statement);
        var entryStatements = entryBlock.Statements.ToArray();

        var awaiterField = Assert.Single(stateMachine.HoistedLocals);
        Assert.Equal("<>awaiter0", awaiterField.Name);

        var storeAwaiter = Assert.IsType<BoundAssignmentStatement>(entryStatements[0]);
        var storeAwaiterExpression = Assert.IsType<BoundFieldAssignmentExpression>(storeAwaiter.Expression);
        Assert.Same(awaiterField, storeAwaiterExpression.Field);
        Assert.IsType<BoundSelfExpression>(storeAwaiterExpression.Receiver);
        var getAwaiterCall = Assert.IsType<BoundInvocationExpression>(storeAwaiterExpression.Right);
        Assert.Equal("GetAwaiter", getAwaiterCall.Method.Name);

        var awaitIf = Assert.IsType<BoundIfStatement>(entryStatements[1]);
        var awaitElse = Assert.IsType<BoundBlockStatement>(awaitIf.ElseNode);
        var awaitElseStatements = awaitElse.Statements.ToArray();
        Assert.Equal(3, awaitElseStatements.Length);
        var stateAssignmentAwait = Assert.IsType<BoundAssignmentStatement>(awaitElseStatements[0]);
        var stateAssignmentExpr = Assert.IsType<BoundFieldAssignmentExpression>(stateAssignmentAwait.Expression);
        Assert.Same(stateMachine.StateField, stateAssignmentExpr.Field);
        var stateAssignmentLiteral = Assert.IsType<BoundLiteralExpression>(stateAssignmentExpr.Right);
        Assert.Equal(0, Assert.IsType<int>(stateAssignmentLiteral.Value));

        var scheduleStatement = Assert.IsType<BoundExpressionStatement>(awaitElseStatements[1]);
        var scheduleInvocation = Assert.IsType<BoundInvocationExpression>(scheduleStatement.Expression);
        Assert.True(scheduleInvocation.Method.Name is "AwaitUnsafeOnCompleted" or "AwaitOnCompleted");
        var scheduleArguments = scheduleInvocation.Arguments.ToArray();
        Assert.Equal(2, scheduleArguments.Length);
        Assert.IsType<BoundAddressOfExpression>(scheduleArguments[0]);
        Assert.IsType<BoundAddressOfExpression>(scheduleArguments[1]);
        var scheduleReceiver = Assert.IsType<BoundMemberAccessExpression>(scheduleInvocation.Receiver);
        Assert.Same(stateMachine.BuilderField, Assert.IsAssignableFrom<IFieldSymbol>(scheduleReceiver.Member));

        var scheduleReturn = Assert.IsType<BoundReturnStatement>(awaitElseStatements[2]);
        Assert.Null(scheduleReturn.Expression);

        var resumeLabeled = Assert.IsType<BoundLabeledStatement>(entryStatements[2]);
        Assert.Equal(resumeGoto.Target, resumeLabeled.Label);
        var resumeBlock = Assert.IsType<BoundBlockStatement>(resumeLabeled.Statement);
        var resumeStatements = resumeBlock.Statements.ToArray();
        Assert.Equal(2, resumeStatements.Length);
        var resumeStateAssignment = Assert.IsType<BoundAssignmentStatement>(resumeStatements[0]);
        var resumeStateExpr = Assert.IsType<BoundFieldAssignmentExpression>(resumeStateAssignment.Expression);
        Assert.Same(stateMachine.StateField, resumeStateExpr.Field);
        var resumeLiteral = Assert.IsType<BoundLiteralExpression>(resumeStateExpr.Right);
        Assert.Equal(-1, Assert.IsType<int>(resumeLiteral.Value));

        var getResultStatement = Assert.IsType<BoundExpressionStatement>(resumeStatements[1]);
        var getResultInvocation = Assert.IsType<BoundInvocationExpression>(getResultStatement.Expression);
        Assert.Equal("GetResult", getResultInvocation.Method.Name);
        var getResultReceiver = Assert.IsType<BoundMemberAccessExpression>(getResultInvocation.Receiver);
        Assert.Same(awaiterField, Assert.IsAssignableFrom<IFieldSymbol>(getResultReceiver.Member));

        var completionStateAssignment = Assert.IsType<BoundAssignmentStatement>(entryStatements[^3]);
        var completionStateField = Assert.IsType<BoundFieldAssignmentExpression>(completionStateAssignment.Expression);
        Assert.Same(stateMachine.StateField, completionStateField.Field);

        var completionSetResult = Assert.IsType<BoundExpressionStatement>(entryStatements[^2]);
        var setResultInvocation = Assert.IsType<BoundInvocationExpression>(completionSetResult.Expression);
        Assert.Equal("SetResult", setResultInvocation.Method.Name);

        var completionReturn = Assert.IsType<BoundReturnStatement>(entryStatements[^1]);
        Assert.Null(completionReturn.Expression);

        var catchClause = Assert.Single(tryStatement.CatchClauses);
        Assert.Equal(
            compilation.GetSpecialType(SpecialType.System_Exception),
            catchClause.ExceptionType);
        var catchLocal = Assert.IsAssignableFrom<ILocalSymbol>(catchClause.Local);
        Assert.Equal("<>ex", catchLocal.Name);

        var catchStatements = catchClause.Block.Statements.ToArray();
        Assert.True(catchStatements.Length >= 2);
        var catchStateAssignment = Assert.IsType<BoundAssignmentStatement>(catchStatements[0]);
        var catchStateFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(catchStateAssignment.Expression);
        Assert.Same(stateMachine.StateField, catchStateFieldAssignment.Field);

        var setExceptionStatement = Assert.IsType<BoundExpressionStatement>(catchStatements[1]);
        var setExceptionInvocation = Assert.IsType<BoundInvocationExpression>(setExceptionStatement.Expression);
        Assert.Equal("SetException", setExceptionInvocation.Method.Name);
        var setExceptionReceiver = Assert.IsType<BoundMemberAccessExpression>(setExceptionInvocation.Receiver);
        Assert.Same(stateMachine.BuilderField, Assert.IsAssignableFrom<IFieldSymbol>(setExceptionReceiver.Member));

        var setExceptionArgument = Assert.Single(setExceptionInvocation.Arguments);
        var exceptionAccess = Assert.IsType<BoundLocalAccess>(setExceptionArgument);
        Assert.Same(catchLocal, exceptionAccess.Local);

        var catchReturn = Assert.IsType<BoundReturnStatement>(catchStatements[^1]);
        Assert.Null(catchReturn.Expression);

        var synthesizedTypes = compilation.GetSynthesizedAsyncStateMachineTypes().ToArray();
        Assert.Contains(stateMachine, synthesizedTypes);

        var statements = rewritten.Statements.ToArray();
        var declaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        var declarator = Assert.Single(declaration.Declarators);
        Assert.Equal("<>async", declarator.Local.Name);
        Assert.Same(stateMachine, declarator.Local.Type);
        var asyncLocal = declarator.Local;

        var builderField = stateMachine.BuilderField;
        Assert.NotNull(builderField);

        var thisField = Assert.IsType<SourceFieldSymbol>(stateMachine.ThisField);
        var thisAssignment = Assert.IsType<BoundAssignmentStatement>(statements[1]);
        var thisFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(thisAssignment.Expression);
        Assert.Same(thisField, thisFieldAssignment.Field);

        var stateAssignment = Assert.IsType<BoundAssignmentStatement>(statements[^4]);
        var stateFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(stateAssignment.Expression);
        Assert.Same(stateMachine.StateField, stateFieldAssignment.Field);

        var builderAssignment = Assert.IsType<BoundAssignmentStatement>(statements[^3]);
        var builderFieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(builderAssignment.Expression);
        Assert.Same(builderField, builderFieldAssignment.Field);
        Assert.IsType<BoundLocalAccess>(builderFieldAssignment.Receiver);
        var createCall = Assert.IsType<BoundInvocationExpression>(builderFieldAssignment.Right);
        Assert.Equal("Create", createCall.Method.Name);

        var startCall = Assert.IsType<BoundExpressionStatement>(statements[^2]);
        var startInvocation = Assert.IsType<BoundInvocationExpression>(startCall.Expression);
        Assert.Equal("Start", startInvocation.Method.Name);
        var startReceiver = Assert.IsType<BoundMemberAccessExpression>(startInvocation.Receiver);
        Assert.Same(builderField, Assert.IsAssignableFrom<IFieldSymbol>(startReceiver.Member));
        var startReceiverLocal = Assert.IsType<BoundLocalAccess>(startReceiver.Receiver);
        Assert.Same(asyncLocal, startReceiverLocal.Local);
        var startArguments = startInvocation.Arguments.ToArray();
        var addressOf = Assert.IsType<BoundAddressOfExpression>(Assert.Single(startArguments));
        Assert.Same(asyncLocal, Assert.IsAssignableFrom<ILocalSymbol>(addressOf.Symbol));
        Assert.Same(stateMachine, addressOf.ValueType);

        var returnStatement = Assert.IsType<BoundReturnStatement>(statements[^1]);
        var returnAccess = Assert.IsType<BoundMemberAccessExpression>(returnStatement.Expression);
        var taskProperty = Assert.IsAssignableFrom<IPropertySymbol>(returnAccess.Member);
        Assert.Equal("Task", taskProperty.Name);

        var builderAccess = Assert.IsType<BoundMemberAccessExpression>(returnAccess.Receiver);
        Assert.Same(builderField, Assert.IsAssignableFrom<IFieldSymbol>(builderAccess.Member));
        var builderReceiver = Assert.IsType<BoundLocalAccess>(builderAccess.Receiver);
        Assert.Equal("<>async", builderReceiver.Local.Name);

        var asyncInterface = compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine);
        Assert.Contains(stateMachine.Interfaces, i => SymbolEqualityComparer.Default.Equals(i, asyncInterface));
        Assert.NotNull(stateMachine.SetStateMachineMethod);

        var setStateMachineBody = Assert.IsType<BoundBlockStatement>(stateMachine.SetStateMachineBody);
        var setStatements = Assert.Single(setStateMachineBody.Statements);
        var setExpressionStatement = Assert.IsType<BoundExpressionStatement>(setStatements);
        var setInvocation = Assert.IsType<BoundInvocationExpression>(setExpressionStatement.Expression);
        Assert.Equal("SetStateMachine", setInvocation.Method.Name);
        var setReceiver = Assert.IsType<BoundMemberAccessExpression>(setInvocation.Receiver);
        Assert.Same(stateMachine.BuilderField, Assert.IsAssignableFrom<IFieldSymbol>(setReceiver.Member));
        Assert.IsType<BoundSelfExpression>(setReceiver.Receiver);
        var setArgument = Assert.Single(setInvocation.Arguments);
        var parameterAccess = Assert.IsType<BoundParameterAccess>(setArgument);
        Assert.Same(stateMachine.SetStateMachineMethod.Parameters[0], parameterAccess.Parameter);
    }

    [Fact]
    public void Rewrite_AsyncMethodWithLocalAcrossAwait_HoistsLocal()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Compute() -> Task {
        var value = 42
        await Task.CompletedTask
        value = value + 1
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

        var rewritten = AsyncLowerer.Rewrite(methodSymbol, boundBody);

        Assert.NotSame(boundBody, rewritten);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var hoistedLocals = stateMachine.HoistedLocals.ToArray();

        var localField = Assert.Single(hoistedLocals.Where(field => field.Name == "<>local0"));
        Assert.Equal(SpecialType.System_Int32, localField.Type.SpecialType);

        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));
        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryLabeled = Assert.IsType<BoundLabeledStatement>(tryStatements[^1]);
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabeled.Statement);
        var entryStatements = entryBlock.Statements.ToArray();

        var initialization = Assert.IsType<BoundAssignmentStatement>(entryStatements[0]);
        var initializationExpression = Assert.IsType<BoundFieldAssignmentExpression>(initialization.Expression);
        Assert.Same(localField, Assert.IsAssignableFrom<IFieldSymbol>(initializationExpression.Field));
        var initializerLiteral = Assert.IsType<BoundLiteralExpression>(initializationExpression.Right);
        Assert.Equal(42, Assert.IsType<int>(initializerLiteral.Value));

        var updateAssignment = entryStatements
            .OfType<BoundAssignmentStatement>()
            .Select(statement => statement.Expression)
            .OfType<BoundFieldAssignmentExpression>()
            .First(expression => ReferenceEquals(expression.Field, localField) && expression.Right is BoundBinaryExpression);

        Assert.IsType<BoundBinaryExpression>(updateAssignment.Right);
    }

    [Fact]
    public void Rewrite_AsyncMethodWithUsingAcrossAwait_DisposesHoistedLocal()
    {
        const string source = """
import System.IO.*
import System.Threading.Tasks.*

class C {
    async Compute() -> Task {
        using var stream = new MemoryStream()
        await Task.CompletedTask
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

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var hoistedLocals = stateMachine.HoistedLocals.ToArray();
        var hoistedDisposable = Assert.Single(hoistedLocals.Where(field => field.Name == "<>local0"));
        Assert.Contains(hoistedDisposable, stateMachine.HoistedLocalsToDispose);

        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var tryStatement = Assert.IsType<BoundTryStatement>(Assert.Single(moveNextBody.Statements));

        var tryStatements = tryStatement.TryBlock.Statements.ToArray();
        var entryLabeled = Assert.IsType<BoundLabeledStatement>(tryStatements[^1]);
        var entryBlock = Assert.IsType<BoundBlockStatement>(entryLabeled.Statement);
        var entryStatements = entryBlock.Statements.ToArray();

        var disposeGuard = Assert.IsType<BoundIfStatement>(entryStatements[^4]);
        var disposeBlock = Assert.IsType<BoundBlockStatement>(disposeGuard.ThenNode);
        var disposeStatements = disposeBlock.Statements.ToArray();
        var disposeCallStatement = Assert.IsType<BoundExpressionStatement>(disposeStatements[0]);
        var disposeInvocation = Assert.IsType<BoundInvocationExpression>(disposeCallStatement.Expression);
        Assert.Equal("Dispose", disposeInvocation.Method.Name);
        var disposeReceiver = Assert.IsType<BoundMemberAccessExpression>(disposeInvocation.Receiver);
        Assert.Same(hoistedDisposable, Assert.IsAssignableFrom<IFieldSymbol>(disposeReceiver.Member));

        if (disposeStatements.Length > 1)
        {
            var resetAssignment = Assert.IsType<BoundAssignmentStatement>(disposeStatements[1]);
            var resetField = Assert.IsType<BoundFieldAssignmentExpression>(resetAssignment.Expression);
            Assert.Same(hoistedDisposable, Assert.IsAssignableFrom<IFieldSymbol>(resetField.Field));
        }

        var catchClause = Assert.Single(tryStatement.CatchClauses);
        var catchStatements = catchClause.Block.Statements.ToArray();
        var catchDisposeGuard = Assert.IsType<BoundIfStatement>(catchStatements[1]);
        var catchDisposeBlock = Assert.IsType<BoundBlockStatement>(catchDisposeGuard.ThenNode);
        var catchDisposeStatements = catchDisposeBlock.Statements.ToArray();
        var catchDisposeInvocation = Assert.IsType<BoundInvocationExpression>(
            Assert.IsType<BoundExpressionStatement>(catchDisposeStatements[0]).Expression);
        Assert.Equal("Dispose", catchDisposeInvocation.Method.Name);
        var catchReceiver = Assert.IsType<BoundMemberAccessExpression>(catchDisposeInvocation.Receiver);
        Assert.Same(hoistedDisposable, Assert.IsAssignableFrom<IFieldSymbol>(catchReceiver.Member));
    }

    [Fact]
    public void GetConstructedMembers_GenericAsyncMethod_SubstitutesStateMachineMembers()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    static async Compute<T>(value: T) -> Task<T> {
        await Task.FromResult(value)
        return value
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Compute");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var constructed = stateMachine.GetConstructedMembers(methodSymbol);

        var methodTypeParameter = Assert.Single(methodSymbol.TypeParameters);
        var stateMachineTypeParameter = Assert.Single(stateMachine.TypeParameters);

        var constructedType = constructed.StateMachineType;
        Assert.Same(stateMachine, constructedType);

        var builderField = constructed.BuilderField;
        Assert.Same(stateMachine.BuilderField, builderField);
        Assert.Same(builderField, constructed.BuilderMembers.BuilderField);

        var builderType = Assert.IsAssignableFrom<INamedTypeSymbol>(builderField.Type);
        Assert.True(SymbolEqualityComparer.Default.Equals(stateMachineTypeParameter, builderType.TypeArguments[0]));
        Assert.False(SymbolEqualityComparer.Default.Equals(methodTypeParameter, builderType.TypeArguments[0]));

        var parameter = Assert.Single(methodSymbol.Parameters);
        Assert.True(constructed.ParameterFields.TryGetValue(parameter, out var parameterField));
        Assert.NotNull(parameterField);

        Assert.Same(stateMachine.ParameterFieldMap[parameter], parameterField);
        Assert.True(SymbolEqualityComparer.Default.Equals(stateMachineTypeParameter, parameterField!.Type));
        Assert.False(SymbolEqualityComparer.Default.Equals(methodTypeParameter, parameterField.Type));

        Assert.Same(stateMachine.Constructor, constructed.Constructor);
        Assert.Same(stateMachine.MoveNextMethod, constructed.MoveNext);
    }

    [Fact]
    public void AsyncStateMachine_ProvidesTypeParameterMappings()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    static async Compute<T>(value: T) -> Task<T> {
        await Task.FromResult(value)
        return value
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.ValueText == "Compute");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var methodParameter = Assert.Single(methodSymbol.TypeParameters);
        var stateMachineParameter = Assert.Single(stateMachine.TypeParameters);

        var mapping = Assert.Single(stateMachine.TypeParameterMappings);
        Assert.Same(methodParameter, mapping.AsyncParameter);
        Assert.Same(stateMachineParameter, mapping.StateMachineParameter);

        Assert.True(stateMachine.TryMapToStateMachineTypeParameter(methodParameter, out var mappedToState));
        Assert.Same(stateMachineParameter, mappedToState);

        Assert.True(stateMachine.TryMapToAsyncMethodTypeParameter(stateMachineParameter, out var mappedToAsync));
        Assert.Same(methodParameter, mappedToAsync);
    }

    private static IReadOnlyList<BoundAwaitExpression> CollectAwaitExpressions(BoundNode node)
    {
        var collector = new AwaitCollector();
        collector.Visit(node);
        return collector.Awaits;
    }

    private sealed class AwaitCollector : BoundTreeWalker
    {
        private readonly List<BoundAwaitExpression> _awaits = new();

        public IReadOnlyList<BoundAwaitExpression> Awaits => _awaits;

        public override void VisitAwaitExpression(BoundAwaitExpression node)
        {
            if (node is null)
                return;

            _awaits.Add(node);
            base.VisitAwaitExpression(node);
        }
    }

    private static BoundBlockStatement ToBlock(SourceMethodSymbol methodSymbol, BoundNode bound)
    {
        return bound switch
        {
            BoundBlockStatement block => block,
            BoundBlockExpression blockExpr => new BoundBlockStatement(blockExpr.Statements, blockExpr.LocalsToDispose),
            BoundExpression expression when methodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
                => new BoundBlockStatement(new BoundStatement[] { new BoundExpressionStatement(expression) }),
            BoundExpression expression => new BoundBlockStatement(new BoundStatement[] { new BoundReturnStatement(expression) }),
            _ => throw new InvalidOperationException("Unsupported async body shape."),
        };
    }
}
