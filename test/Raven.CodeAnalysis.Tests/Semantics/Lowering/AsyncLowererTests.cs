using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

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
    public void Rewrite_AsyncMethodWithoutAwait_RewritesWithoutStateMachine()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Compute() -> Task<Int32> {
        return 42
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

        var rewritten = AsyncLowerer.Rewrite(methodSymbol, boundBody);

        Assert.Null(methodSymbol.AsyncStateMachine);

        var returnStatement = Assert.IsType<BoundReturnStatement>(Assert.Single(rewritten.Statements));
        Assert.NotNull(returnStatement.Expression);
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

val result = await Test(42)
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

val result = await Test(42)
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
    public void Analyze_AsyncMethodWithAwaitInUseDeclaration_FindsAwait()
    {
        const string source = """
import System.Net.Http.*
import System.Threading.Tasks.*

class C {
    async Fetch(url: string) -> Task<string> {
        use client = HttpClient()
        use response = await client.GetAsync(url)
        return await response.Content.ReadAsStringAsync()
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

        var analysis = AsyncLowerer.Analyze(methodSymbol, boundBody);

        Assert.True(analysis.ContainsAwait);
        Assert.True(methodSymbol.ContainsAwait);
    }

    [Fact]
    public void Analyze_TopLevelAsyncFunctionWithAwaitInTry_FindsAwait()
    {
        const string source = """
import System.Net.Http.*
import System.Threading.Tasks.*

async func Fetch(url: string) -> Task<string> {
    use client = HttpClient()

    try {
        use response = await client.GetAsync(url)
        return await response.Content.ReadAsStringAsync()
    } catch (HttpRequestException e) {
        return e.Message
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var functionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(functionSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(functionSyntax.Body!));

        var analysis = AsyncLowerer.Analyze(methodSymbol, boundBody);

        Assert.True(analysis.ContainsAwait);
        Assert.True(methodSymbol.ContainsAwait);
    }

    [Fact]
    public void Rewrite_GenericAsyncMethod_ConstructedStateMachineBuilderField_UsesStateMachineOwnedTypeParameter()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Compute<T>(value: T) -> Task<T> {
        await Task.Delay(1)
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
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var rewritten = AsyncLowerer.RewriteMethod(methodSymbol, boundBody);
        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(rewritten.StateMachine);

        var constructedStateMachine = stateMachine.GetConstructedStateMachine(methodSymbol);
        Assert.False(ReferenceEquals(stateMachine, constructedStateMachine));
        var constructedStateArg = Assert.Single(constructedStateMachine.TypeArguments);
        var constructedStateArgTypeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(constructedStateArg);
        Assert.Equal(TypeParameterOwnerKind.Method, constructedStateArgTypeParameter.OwnerKind);
        var builderField = constructedStateMachine.GetMembers("_builder").OfType<IFieldSymbol>().Single();

        var builderType = Assert.IsAssignableFrom<INamedTypeSymbol>(builderField.Type);
        var builderTypeArgument = Assert.Single(builderType.TypeArguments);
        var typeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(builderTypeArgument);

        Assert.Equal(TypeParameterOwnerKind.Method, typeParameter.OwnerKind);
        Assert.NotNull(typeParameter.DeclaringMethodParameterOwner);
    }

    [Fact]
    public void Rewrite_GenericAsyncMethod_AsyncBuilderMembers_UseMethodOwnedTypeParameter()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    async Compute<T>(value: T) -> Task<T> {
        await Task.Delay(1)
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
            .Single();

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var rewritten = AsyncLowerer.RewriteMethod(methodSymbol, boundBody);
        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(rewritten.StateMachine);

        var members = stateMachine.GetConstructedMembers(methodSymbol);
        var createMethod = members.AsyncMethodBuilderMembers.Create;
        Assert.NotNull(createMethod);
        var taskProperty = members.AsyncMethodBuilderMembers.TaskProperty;
        Assert.NotNull(taskProperty);

        var createReturnType = Assert.IsAssignableFrom<INamedTypeSymbol>(createMethod.ReturnType);
        var createReturnArgument = Assert.Single(createReturnType.TypeArguments);
        var createReturnTypeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(createReturnArgument);

        Assert.Equal(TypeParameterOwnerKind.Method, createReturnTypeParameter.OwnerKind);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol, createReturnTypeParameter.DeclaringMethodParameterOwner));

        var taskType = Assert.IsAssignableFrom<INamedTypeSymbol>(taskProperty.Type);
        var taskTypeArgument = Assert.Single(taskType.TypeArguments);
        var taskTypeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(taskTypeArgument);

        Assert.Equal(TypeParameterOwnerKind.Method, taskTypeParameter.OwnerKind);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodSymbol, taskTypeParameter.DeclaringMethodParameterOwner));
    }

    [Fact]
    public void Analyze_TopLevelAsyncFunctionWithAwaitAndUnionMatch_FindsAwait()
    {
        const string source = """
import System.Console.*
import System.Net.Http.*
import System.Threading.Tasks.*

val res = await fetch("http://www.contoso.com/")

val str = res match {
    .Ok(str) => "Response is: ${str}"
    .Error(message) => "Error is: ${message}"
}

WriteLine(str)

async func fetch(url: string) -> Task<Result<string, string>> {
    use client = HttpClient()

    try {
        use response = await client.GetAsync(url)
        response.EnsureSuccessStatusCode()
        val responseBody = await response.Content.ReadAsStringAsync()
        return .Ok(responseBody)
    } catch (HttpRequestException e) {
        return .Error(e.Message)
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var functionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.ValueText == "fetch");

        var methodSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(functionSyntax));
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(Compilation.ContainsAwaitExpressionOutsideNestedFunctions(functionSyntax.Body!));
        Assert.True(methodSymbol.ContainsAwait);
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.AsyncLacksAwait.Id);

        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(functionSyntax.Body!));
        var analysis = AsyncLowerer.Analyze(methodSymbol, boundBody);

        Assert.True(analysis.ContainsAwait);
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

        var allStatements = EnumerateStatements(entryBlock).ToArray();

        Assert.Contains(allStatements, statement => statement is BoundIfStatement);

        var resumeLabel = Assert.IsType<BoundLabeledStatement>(
            allStatements.Single(statement => statement is BoundLabeledStatement labeled &&
                                              labeled.Label.Name.Contains("state", StringComparison.Ordinal)));
        var resumeBlock = Assert.IsType<BoundBlockStatement>(resumeLabel.Statement);
        var resumeStatements = resumeBlock.Statements.ToArray();
        Assert.Contains(resumeStatements, statement => statement is BoundAssignmentStatement);
        var storeResult = Assert.IsType<BoundExpressionStatement>(
            resumeStatements.Single(statement => statement is BoundExpressionStatement expressionStatement &&
                                                 expressionStatement.Expression is BoundLocalAssignmentExpression localAssignment &&
                                                 localAssignment.Local.Name.StartsWith("<>awaitResult", StringComparison.Ordinal)));
        var assignment = Assert.IsType<BoundLocalAssignmentExpression>(storeResult.Expression);
        Assert.StartsWith("<>awaitResult", assignment.Local.Name, StringComparison.Ordinal);

        var setResultStatement = Assert.IsType<BoundExpressionStatement>(
            allStatements.Single(statement => statement is BoundExpressionStatement expressionStatement &&
                                              expressionStatement.Expression is BoundInvocationExpression invocation &&
                                              invocation.Method.Name == "SetResult"));
        _ = Assert.IsType<BoundInvocationExpression>(setResultStatement.Expression);

        var returnStatement = Assert.IsType<BoundReturnStatement>(allStatements.Last());
        Assert.Null(returnStatement.Expression);

        static IEnumerable<BoundStatement> EnumerateStatements(BoundBlockStatement block)
        {
            foreach (var statement in block.Statements)
            {
                yield return statement;

                switch (statement)
                {
                    case BoundBlockStatement nestedBlock:
                        foreach (var nested in EnumerateStatements(nestedBlock))
                            yield return nested;
                        break;
                    case BoundLabeledStatement labeledStatement when labeledStatement.Statement is BoundBlockStatement labeledBlock:
                        foreach (var nested in EnumerateStatements(labeledBlock))
                            yield return nested;
                        break;
                }
            }
        }
    }

    [Fact]
    public void Rewrite_AsyncAccessor_RewritesAwait()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    private val backing: Int32

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

        if (accessorSymbol.AsyncStateMachine is SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);

            var collector = new AwaitCollector();
            collector.Visit(stateMachine.MoveNextBody!);
            Assert.Empty(collector.Awaits);
        }
        else
        {
            var collector = new AwaitCollector();
            collector.Visit(boundBody);
            Assert.Empty(collector.Awaits);
        }
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

        if (methodSymbol.AsyncStateMachine is SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
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
        else
        {
            var collector = new AwaitCollector();
            collector.Visit(boundBody);
            Assert.Empty(collector.Awaits);
        }
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
        val first = await Task.FromResult(1)
        val second = await Task.FromResult(first + 2)
        val third = await Task.FromResult(second + 3)
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
        Assert.NotNull(stateMachine.OriginalBody);

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

        var storeAwaiterExpression = entryStatements
            .OfType<BoundAssignmentStatement>()
            .Select(statement => statement.Expression)
            .OfType<BoundFieldAssignmentExpression>()
            .First(expression => ReferenceEquals(expression.Field, awaiterField));
        Assert.Same(awaiterField, storeAwaiterExpression.Field);
        Assert.IsType<BoundSelfExpression>(storeAwaiterExpression.Receiver);
        var getAwaiterCall = Assert.IsType<BoundInvocationExpression>(storeAwaiterExpression.Right);
        Assert.Equal("GetAwaiter", getAwaiterCall.Method.Name);

        var awaitIf = Assert.IsType<BoundIfStatement>(entryStatements.OfType<BoundIfStatement>().First());
        if (awaitIf.ElseNode is BoundBlockStatement awaitElse)
        {
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
        }
        else
        {
            var collector = new AwaitCollector();
            collector.Visit(entryBlock);
            Assert.Empty(collector.Awaits);
        }

        Assert.Empty(CollectAwaitExpressions(entryBlock));
        Assert.Contains(
            entryStatements.OfType<BoundExpressionStatement>(),
            statement => statement.Expression is BoundInvocationExpression invocation &&
                         invocation.Method.Name == "SetResult");

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
        var setExpressionStatement = Assert.IsType<BoundExpressionStatement>(
            setStateMachineBody.Statements.OfType<BoundExpressionStatement>().First());
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

        var initializationExpression = entryStatements
            .OfType<BoundAssignmentStatement>()
            .Select(statement => statement.Expression)
            .OfType<BoundFieldAssignmentExpression>()
            .First(expression => ReferenceEquals(expression.Field, localField) && expression.Right is BoundLiteralExpression);
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
        Assert.Empty(stateMachine.HoistedLocalsToDispose);

        var originalBody = Assert.IsType<BoundBlockStatement>(stateMachine.OriginalBody);
        var moveNextBody = Assert.IsType<BoundBlockStatement>(stateMachine.MoveNextBody);
        var hasUseDeclaration = false;
        var hasTryStatement = false;

        ScanStatements(originalBody.Statements);
        ScanStatements(moveNextBody.Statements);

        Assert.False(hasUseDeclaration);
        Assert.True(hasTryStatement);

        void ScanStatements(IEnumerable<BoundStatement> statements)
        {
            foreach (var statement in statements)
            {
                switch (statement)
                {
                    case BoundLocalDeclarationStatement localDeclaration when localDeclaration.IsUsing:
                        hasUseDeclaration = true;
                        break;
                    case BoundTryStatement tryStatement:
                        hasTryStatement = true;
                        ScanStatements(tryStatement.TryBlock.Statements);
                        foreach (var catchClause in tryStatement.CatchClauses)
                            ScanStatements(catchClause.Block.Statements);
                        if (tryStatement.FinallyBlock is not null)
                            ScanStatements(tryStatement.FinallyBlock.Statements);
                        break;
                    case BoundBlockStatement blockStatement:
                        ScanStatements(blockStatement.Statements);
                        break;
                    case BoundLabeledStatement labeledStatement:
                        ScanStatements(new[] { labeledStatement.Statement });
                        break;
                }
            }
        }
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
        var constructedStateMachine = Assert.IsType<ConstructedNamedTypeSymbol>(constructedType);
        Assert.Same(stateMachine, constructedStateMachine.ConstructedFrom);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, Assert.Single(constructedStateMachine.TypeArguments)));

        var builderField = constructed.BuilderField;
        Assert.NotSame(stateMachine.BuilderField, builderField);
        Assert.Same(builderField, constructed.StateMachineBuilderMembers.BuilderField);

        var builderType = Assert.IsAssignableFrom<INamedTypeSymbol>(builderField.Type);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, builderType.TypeArguments[0]));
        Assert.False(SymbolEqualityComparer.Default.Equals(stateMachineTypeParameter, builderType.TypeArguments[0]));

        var parameter = Assert.Single(methodSymbol.Parameters);
        Assert.True(constructed.ParameterFields.TryGetValue(parameter, out var parameterField));
        Assert.NotNull(parameterField);

        Assert.NotSame(stateMachine.ParameterFieldMap[parameter], parameterField);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, parameterField!.Type));
        Assert.False(SymbolEqualityComparer.Default.Equals(stateMachineTypeParameter, parameterField.Type));

        Assert.NotSame(stateMachine.Constructor, constructed.Constructor);
        Assert.NotSame(stateMachine.MoveNextMethod, constructed.MoveNext);
    }

    [Fact]
    public void Rewrite_AsyncGenericMethod_UsesAsyncMethodTypeParametersForBuilder()
    {
        const string source = """
import System.Threading.Tasks.*

class C {
    static async Compute<T>(value: T) -> Task<T> {
        await Task.Delay(1)
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

        var rewrittenBody = AsyncLowerer.Rewrite(methodSymbol, boundBody);

        var stateMachine = Assert.IsType<SynthesizedAsyncStateMachineTypeSymbol>(methodSymbol.AsyncStateMachine);
        var constructed = stateMachine.GetConstructedMembers(methodSymbol);
        var methodTypeParameter = Assert.Single(methodSymbol.TypeParameters);

        var asyncBuilderMembers = constructed.AsyncMethodBuilderMembers;
        var createMethod = Assert.IsAssignableFrom<IMethodSymbol>(asyncBuilderMembers.Create);
        var createReturnType = Assert.IsAssignableFrom<INamedTypeSymbol>(createMethod.ReturnType);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, createReturnType.TypeArguments[0]));

        var createContainingType = Assert.IsAssignableFrom<INamedTypeSymbol>(createMethod.ContainingType);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, createContainingType.TypeArguments[0]));

        var builderAssignments = rewrittenBody.Statements
            .OfType<BoundAssignmentStatement>()
            .Select(statement => statement.Expression)
            .OfType<BoundFieldAssignmentExpression>();

        var builderInitialization = Assert.Single(
            builderAssignments,
            assignment => assignment.Field.Name == "_builder" && assignment.Right is BoundInvocationExpression);

        var builderInvocation = Assert.IsType<BoundInvocationExpression>(builderInitialization.Right);
        var invocationReturnType = Assert.IsAssignableFrom<INamedTypeSymbol>(builderInvocation.Type);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, invocationReturnType.TypeArguments[0]));

        var invocationContainingType = Assert.IsAssignableFrom<INamedTypeSymbol>(builderInvocation.Method.ContainingType);
        Assert.True(SymbolEqualityComparer.Default.Equals(methodTypeParameter, invocationContainingType.TypeArguments[0]));
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

        Assert.Equal(TypeParameterOwnerKind.Method, methodParameter.OwnerKind);
        Assert.Same(methodSymbol, methodParameter.DeclaringMethodParameterOwner);
        Assert.Null(methodParameter.DeclaringTypeParameterOwner);

        Assert.Equal(TypeParameterOwnerKind.Type, stateMachineParameter.OwnerKind);
        Assert.Same(stateMachine, stateMachineParameter.DeclaringTypeParameterOwner);
        Assert.Null(stateMachineParameter.DeclaringMethodParameterOwner);

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
