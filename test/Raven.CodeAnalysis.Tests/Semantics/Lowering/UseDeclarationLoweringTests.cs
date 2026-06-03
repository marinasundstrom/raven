using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Lowering.Tests;

public sealed class UseDeclarationLoweringTests : CompilationTestBase
{
    [Fact]
    public void Lowerer_UseDeclaration_WrapsFollowingStatementsInTryFinally()
    {
        const string source = """
import System.*

class Resource : IDisposable {
    init() {}
    func Run() -> unit {}
    func Dispose() -> unit {}
}

class C {
    func Test() {
        use resource = Resource()
        resource.Run()
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");

        var lowered = Lowerer.LowerBlock(methodSymbol, boundBody);
        var statements = lowered.Statements.ToArray();

        Assert.Empty(lowered.LocalsToDispose);
        Assert.Equal(2, statements.Length);

        var declaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        Assert.False(declaration.IsUsing);
        Assert.Equal("resource", declaration.Declarators.Single().Local.Name);

        var tryStatement = Assert.IsType<BoundTryStatement>(statements[1]);
        Assert.Empty(tryStatement.CatchClauses);
        Assert.NotNull(tryStatement.FinallyBlock);

        var tryBody = tryStatement.TryBlock.Statements.ToArray();
        var runStatement = Assert.Single(tryBody);
        var runInvocation = AssertInvocationExpression(runStatement);
        Assert.Equal("Run", runInvocation.Method.Name);

        var disposeInvocation = Assert.Single(CollectInvocations(tryStatement.FinallyBlock!),
            static invocation => invocation.Method.Name == "Dispose");
        Assert.Equal("Dispose", disposeInvocation.Method.Name);
    }

    [Fact]
    public void Lowerer_MultipleUseDeclarations_NestsDisposalScopes()
    {
        const string source = """
import System.*

class Resource : IDisposable {
    init(name: string) {}
    func Run(other: Resource) -> unit {}
    func Dispose() -> unit {}
}

class C {
    func Test() {
        use outer = Resource("outer")
        use inner = Resource("inner")
        outer.Run(inner)
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");

        var lowered = Lowerer.LowerBlock(methodSymbol, boundBody);
        var statements = lowered.Statements.ToArray();

        Assert.Empty(lowered.LocalsToDispose);
        Assert.Equal(2, statements.Length);

        var outerDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        Assert.False(outerDeclaration.IsUsing);
        Assert.Equal("outer", outerDeclaration.Declarators.Single().Local.Name);

        var outerTry = Assert.IsType<BoundTryStatement>(statements[1]);
        var outerTryStatements = outerTry.TryBlock.Statements.ToArray();
        Assert.Equal(2, outerTryStatements.Length);

        var innerDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(outerTryStatements[0]);
        Assert.False(innerDeclaration.IsUsing);
        Assert.Equal("inner", innerDeclaration.Declarators.Single().Local.Name);

        var innerTry = Assert.IsType<BoundTryStatement>(outerTryStatements[1]);
        var runInvocation = AssertInvocationExpression(Assert.Single(innerTry.TryBlock.Statements));
        Assert.Equal("Run", runInvocation.Method.Name);

        Assert.Contains(CollectInvocations(innerTry.FinallyBlock!), static invocation => invocation.Method.Name == "Dispose");
        Assert.Contains(CollectInvocations(outerTry.FinallyBlock!), static invocation => invocation.Method.Name == "Dispose");
    }

    private (IMethodSymbol Method, BoundBlockStatement Body) BindMethodBody(string source, string methodName)
    {
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.Text == methodName);

        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        return (methodSymbol, boundBody);
    }

    private static BoundInvocationExpression AssertInvocationExpression(BoundStatement statement)
    {
        var expressionStatement = Assert.IsType<BoundExpressionStatement>(statement);
        return Assert.IsType<BoundInvocationExpression>(expressionStatement.Expression);
    }

    private static BoundInvocationExpression[] CollectInvocations(BoundBlockStatement block)
    {
        var collector = new InvocationCollector();
        collector.VisitBlockStatement(block);
        return collector.Invocations.ToArray();
    }

    private sealed class InvocationCollector : BoundTreeWalker
    {
        public List<BoundInvocationExpression> Invocations { get; } = [];

        public override void VisitInvocationExpression(BoundInvocationExpression node)
        {
            Invocations.Add(node);
            base.VisitInvocationExpression(node);
        }
    }
}
