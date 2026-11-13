using System.Collections;
using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class ConditionalCallRewriterContractTests
{
    [Fact]
    public void OmittedInvocationSkipsReceiverExtensionReceiverAndArguments()
    {
        var method = Method();
        var receiver = new BoundInvocationExpression(method, new UnvisitableArguments());
        var extension = new BoundInvocationExpression(method, new UnvisitableArguments());
        var call = new BoundInvocationExpression(method, new UnvisitableArguments(), receiver, extension,
            requiresReceiverAddress: true, isConditionallyOmitted: true);
        var result = ConditionalCallRewriter.Rewrite<BoundExpression>(call);
        Assert.Same(call.Type, Assert.IsType<BoundUnitExpression>(result).UnitType);
        Assert.Null(result.Symbol);
    }

    [Fact]
    public void OmittedConditionalCallSkipsOuterReceiverAndInvocationChildren()
    {
        var method = Method();
        var receiver = new BoundInvocationExpression(method, new UnvisitableArguments());
        var call = new BoundInvocationExpression(method, new UnvisitableArguments(), receiver,
            isConditionallyOmitted: true);
        var access = new BoundConditionalAccessExpression(receiver, call, Named("Runtime", "Example.Access"));
        var result = ConditionalCallRewriter.Rewrite<BoundExpression>(access);
        Assert.Same(call.Type, Assert.IsType<BoundUnitExpression>(result).UnitType);
    }

    [Fact]
    public void RetainedInvocationRewritesChildrenWithoutLosingCallMetadata()
    {
        var method = Method();
        var omitted = new BoundInvocationExpression(method, [], isConditionallyOmitted: true);
        var argument = new BoundDefaultValueExpression(method.ReturnType);
        var call = new BoundInvocationExpression(method, [omitted, argument], omitted, omitted,
            requiresReceiverAddress: true);
        var result = Assert.IsType<BoundInvocationExpression>(ConditionalCallRewriter.Rewrite<BoundExpression>(call));
        Assert.Same(method, result.Method);
        Assert.True(result.RequiresReceiverAddress);
        Assert.False(result.IsConditionallyOmitted);
        Assert.IsType<BoundUnitExpression>(result.Receiver);
        Assert.IsType<BoundUnitExpression>(result.ExtensionReceiver);
        Assert.Collection(result.Arguments,
            first => Assert.IsType<BoundUnitExpression>(first),
            second => Assert.Same(argument, second));
    }

    [Fact]
    public void RetainedConditionalAccessPreservesItsReceiverAndType()
    {
        var method = Method();
        var receiver = new BoundDefaultValueExpression(method.ReturnType);
        var call = new BoundInvocationExpression(method, []);
        var type = Named("Runtime", "Example.Access");
        var access = new BoundConditionalAccessExpression(receiver, call, type);
        var result = Assert.IsType<BoundConditionalAccessExpression>(ConditionalCallRewriter.Rewrite<BoundExpression>(access));
        Assert.Same(receiver, result.Receiver);
        Assert.Same(type, result.Type);
        Assert.Same(method, Assert.IsType<BoundInvocationExpression>(result.WhenNotNull).Method);
    }

    [Fact]
    public void NonInvocationConditionalAccessStillRewritesItsChildren()
    {
        var method = Method();
        var omitted = new BoundInvocationExpression(method, [], isConditionallyOmitted: true);
        var whenNotNull = new BoundDefaultValueExpression(method.ReturnType);
        var access = new BoundConditionalAccessExpression(omitted, whenNotNull, method.ReturnType);
        var result = Assert.IsType<BoundConditionalAccessExpression>(ConditionalCallRewriter.Rewrite<BoundExpression>(access));
        Assert.IsType<BoundUnitExpression>(result.Receiver);
        Assert.Same(whenNotNull, result.WhenNotNull);
    }

    [Fact]
    public void StatementRootsPreserveScopeAndDisposalMetadata()
    {
        var method = Method();
        var call = new BoundInvocationExpression(method, [], isConditionallyOmitted: true);
        var local = Make<LocalSymbol>(SymbolKind.Local, "resource");
        var block = new BoundBlockStatement([new BoundExpressionStatement(call)], [local], introduceILScope: false);
        var result = ConditionalCallRewriter.Rewrite(block);
        Assert.False(result.IntroduceILScope);
        Assert.Same(local, Assert.Single(result.LocalsToDispose));
        var statement = Assert.IsType<BoundExpressionStatement>(Assert.Single(result.Statements));
        Assert.IsType<BoundUnitExpression>(statement.Expression);
    }

    private static MethodSymbol Method()
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, "Call");
        var type = Named("Runtime", "Example.Unit");
        Set(type, "get_IsValueType", true);
        Set(method, "get_ReturnType", type);
        Set(method, "GetReturnTypeAttributes", ImmutableArray<AttributeData>.Empty);
        return method;
    }

    private sealed class UnvisitableArguments : IEnumerable<BoundExpression>
    {
        public IEnumerator<BoundExpression> GetEnumerator()
            => throw new InvalidOperationException("Omitted-call children must not be visited.");

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
