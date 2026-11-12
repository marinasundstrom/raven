using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundInvocationContractTests
{
    [Theory]
    [InlineData(false, "Call()")]
    [InlineData(true, "Owner.Call()")]
    public void DisplaySupportsMethodsWithOrWithoutContainingTypes(bool hasOwner, string expected)
    {
        var method = Method(ValueType());
        Set(method, "get_ContainingType", hasOwner ? Make<NamedTypeSymbol>(SymbolKind.Type, "Owner", TypeKind.Class) : null);
        Assert.Equal(expected, new BoundInvocationExpression(method, []).ToString());
    }

    [Fact]
    public void InvocationPreservesSelectionArgumentsReceiversAndFlags()
    {
        var type = ValueType();
        var method = Method(type);
        var receiver = Literal(type, 1);
        var extension = Literal(type, 2);
        BoundExpression[] arguments = [Literal(type, 3)];
        var node = new BoundInvocationExpression(method, arguments, receiver, extension, true, true);
        Assert.Same(method, node.Method);
        Assert.Same(method, node.Symbol);
        Assert.Same(type, node.Type);
        Assert.Same(arguments, node.Arguments);
        Assert.Same(receiver, node.Receiver);
        Assert.Same(extension, node.ExtensionReceiver);
        Assert.True(node.RequiresReceiverAddress);
        Assert.True(node.IsConditionallyOmitted);
        Assert.Equal(BoundExpressionReason.None, node.Reason);
    }

    [Fact]
    public void InvocationUsesMetadataReturnProjection()
    {
        var type = ValueType();
        var annotation = new NullableTypeSymbol(type, null, null, null, [], NullableAbiProjection.AnnotatedUnderlyingType);
        var method = Method(annotation);
        var node = new BoundInvocationExpression(method, []);
        Assert.Same(type, node.Type);
        Assert.Same(method, node.Symbol);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void WalkerVisitsReceiversOnceThenArguments(bool sharedReceiver)
    {
        var type = ValueType();
        var receiver = Literal(type, 1);
        var extension = sharedReceiver ? receiver : Literal(type, 2);
        var first = Literal(type, 3);
        var second = Literal(type, 4);
        var node = new BoundInvocationExpression(Method(type), [first, second], receiver, extension);
        var walker = new RecordingWalker();
        walker.Visit(node);
        Assert.Equal<BoundExpression>(sharedReceiver
            ? [receiver, first, second]
            : [receiver, extension, first, second], walker.Leaves);
    }

    [Fact]
    public void UpdatePreservesFlagsAndReusesUnchangedInvocation()
    {
        var type = ValueType();
        var method = Method(type);
        BoundExpression[] arguments = [Literal(type, 1)];
        var receiver = Literal(type, 2);
        var node = new BoundInvocationExpression(method, arguments, receiver, null, true, true);
        Assert.Same(node, node.Update(method, arguments, receiver, null, true, true));
        var replacement = Literal(type, 3);
        var changed = node.Update(method, [replacement], receiver, null, true, true);
        Assert.NotSame(node, changed);
        Assert.Same(replacement, Assert.Single(changed.Arguments));
        Assert.Same(receiver, changed.Receiver);
        Assert.True(changed.RequiresReceiverAddress);
        Assert.True(changed.IsConditionallyOmitted);
        Assert.Same(method, changed.Symbol);
        Assert.Same(type, changed.Type);
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        public List<BoundExpression> Leaves { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Leaves.Add(node);
    }

    private static NamedTypeSymbol ValueType()
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, "Value", TypeKind.Struct);
        Set(type, "get_IsValueType", true);
        Set(type, "get_BaseType", null);
        return type;
    }

    private static MethodSymbol Method(TypeSymbol type)
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, "Call");
        Set(method, "get_ReturnType", type);
        Set(method, "GetReturnTypeAttributes", ImmutableArray<AttributeData>.Empty);
        return method;
    }

    private static BoundLiteralExpression Literal(TypeSymbol type, int value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);
}
