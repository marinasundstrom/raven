using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundCallableContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MethodGroupRejectsMissingCandidates(bool defaultArray)
    {
        var type = Named("A", "Example.MethodGroup");
        var methods = defaultArray ? default : ImmutableArray<MethodSymbol>.Empty;
        var error = Assert.Throws<ArgumentException>(() => new BoundMethodGroupExpression(null, methods, type));
        Assert.Equal("methods", error.ParamName);
    }

    [Fact]
    public void DelegateCreationRejectsNullMethodGroupBeforeReadingIt()
    {
        var type = Named("A", "Example.Delegate");
        var error = Assert.Throws<ArgumentNullException>(() => new BoundDelegateCreationExpression(null!, type));
        Assert.Equal("methodGroup", error.ParamName);
    }

    [Fact]
    public void MethodGroupInfersItsDelegateOnceAndPreservesSelection()
    {
        var type = Named("A", "Example.Delegate");
        var method = Make<MethodSymbol>(SymbolKind.Method, "Run");
        var calls = 0;
        var group = new BoundMethodGroupExpression(null, ImmutableArray.Create(method), type, () => { calls++; return type; });
        Assert.Same(method, group.SelectedMethod);
        Assert.Equal(0, calls);
        var creation = new BoundDelegateCreationExpression(group, type);
        Assert.Same(type, creation.GetConvertedType());
        Assert.Same(type, group.GetConvertedType());
        Assert.Equal(1, calls);
        Assert.Same(method, creation.Method);
        Assert.Null(creation.Receiver);
        Assert.Same(creation, creation.Update(group, type));
    }

    [Fact]
    public void MultipleCandidatesRemainUnselectedAndNullInferenceIsCached()
    {
        var type = Named("A", "Example.Delegate");
        var first = Make<MethodSymbol>(SymbolKind.Method, "First");
        var second = Make<MethodSymbol>(SymbolKind.Method, "Second");
        var calls = 0;
        var group = new BoundMethodGroupExpression(null, ImmutableArray.Create(first, second), type, () => { calls++; return null; });
        Assert.Null(group.SelectedMethod);
        Assert.Null(group.DelegateType);
        Assert.Null(group.DelegateType);
        Assert.Equal(1, calls);
        var creation = new BoundDelegateCreationExpression(group, type);
        Assert.Same(type, creation.GetConvertedType());
        Assert.Same(type, creation.DelegateType);
        Assert.Equal(2, creation.Methods.Length);
    }

    [Fact]
    public void SelectedCandidateAndReceiverSurviveDelegateCreation()
    {
        var type = Named("A", "Example.Delegate");
        var first = Make<MethodSymbol>(SymbolKind.Method, "First");
        var selected = Make<MethodSymbol>(SymbolKind.Method, "Second");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, type);
        var methods = ImmutableArray.Create(first, selected);
        var group = new BoundMethodGroupExpression(receiver, methods, type, selectedMethod: selected);
        Assert.Same(selected, group.SelectedMethod);
        Assert.Same(group, group.Update(receiver, methods, type, null, selected, BoundExpressionReason.None));
        var creation = new BoundDelegateCreationExpression(group, type);
        Assert.Same(selected, creation.Symbol);
        Assert.Same(receiver, creation.Receiver);
        var walker = new RecordingWalker();
        walker.Visit(creation);
        Assert.Same(receiver, Assert.Single(walker.Values));
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Values { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add(node);
    }
}
