using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundReceiverFactsTests
{
    [Fact]
    public void BaseReceiverSurvivesOnlyTransparentWrappers()
    {
        var type = Named("A", "Example.Parent");
        var receiver = new BoundBaseExpression(type);
        var identity = new BoundConversionExpression(receiver, type, new Conversion(isImplicit: true, isIdentity: true));
        Assert.True(BoundReceiverFacts.IsBaseReceiver(receiver));
        Assert.True(BoundReceiverFacts.IsBaseReceiver(new BoundRequiredResultExpression(identity)));
        Assert.False(BoundReceiverFacts.IsBaseReceiver(new BoundConversionExpression(receiver, type, new Conversion(isImplicit: true, isReference: true))));
        Assert.False(BoundReceiverFacts.IsBaseReceiver(new BoundSelfExpression(type)));
        Assert.False(BoundReceiverFacts.IsBaseReceiver(null));
    }
}
