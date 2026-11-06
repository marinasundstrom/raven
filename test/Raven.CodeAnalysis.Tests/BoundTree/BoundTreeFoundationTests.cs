using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundTreeFoundationTests
{
    [Fact]
    public void LiteralAndRequiredResultPreserveTheirTypeContractsAndUpdateIdentity()
    {
        var type = Named("A", "Example.Natural");
        var converted = Named("A", "Example.Converted");
        object value = 123;
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type, converted);
        Assert.Same(type, literal.GetNaturalType());
        Assert.Same(converted, literal.GetConvertedType());
        Assert.Null(literal.Symbol);
        Assert.Equal(BoundExpressionReason.None, literal.Reason);
        Assert.Same(literal, literal.Update(literal.Kind, value, type, converted));
        object equalButDistinct = 123;
        var updated = literal.Update(literal.Kind, equalButDistinct, type, converted);
        Assert.NotSame(literal, updated);
        Assert.Same(equalButDistinct, updated.Value);

        var required = literal.RequireValue();
        Assert.Same(type, required.Type);
        Assert.Same(literal, required.Operand);
        Assert.Same(required, required.Update(literal));
        Assert.Same(updated, required.Update(updated).Operand);
        var statement = new BoundExpressionStatement(literal);
        Assert.Same(statement, statement.Update(literal));
        Assert.Same(updated, statement.Update(updated).Expression);
    }

    [Fact]
    public void MemberAndFieldAccessPreserveReceiverSymbolAndReason()
    {
        var type = Named("A", "Example.Element");
        var field = Make<FieldSymbol>(SymbolKind.Field, "Item");
        Set(field, "get_Type", type);
        var receiver = new BoundDefaultValueExpression(type);
        var access = new BoundFieldAccess(receiver, field, BoundExpressionReason.Inaccessible);
        Assert.Same(type, access.Type);
        Assert.Same(field, access.Symbol);
        Assert.Same(receiver, access.Receiver);
        Assert.Equal(BoundExpressionReason.Inaccessible, access.Reason);
        Assert.Null(new BoundFieldAccess(field).Receiver);
        var member = new BoundMemberAccessExpression(receiver, field);
        Assert.Same(type, member.Type);
        Assert.Same(member, member.Update(receiver, field, BoundExpressionReason.None));
        Assert.Same(field, new BoundExpressionStatement(member).Symbol);
    }

    [Fact]
    public void VisitorsDispatchFromNodeAndCategoryEntryPoints()
    {
        var type = Named("A", "Example.Value");
        var pattern = new BoundDiscardPattern(type);
        var designator = new BoundDiscardDesignator(type);
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, true, type);
        var visitor = new CountingVisitor();
        visitor.Visit(pattern);
        visitor.VisitPattern(pattern);
        visitor.Visit(designator);
        visitor.VisitDesignator(designator);
        visitor.Visit(literal);
        visitor.VisitExpression(literal);
        Assert.Equal((2, 2, 2), (visitor.Patterns, visitor.Designators, visitor.Literals));
        var returning = new ReturningVisitor();
        Assert.Equal("pattern", returning.Visit(pattern));
        Assert.Equal("designator", returning.Visit(designator));
        Assert.Equal("literal", returning.Visit(literal));
        Assert.Null(new BoundTreeVisitor<string>().Visit(literal));
    }

    private sealed class CountingVisitor : BoundTreeVisitor
    {
        internal int Patterns, Designators, Literals;
        public override void VisitDiscardPattern(BoundDiscardPattern node) => Patterns++;
        public override void VisitDiscardDesignator(BoundDiscardDesignator node) => Designators++;
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals++;
    }

    private sealed class ReturningVisitor : BoundTreeVisitor<string>
    {
        public override string VisitDiscardPattern(BoundDiscardPattern node) => "pattern";
        public override string VisitDiscardDesignator(BoundDiscardDesignator node) => "designator";
        public override string VisitLiteralExpression(BoundLiteralExpression node) => "literal";
    }
}
