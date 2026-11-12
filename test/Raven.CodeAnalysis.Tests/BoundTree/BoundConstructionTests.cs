using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundConstructionTests
{
    [Fact]
    public void ObjectConstructionVisitsReceiverArgumentsAndInitializerInOrder()
    {
        var type = Named("A", "Example.Widget");
        var constructor = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        Set(constructor, "get_ContainingType", type);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", type);
        var values = Enumerable.Range(1, 4)
            .Select(value => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type)).ToArray();
        var assignment = new BoundObjectInitializerAssignmentEntry(field, values[2]);
        Assert.Equal(SyntaxKind.EqualsToken, assignment.OperatorTokenKind);
        var content = new BoundObjectInitializerExpressionEntry(values[3]);
        BoundObjectInitializerEntry[] input = [assignment, content];
        var initializer = new BoundObjectInitializer(input);
        input[0] = content;
        Assert.Equal<BoundObjectInitializerEntry>([assignment, content], initializer.Entries);
        var expression = new BoundObjectCreationExpression(constructor, [values[1]], values[0], initializer);
        Assert.Same(type, expression.Type);
        Assert.Same(constructor, expression.Symbol);
        Assert.Same(expression, expression.Update(constructor, expression.Arguments, values[0], initializer));
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal<BoundLiteralExpression>(values, walker.Literals);
        Assert.EndsWith(" { ... }", expression.ToString());
    }

    [Fact]
    public void ConstructionWithoutAnOwnerReportsTheBrokenInvariantWhenPrinted()
    {
        var constructor = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        var expression = new BoundObjectCreationExpression(constructor, []);
        Assert.Null(expression.Type);
        var error = Assert.Throws<InvalidOperationException>(() => expression.ToString());
        Assert.Equal("An object constructor must have a containing type.", error.Message);
    }

    [Fact]
    public void PatternTestVisitsValueAndPatternChildren()
    {
        var type = Named("A", "Example.Value");
        var value = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, type);
        var pattern = new BoundDiscardPattern(type);
        var expression = new BoundIsPatternExpression(value, pattern, type, BoundExpressionReason.OtherError);
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Same(value, Assert.Single(walker.Literals));
        Assert.Same(pattern, Assert.Single(walker.Patterns));
        Assert.Same(type, expression.BooleanType);
        Assert.Equal(BoundExpressionReason.OtherError, expression.Reason);
    }

    [Fact]
    public void PointerMemberAccessRejectsNullMemberBeforeDispatchingItsKind()
    {
        var type = Named("A", "Example.Value");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0, type);
        var error = Assert.Throws<ArgumentNullException>(() => new BoundPointerMemberAccessExpression(receiver, null!));
        Assert.Equal("member", error.ParamName);
    }

    [Fact]
    public void PointerMemberAccessPreservesFieldIdentityAndVisitsReceiver()
    {
        var type = Named("A", "Example.Value");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0, type);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", type);
        var expression = new BoundPointerMemberAccessExpression(receiver, field);
        Assert.Same(type, expression.Type);
        Assert.Same(field, expression.Member);
        Assert.Same(field, expression.Symbol);
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Same(receiver, Assert.Single(walker.Literals));
        var error = Assert.Throws<ArgumentNullException>(() => new BoundPointerMemberAccessExpression(null!, field));
        Assert.Equal("pointerReceiver", error.ParamName);
    }

    [Theory]
    [InlineData(SymbolKind.Property)]
    [InlineData(SymbolKind.Method)]
    [InlineData(SymbolKind.Event)]
    public void PointerMemberAccessUsesTheMemberValueType(SymbolKind kind)
    {
        var type = Named("A", "Example.Value");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0, type);
        Symbol member = kind switch
        {
            SymbolKind.Property => Make<PropertySymbol>(kind, "Value"),
            SymbolKind.Method => Make<MethodSymbol>(kind, "Read"),
            _ => Make<EventSymbol>(kind, "Changed")
        };
        Set(member, kind == SymbolKind.Method ? "get_ReturnType" : "get_Type", type);
        var expression = new BoundPointerMemberAccessExpression(receiver, member);
        Assert.Same(type, expression.Type);
        Assert.Same(member, expression.Member);
        Assert.Same(member, expression.Symbol);
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Literals { get; } = [];
        internal List<BoundDiscardPattern> Patterns { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals.Add(node);
        public override void VisitDiscardPattern(BoundDiscardPattern node) => Patterns.Add(node);
    }
}
