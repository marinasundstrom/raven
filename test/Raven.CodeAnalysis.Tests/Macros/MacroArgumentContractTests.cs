using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroArgumentContractTests
{
    [Theory]
    [InlineData("42", SpecialType.System_Int32, 42)]
    [InlineData("true", SpecialType.System_Boolean, true)]
    [InlineData("\"hello\"", SpecialType.System_String, "hello")]
    public void ConstantAndSemanticTypeAreIndependentLazyValues(string text, SpecialType expectedType, object expectedValue)
    {
        var expression = SyntaxFactory.ParseExpression(text);
        var services = new Services();
        var argument = new MacroArgument(expression, services);
        Assert.Same(expression, argument.Syntax);
        Assert.Same(expression, argument.Expression);
        Assert.False(argument.IsNamed);
        Assert.Equal(0, services.SpecialCalls);
        Assert.Equal(0, services.SemanticCalls);
        Assert.Equal(expectedValue, argument.Value);
        Assert.True(argument.HasValue);
        Assert.Equal(TypedConstantKind.Primitive, argument.ValueKind);
        Assert.Same(services.Result, argument.Type);
        Assert.Equal(expectedType, services.LastSpecialType);
        Assert.Equal(1, services.SpecialCalls);
        Assert.Equal(0, services.SemanticCalls);
        Assert.Same(services.Result, argument.SemanticType);
        Assert.Same(services.Result, argument.SemanticType);
        Assert.Same(expression, services.LastExpression);
        Assert.Equal(1, services.SemanticCalls);
    }

    [Theory]
    [InlineData("null", TypedConstantKind.Null, true)]
    [InlineData("unknown", TypedConstantKind.Error, false)]
    public void NullAndUnsupportedConstantsDoNotRequestSpecialTypes(string text, TypedConstantKind kind, bool hasValue)
    {
        var services = new Services { Result = null };
        var argument = new MacroArgument(SyntaxFactory.ParseExpression(text), services);
        Assert.Equal(kind, argument.ValueKind);
        Assert.Equal(hasValue, argument.HasValue);
        Assert.Null(argument.Value);
        Assert.Null(argument.Type);
        Assert.Equal(0, services.SpecialCalls);
        Assert.Null(argument.SemanticType);
        Assert.Null(argument.SemanticType);
        Assert.Equal(1, services.SemanticCalls);
    }

    [Fact]
    public void NamedArgumentPreservesAuthoredNodesAndServices()
    {
        var invocation = Assert.IsType<InvocationExpressionSyntax>(SyntaxFactory.ParseExpression("Run(answer: 42)"));
        var syntax = Assert.Single(invocation.ArgumentList.Arguments);
        var services = new Services();
        var argument = new MacroArgument(syntax, services);
        Assert.True(argument.IsNamed);
        Assert.Equal("answer", argument.Name);
        Assert.Same(syntax, argument.Syntax);
        Assert.Same(syntax.Expression, argument.Expression);
        Assert.Same(services, argument.Services);
        Assert.Equal(42, argument.Value);
    }

    [Fact]
    public void LazyFailuresAreCachedAndDoNotForceTheOtherValue()
    {
        var failure = new InvalidOperationException("binding failed");
        var services = new Services { Failure = failure };
        var argument = new MacroArgument(SyntaxFactory.ParseExpression("42"), services);
        Assert.Same(failure, Assert.Throws<InvalidOperationException>(() => argument.SemanticType));
        Assert.Same(failure, Assert.Throws<InvalidOperationException>(() => argument.SemanticType));
        Assert.Equal(1, services.SemanticCalls);
        Assert.Equal(42, argument.Value);
    }

    [Fact]
    public void ConstructionRejectsNullSyntaxAndServices()
    {
        var services = new Services();
        Assert.Equal("syntax", Assert.Throws<ArgumentNullException>(() => new MacroArgument((ExpressionSyntax)null!, services)).ParamName);
        Assert.Equal("syntax", Assert.Throws<ArgumentNullException>(() => new MacroArgument((ArgumentSyntax)null!, services)).ParamName);
        Assert.Equal("services", Assert.Throws<ArgumentNullException>(() => new MacroArgument(SyntaxFactory.ParseExpression("42"), (MacroArgumentServices)null!)).ParamName);
    }

    private sealed class Services : MacroArgumentServices
    {
        public TypeSymbol? Result { get; set; } = MetadataSymbolFixture.Named("Types", "Example.Value");
        public Exception? Failure { get; set; }
        public int SpecialCalls { get; private set; }
        public int SemanticCalls { get; private set; }
        public SpecialType LastSpecialType { get; private set; }
        public ExpressionSyntax? LastExpression { get; private set; }

        public TypeSymbol GetSpecialType(SpecialType specialType)
        {
            SpecialCalls++;
            LastSpecialType = specialType;
            return Result!;
        }

        public TypeSymbol? GetArgumentType(ExpressionSyntax expression)
        {
            SemanticCalls++;
            LastExpression = expression;
            if (Failure is { } failure)
                throw failure;
            return Result;
        }

        public TypeSymbol? ResolveConstraint(Type runtimeType) => throw new InvalidOperationException("Construction must not resolve constraints");

        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => throw new InvalidOperationException("Construction must not classify conversions");
    }
}
