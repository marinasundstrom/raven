using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroExecutionValueTests
{
    [Fact]
    public void ResultFactories_NormalizeNullAndKeepAlternativesExclusive()
    {
        var attached = MacroExecutionResult.Attached(null);
        Assert.Same(MacroExpansionResult.Empty, attached.AttachedResult);
        Assert.Null(attached.FreestandingResult);
        var freestanding = MacroExecutionResult.Freestanding(null);
        Assert.Same(FreestandingMacroExpansionResult.Empty, freestanding.FreestandingResult);
        Assert.Null(freestanding.AttachedResult);
        Assert.NotEqual(attached, freestanding);
    }

    [Fact]
    public void ResultEquality_UsesWrappedMutableResultIdentity()
    {
        var payload = new MacroExpansionResult();
        var first = MacroExecutionResult.Attached(payload);
        var second = MacroExecutionResult.Attached(payload);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, MacroExecutionResult.Attached(new MacroExpansionResult()));
        payload.ReplacementDeclaration = SyntaxFactory.ParseExpression("42");
        Assert.Equal(first, second);
        Assert.Same(payload, first.AttachedResult);
        Assert.Null(typeof(MacroExecutionResult).GetProperty("AttachedResult")!.SetMethod);
    }

    [Fact]
    public void ArgumentWrapper_PreservesOrdinalNameAndArgumentIdentity()
    {
        var invocation = Assert.IsType<InvocationExpressionSyntax>(SyntaxFactory.ParseExpression("Call(value: 42)"));
        var argument = new MacroArgument(invocation.ArgumentList.Arguments[0], new UnusedServices());
        var first = new MacroExecutionArgument(2, argument);
        var second = new MacroExecutionArgument(2, argument);
        Assert.Equal(2, first.Ordinal);
        Assert.Equal("value", first.Name);
        Assert.Same(argument, first.Argument);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, new MacroExecutionArgument(3, argument));
        Assert.NotEqual(first, new MacroExecutionArgument(2,
            new MacroArgument(invocation.ArgumentList.Arguments[0], new UnusedServices())));
        Assert.Null(typeof(MacroExecutionArgument).GetProperty("Ordinal")!.SetMethod);
    }

    private sealed class UnusedServices : MacroArgumentServices
    {
        public TypeSymbol GetSpecialType(SpecialType specialType) => throw new InvalidOperationException();
        public TypeSymbol? GetArgumentType(ExpressionSyntax expression) => throw new InvalidOperationException();
        public TypeSymbol? ResolveConstraint(Type runtimeType) => throw new InvalidOperationException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => throw new InvalidOperationException();
    }
}
