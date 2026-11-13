using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroParameterBinderContractTests
{
    [Theory]
    [InlineData("Run()", "RAVM034")]
    [InlineData("Run(1, 2)", "RAVM036")]
    [InlineData("Run(other: 1)", "RAVM032")]
    [InlineData("Run(Count: 1)", "RAVM032")]
    [InlineData("Run(1, count: 2)", "RAVM033")]
    [InlineData("Run(count: 1, count: 2)", "RAVM033")]
    [InlineData("Run(\"no\")", "RAVM035")]
    [InlineData("Run(unknown)", "RAVM035")]
    [InlineData("Run(1)", null)]
    [InlineData("Run(count: 1)", null)]
    public void ArgumentValidationPreservesDiagnosticSelection(string text, string? diagnosticId)
    {
        var invocation = Assert.IsType<InvocationExpressionSyntax>(SyntaxFactory.ParseExpression(text));
        var arguments = invocation.ArgumentList.Arguments.Select(a => new MacroArgument(a, new Services())).ToArray();
        var parameters = ImmutableArray.Create(new MacroParameterDescriptor("count", typeof(int), MacroParameterRole.Value, 0, true, null));
        var diagnostics = new DiagnosticBag();
        Assert.Equal(diagnosticId is null, MacroParameterBinder.ValidateArguments("Run", invocation.GetLocation(), parameters, arguments, diagnostics));
        if (diagnosticId is null)
            Assert.Empty(diagnostics.AsEnumerable());
        else
        {
            var diagnostic = Assert.Single(diagnostics.AsEnumerable());
            Assert.Equal(diagnosticId, diagnostic.Descriptor.Id);
            Assert.Contains("Run", diagnostic.GetMessage());
        }
    }

    [Theory]
    [InlineData("42", typeof(long), true, 42L)]
    [InlineData("256", typeof(byte), false, null)]
    [InlineData("null", typeof(int?), true, null)]
    [InlineData("null", typeof(int), false, null)]
    [InlineData("null", typeof(string), true, null)]
    [InlineData("\"true\"", typeof(bool), true, true)]
    [InlineData("\"x\"", typeof(char), true, 'x')]
    [InlineData("\"bad\"", typeof(DateTime), false, null)]
    [InlineData("42", typeof(Guid), false, null)]
    public void ConstantConversionPreservesResultAndFailure(string source, Type target, bool success, object? expected)
    {
        var argument = new MacroArgument(SyntaxFactory.ParseExpression(source), new Services());
        Assert.Equal(success, MacroParameterBinder.TryConvertValue(argument, target, out var converted));
        Assert.Equal(expected, converted);
    }

    [Fact]
    public void SyntaxAndEnumConversionsPreserveTheirRuntimeShapes()
    {
        var syntax = SyntaxFactory.ParseExpression("1");
        var argument = new MacroArgument(syntax, new Services());
        Assert.True(MacroParameterBinder.TryConvertValue(argument, typeof(Raven.CodeAnalysis.Syntax.ExpressionSyntax), out var converted));
        Assert.Same(syntax, converted);
        Assert.True(MacroParameterBinder.TryConvertValue(argument, typeof(DayOfWeek), out converted));
        Assert.Equal(DayOfWeek.Monday, Assert.IsType<DayOfWeek>(converted));
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(false, false)]
    public void TypedExpressionValidationRequiresImplicitConversion(bool implicitConversion, bool success)
    {
        var services = new Services { Conversion = new Conversion(isImplicit: implicitConversion) };
        var argument = new MacroArgument(SyntaxFactory.ParseExpression("42"), services);
        var parameters = ImmutableArray.Create(new MacroParameterDescriptor("value", typeof(ExpressionSyntax<int>), MacroParameterRole.SyntaxInput, 0, true, null));
        var diagnostics = new DiagnosticBag();
        Assert.Equal(success, MacroParameterBinder.ValidateArguments("Run", argument.Syntax.GetLocation(), parameters, new[] { argument }, diagnostics));
        if (success)
            Assert.Empty(diagnostics.AsEnumerable());
        else
            Assert.Equal("RAVM037", Assert.Single(diagnostics.AsEnumerable()).Descriptor.Id);
        Assert.True(MacroParameterBinder.TryConvertValue(argument, typeof(ExpressionSyntax<int>), out var converted));
        Assert.Same(argument.Expression, Assert.IsType<ExpressionSyntax<int>>(converted).Syntax);
    }

    [Fact]
    public void OptionalArgumentsAndMissingSemanticTypesRemainDistinct()
    {
        var parameter = new MacroParameterDescriptor("count", typeof(int), MacroParameterRole.Value, 0, false, 42);
        var diagnostics = new DiagnosticBag();
        Assert.True(MacroParameterBinder.ValidateArguments("Run", Location.None, ImmutableArray.Create(parameter), Array.Empty<MacroArgument>(), diagnostics));
        Assert.Empty(diagnostics.AsEnumerable());
        var services = new Services { MissingType = true };
        var argument = new MacroArgument(SyntaxFactory.ParseExpression("unknown"), services);
        Assert.False(MacroParameterBinder.TryConvertValue(argument, typeof(ExpressionSyntax<int>), out var converted));
        Assert.Null(converted);
        var typedParameter = new MacroParameterDescriptor("value", typeof(ExpressionSyntax<int>), MacroParameterRole.SyntaxInput, 0, true, null);
        Assert.False(MacroParameterBinder.ValidateArguments("Run", argument.Syntax.GetLocation(), ImmutableArray.Create(typedParameter), new[] { argument }, diagnostics));
        var diagnostic = Assert.Single(diagnostics.AsEnumerable());
        Assert.Equal("RAVM037", diagnostic.Descriptor.Id);
        Assert.Contains("<unknown>", diagnostic.GetMessage());
    }

    private sealed class Services : MacroArgumentServices
    {
        private readonly TypeSymbol _type = MetadataSymbolFixture.Named("Types", "Example.Value");
        public Services()
        {
            MetadataSymbolFixture.Set(_type, "get_SpecialType", SpecialType.System_Int32);
            MetadataSymbolFixture.Set(_type, "get_Arity", 0);
        }

        public bool MissingType { get; init; }
        public Conversion Conversion { get; init; } = new Conversion(isImplicit: true);
        public TypeSymbol GetSpecialType(SpecialType specialType) => _type;
        public TypeSymbol? GetArgumentType(Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => MissingType ? null : _type;
        public TypeSymbol? ResolveConstraint(Type runtimeType) => _type;
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => Conversion;
    }
}
