using System;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroParameterFoundationTests
{
    [Theory]
    [InlineData(typeof(bool), "bool")]
    [InlineData(typeof(byte), "byte")]
    [InlineData(typeof(sbyte), "sbyte")]
    [InlineData(typeof(short), "short")]
    [InlineData(typeof(ushort), "ushort")]
    [InlineData(typeof(int), "int")]
    [InlineData(typeof(uint), "uint")]
    [InlineData(typeof(long), "long")]
    [InlineData(typeof(ulong), "ulong")]
    [InlineData(typeof(float), "float")]
    [InlineData(typeof(double), "double")]
    [InlineData(typeof(decimal), "decimal")]
    [InlineData(typeof(char), "char")]
    [InlineData(typeof(string), "string")]
    [InlineData(typeof(object), "object")]
    [InlineData(typeof(int?), "int?")]
    [InlineData(typeof(ExpressionSyntax<int?>), "ExpressionSyntax<int?>")]
    [InlineData(typeof(DateTime), "DateTime")]
    public void DisplayPreservesLanguageNames(Type type, string expected)
    {
        Assert.Equal(expected, MacroParameterTypeDisplay.Format(type));
    }

    [Fact]
    public void ConstraintRequiresTheExactFacadeDefinition()
    {
        Assert.True(MacroExpressionTypeFacts.TryGetConstraint(typeof(ExpressionSyntax<int[]>), out var constraint));
        Assert.Equal(typeof(int[]), constraint);
        Assert.False(MacroExpressionTypeFacts.TryGetConstraint(typeof(Tuple<int>), out constraint));
        Assert.Null(constraint);
        Assert.False(MacroExpressionTypeFacts.TryGetConstraint(typeof(int), out constraint));
        Assert.Null(constraint);
    }

    [Fact]
    public void DescriptorDefaultsAndValueEqualityPreserveAllFields()
    {
        var first = new MacroParameterDescriptor("count", typeof(int), MacroParameterRole.Value, 0, false, 42);
        var second = new MacroParameterDescriptor("count", typeof(int), MacroParameterRole.Value, 0, false, 42);
        Assert.Equal(first, second);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Equal("int", first.TypeDisplayName);
        Assert.Null(first.DefaultValueDisplay);
        var custom = new MacroParameterDescriptor("count", typeof(int), MacroParameterRole.Value, 0, false, 42, "Count", "forty-two");
        Assert.NotEqual(first, custom);
        Assert.Equal("Count", custom.TypeDisplayName);
        Assert.Equal("forty-two", custom.DefaultValueDisplay);
        foreach (var property in typeof(MacroParameterDescriptor).GetProperties())
            Assert.Null(property.SetMethod);
    }
    [Fact]
    public void FacadeConstructionPreservesSyntaxAndSemanticType()
    {
        var syntax = Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("42");
        var services = new FacadeServices();
        var argument = new MacroArgument(syntax, services);
        var facade = Assert.IsType<ExpressionSyntax<int>>(MacroExpressionTypeFacts.CreateFacade(typeof(ExpressionSyntax<int>), argument));
        Assert.Same(syntax, facade.Syntax);
        Assert.Same(services.Type, facade.Type);
        Assert.Throws<InvalidOperationException>(() => MacroExpressionTypeFacts.CreateFacade(typeof(string), argument));
    }

    private sealed class FacadeServices : MacroArgumentServices
    {
        public TypeSymbol Type { get; } = Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture.Named("Types", "Example.Value");
        public TypeSymbol GetSpecialType(SpecialType specialType) => throw new InvalidOperationException();
        public TypeSymbol? GetArgumentType(Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => Type;
        public TypeSymbol? ResolveConstraint(Type runtimeType) => throw new InvalidOperationException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => throw new InvalidOperationException();
    }

}
