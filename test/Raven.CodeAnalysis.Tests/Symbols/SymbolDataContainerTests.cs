using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SymbolDataContainerTests
{
    [Fact]
    public void AttributeDataPreservesArgumentStorageAndSyntaxReference()
    {
        var arguments = ImmutableArray.Create(TypedConstant.CreatePrimitive(null, 42));
        var named = ImmutableArray.Create(new KeyValuePair<string, TypedConstant>("Name", TypedConstant.CreatePrimitive(null, "value")));
        var tree = SyntaxTree.ParseText("class Example { }");
        var syntax = new SyntaxReference(tree, tree.GetRoot());
        // Symbol ownership is tested separately while the symbol implementation is ported.
        var data = new AttributeData(null!, null!, arguments, named, syntax);
        Assert.Equal(arguments, data.ConstructorArguments);
        Assert.Equal(named, data.NamedArguments);
        Assert.Same(syntax, data.ApplicationSyntaxReference);
        Assert.Equal(42, Assert.Single(data.ConstructorArguments).Value);
        Assert.Equal("Name", Assert.Single(data.NamedArguments).Key);
        Assert.Equal("value", data.NamedArguments[0].Value.Value);
    }

    [Fact]
    public void AttributeOccurrencesRetainReferenceIdentity()
    {
        var first = new AttributeData(null!, null!, [], [], null);
        var second = new AttributeData(null!, null!, [], [], null);
        Assert.True(first.Equals(first));
        Assert.False(first.Equals(second));
        Assert.True(typeof(AttributeData).IsSealed);
    }

    [Theory]
    [InlineData(MacroParameterSource.Value, 2, 1)]
    [InlineData(MacroParameterSource.Context, 0, null)]
    public void MacroBindingsPreservePositionsAndHaveValueEquality(MacroParameterSource source, int declaration, int? invocation)
    {
        var first = new MacroParameterBinding(null!, source, declaration, invocation);
        var second = new MacroParameterBinding(null!, source, declaration, invocation);
        Assert.Equal(source, first.Source);
        Assert.Equal(declaration, first.DeclarationOrdinal);
        Assert.Equal(invocation, first.InvocationArgumentOrdinal);
        Assert.IsAssignableFrom<IEquatable<MacroParameterBinding>>(first);
        Assert.NotSame(first, second);
        Assert.True(first.Equals(second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Contains(second, new HashSet<MacroParameterBinding> { first });
        Assert.False(first.Equals(null));
        var changed = first with { DeclarationOrdinal = declaration + 1 };
        Assert.NotEqual(first, changed);
        Assert.Equal(declaration, first.DeclarationOrdinal);
        var (parameter, valueSource, declarationPosition, invocationPosition) = first;
        Assert.Null(parameter);
        Assert.Equal(source, valueSource);
        Assert.Equal(declaration, declarationPosition);
        Assert.Equal(invocation, invocationPosition);
        Assert.Equal("Raven.CodeAnalysis.Macros.MacroParameterBinding", first.ToString());
        Assert.True(typeof(MacroParameterBinding).IsSealed);
    }
}
