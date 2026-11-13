using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "NeoCLR")]
public sealed class FunctionReturnProjectionContractTests
{
    [Fact]
    public void UnresolvedProjectionPreservesIdentityOrReplacesOnlyItsOperand()
    {
        var operand = Named("Types", "Example.T");
        var replacement = Named("Types", "Example.U");
        var unit = Named("Types", "Example.Unit");
        var projection = new FunctionReturnTypeSymbolImplementation(operand, unit);
        Assert.Same(projection, projection.Substitute(type => type));
        var changed = Assert.IsType<FunctionReturnTypeSymbolImplementation>(projection.Substitute(_ => replacement));
        Assert.Same(replacement, changed.FunctionType);
        Assert.Same(operand, projection.FunctionType);
        Assert.Same(changed, changed.OriginalDefinition);
        Assert.Equal(TypeKind.FunctionReturn, changed.TypeKind);
        Assert.False(changed.IsReferenceType);
        Assert.False(changed.IsValueType);
        Assert.Null(changed.BaseType);
        Assert.Empty(changed.Interfaces);
        Assert.Empty(changed.AllInterfaces);
        Assert.Empty(changed.GetMembers());
        Assert.Empty(changed.GetMembers("Invoke"));
        Assert.Null(changed.LookupType("Nested"));
        Assert.False(changed.IsMemberDefined("Member", out var member));
        Assert.Null(member);
        Assert.Equal("projection", changed.Accept(new Visitor()));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CallableIdentityResolvesResultAndMapsVoidToUnit(bool returnsVoid)
    {
        var operand = Named("Types", "Example.F");
        var unit = Named("Types", "Example.Unit");
        var result = Named("Types", "Example.Result");
        Set(result, "get_SpecialType", returnsVoid ? SpecialType.System_Void : SpecialType.None);
        var callable = Make<CallableIdentity>(SymbolKind.Type, "Callable", TypeKind.Delegate);
        Set(callable, "get_ReturnType", result);
        var projection = new FunctionReturnTypeSymbolImplementation(operand, unit);
        var calls = 0;
        var resolved = projection.Substitute(type =>
        {
            Assert.Same(operand, type);
            calls++;
            return callable;
        });
        Assert.Equal(1, calls);
        Assert.Same(returnsVoid ? unit : result, resolved);
    }

    internal interface CallableIdentity : TypeSymbol, IStructuralFunctionIdentity { }

    private sealed class Visitor : SymbolVisitor<string>
    {
        public override string DefaultVisit(Symbol symbol) => "projection";
    }
}
