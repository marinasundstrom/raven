using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceValueSymbolTests
{
    [Fact]
    public void ConcreteSymbolInheritanceMatchesBootstrapApi()
    {
        Assert.False(typeof(SourceLocalSymbol).IsSealed);
        Assert.False(typeof(SourceParameterSymbol).IsSealed);
        Assert.False(typeof(LabelSymbolImplementation).IsSealed);
        Assert.True(typeof(SourceFunctionValueSymbol).IsSealed);
        Assert.False(typeof(SourceFunctionValueSymbol).IsAbstract);
        Assert.Equal(typeof(SourceLocalSymbol), typeof(SourceFunctionValueSymbol).BaseType);
    }

    [Fact]
    public void LocalsPreserveFlagsConstantsIdentityAndVisitorDispatch()
    {
        var owner = Make<Symbol>(SymbolKind.Method, "Owner");
        var type = Make<TypeSymbol>(SymbolKind.Type, "int", TypeKind.Struct);
        var first = new SourceLocalSymbol("value", type, false, owner, null, null, [], [],
            isConst: true, constantValue: 42, scopedKind: ScopedKind.ScopedValue, isImplicitlyDeclared: true);
        var second = new SourceLocalSymbol("value", type, false, owner, null, null, [], []);
        Assert.Same(type, first.Type);
        Assert.Same(owner, first.ContainingSymbol);
        Assert.False(first.IsMutable);
        Assert.True(first.IsConst);
        Assert.True(first.IsImplicitlyDeclared);
        Assert.Equal(42, first.ConstantValue);
        Assert.Equal(ScopedKind.ScopedValue, first.ScopedKind);
        Assert.NotEqual(first, second);
        Assert.Equal("local:value", first.Accept(new Visitor()));
        Assert.Empty(first.GetAttributes());
        Assert.Null(second.ConstantValue);
    }

    [Fact]
    public void FunctionValuesRetainTargetMethodAndDispatchAsLocals()
    {
        var owner = Make<Symbol>(SymbolKind.Method, "Owner");
        var target = Make<MethodSymbol>(SymbolKind.Method, "Target");
        var value = new SourceFunctionValueSymbol("f", Make<TypeSymbol>(SymbolKind.Type, "Delegate"), true,
            owner, null, null, [], [], target);
        Assert.Same(target, value.TargetMethod);
        Assert.True(value.IsMutable);
        Assert.False(value.IsConst);
        Assert.Equal("local:f", value.Accept(new Visitor()));
    }

    [Theory]
    [InlineData(RefKind.None, false, ScopedKind.None)]
    [InlineData(RefKind.Ref, false, ScopedKind.None)]
    [InlineData(RefKind.Ref, true, ScopedKind.ScopedRef)]
    [InlineData(RefKind.Out, false, ScopedKind.ScopedRef)]
    [InlineData(RefKind.In, true, ScopedKind.None)]
    public void ParameterScopeInferenceMatchesRefKindAndType(RefKind kind, bool refLike, ScopedKind expected)
    {
        var type = Named("Test", "Value");
        Set(type, "get_IsRefLikeType", refLike);
        var parameter = Parameter(type, kind);
        Assert.Equal(kind, parameter.RefKind);
        Assert.Equal(expected, parameter.ScopedKind);
        Assert.Same(type, parameter.Type);
        Assert.Equal("parameter:value", parameter.Accept(new Visitor()));
    }

    [Fact]
    public void ExplicitScopeOverridesInferenceAndNullDefaultRemainsOptional()
    {
        var parameter = new SourceParameterSymbol("value", Make<TypeSymbol>(SymbolKind.Type, "Value"),
            Make<Symbol>(SymbolKind.Method, "Owner"), null, null, [], [], refKind: RefKind.Out,
            hasExplicitDefaultValue: true, explicitDefaultValue: null, isMutable: true, isVarParams: true,
            scopedKind: ScopedKind.ScopedValue, macroRole: MacroParameterRole.SyntaxInput, hasImplicitName: true);
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.True(parameter.IsOptional);
        Assert.Null(parameter.ExplicitDefaultValue);
        Assert.True(parameter.IsMutable);
        Assert.True(parameter.IsVarParams);
        Assert.True(parameter.HasImplicitName);
        Assert.Equal(MacroParameterRole.SyntaxInput, parameter.MacroRole);
        Assert.Equal(ScopedKind.ScopedValue, parameter.ScopedKind);
        Assert.Empty(parameter.GetAttributes());
    }

    [Fact]
    public void RefTypeParameterAllowsRefLikeScope()
    {
        var type = Make<TypeParameterSymbol>(SymbolKind.TypeParameter, "T", TypeKind.TypeParameter);
        Set(type, "get_ConstraintKind", TypeParameterConstraintKind.AllowByRefLike);
        Assert.Equal(ScopedKind.ScopedRef, Parameter(type, RefKind.Ref).ScopedKind);
    }

    [Theory]
    [InlineData("(left, right)", true)]
    [InlineData("_", true)]
    [InlineData("value", false)]
    public void ParameterBindingPatternsComeFromDeclaration(string name, bool hasPattern)
    {
        var syntax = SyntaxTree.ParseText($"func F({name}: int) {{}}").GetRoot()
            .DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameter = new SourceParameterSymbol("value", Make<TypeSymbol>(SymbolKind.Type, "int"),
            Make<Symbol>(SymbolKind.Method, "Owner"), null, null, [], [syntax.GetReference()]);
        Assert.Equal(hasPattern, parameter.BindingPattern is not null);
        Assert.False(parameter.IsOptional);
    }

    [Fact]
    public void LabelsRetainNameOwnershipAndVisitorDispatch()
    {
        var owner = Make<Symbol>(SymbolKind.Method, "Owner");
        var label = new LabelSymbolImplementation("done", owner, null, null, [], []);
        Assert.Equal(SymbolKind.Label, label.Kind);
        Assert.Equal("done", label.MetadataName);
        Assert.Same(owner, label.ContainingSymbol);
        Assert.True(label.CanBeReferencedByName);
        Assert.Equal("label:done", label.Accept(new Visitor()));
    }

    private static SourceParameterSymbol Parameter(TypeSymbol type, RefKind kind)
        => new("value", type, Make<Symbol>(SymbolKind.Method, "Owner"), null, null, [], [], refKind: kind);

    private sealed class Visitor : SymbolVisitor<string>
    {
        public override string VisitLocal(LocalSymbol symbol) => "local:" + symbol.Name;
        public override string VisitParameter(ParameterSymbol symbol) => "parameter:" + symbol.Name;
        public override string VisitLabel(LabelSymbol symbol) => "label:" + symbol.Name;
    }
}
