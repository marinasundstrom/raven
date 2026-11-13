using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroFactsContractTests
{
    [Fact]
    public void ReflectedParametersPreserveDefaultsAndExcludeInjectedInputs()
    {
        var macro = new Reflected();
        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.Same(macro, descriptor.Definition);
        Assert.Equal(MacroApplicationKind.Freestanding, descriptor.ApplicationKind);
        Assert.Equal(MacroKind.Freestanding, MacroFacts.GetKind(macro));
        Assert.Equal(MacroInvocationTargets.Expression, descriptor.InvocationTargets);
        Assert.Equal(MacroCarrierKinds.Parenthesized | MacroCarrierKinds.TokenTree, descriptor.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.Required, descriptor.BodyRequirement);
        Assert.True(descriptor.HasTokenBody);
        Assert.True(MacroFacts.AcceptsArguments(macro));
        Assert.Equal(new[] { "expression", "count" }, descriptor.Parameters.Select(p => p.Name));
        Assert.Equal(new[] { MacroParameterRole.SyntaxInput, MacroParameterRole.Value }, descriptor.Parameters.Select(p => p.Role));
        Assert.Equal(new[] { 0, 1 }, descriptor.Parameters.Select(p => p.Ordinal));
        Assert.True(descriptor.Parameters[0].IsRequired);
        Assert.False(descriptor.Parameters[1].IsRequired);
        Assert.Equal(7, descriptor.Parameters[1].DefaultValue);
        Assert.Equal("7", descriptor.Parameters[1].DefaultValueDisplay);
        Assert.Equal(MacroTarget.None, descriptor.AttachmentTargets);
    }

    [Fact]
    public void DeclarationInputSelectsDeclarationCarrierWithoutInvocationArgument()
    {
        var descriptor = MacroFacts.GetDescriptor(new Declaration());
        Assert.True(descriptor.HasDeclarationInput);
        Assert.Equal(MacroCarrierKinds.Declaration, descriptor.CarrierKinds);
        Assert.Empty(descriptor.Parameters);
        Assert.False(descriptor.AcceptsArguments);
        Assert.Equal(MacroBodyRequirement.None, descriptor.BodyRequirement);
    }

    [Fact]
    public void AttachedMethodUsesSyntaxTargetAndIgnoresFreestandingPositions()
    {
        var macro = new Attached();
        Assert.Equal(MacroApplicationKind.Attached, MacroFacts.GetApplicationKind(macro));
        Assert.Equal(MacroKind.AttachedDeclaration, MacroFacts.GetKind(macro));
        Assert.Equal(MacroTarget.Type, MacroFacts.GetTargets(macro));
        Assert.Equal(MacroInvocationTargets.None, MacroFacts.GetInvocationTargets(macro));
        Assert.Empty(MacroFacts.GetParameters(macro));
    }

    [Fact]
    public void CarrierAttributePrecedenceRetainsTypeMethodAndPropertyRules()
    {
        var type = MacroFacts.GetDescriptor(new TypeCarrier());
        Assert.Equal(MacroCarrierKinds.ExpressionHeader, type.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.Optional, type.BodyRequirement);
        Assert.True(type.AcceptsArguments);
        Assert.Equal(typeof(int), type.ExpressionResultType);
        var method = MacroFacts.GetDescriptor(new MethodCarrier());
        Assert.Equal(MacroCarrierKinds.TokenTree, method.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.Required, method.BodyRequirement);
        var properties = MacroFacts.GetDescriptor(new PropertyCarrier());
        Assert.Equal(MacroCarrierKinds.Declaration, properties.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.None, properties.BodyRequirement);
        Assert.True(properties.AcceptsArguments);
    }

    [Fact]
    public void ExecutorParametersAreSortedAndTargetsUseExplicitThenInferredThenFallback()
    {
        var macro = new Executor
        {
            Parameters = [Parameter("second", MacroParameterSource.Value, 1),
                Parameter("target", MacroParameterSource.AttachedTarget, -1, typeof(PropertyDeclarationSyntax)),
                Parameter("first", MacroParameterSource.SyntaxInput, 0)]
        };
        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.Equal(new[] { "first", "second" }, descriptor.Parameters.Select(p => p.Name));
        Assert.All(descriptor.Parameters, p => Assert.Null(p.DefaultValue));
        Assert.Equal("default", descriptor.Parameters[0].DefaultValueDisplay);
        Assert.Equal(MacroTarget.Property, descriptor.AttachmentTargets);
        macro.Targets = MacroTarget.Method;
        Assert.Equal(MacroTarget.Method, MacroFacts.GetTargets(macro));
        macro.Targets = MacroTarget.None;
        macro.Parameters = [];
        Assert.Equal(MethodMacroFacts.AllTargets, MacroFacts.GetTargets(macro));
        macro.ApplicationKind = MacroApplicationKind.Freestanding;
        Assert.Equal(MacroTarget.None, MacroFacts.GetTargets(macro));
    }

    [Fact]
    public void ExecutorCapabilityFlagsDriveNormalizedDescriptor()
    {
        var macro = new Executor
        {
            ApplicationKind = MacroApplicationKind.Freestanding,
            HasTokenBody = true,
            Parameters = [Parameter("declaration", MacroParameterSource.DeclarationInput, -1)]
        };
        var descriptor = MacroFacts.GetDescriptor(macro);
        Assert.True(descriptor.HasDeclarationInput);
        Assert.True(descriptor.HasTokenBody);
        Assert.Equal(MacroCarrierKinds.Declaration, descriptor.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.Required, descriptor.BodyRequirement);
        Assert.Empty(descriptor.Parameters);
    }

    [Fact]
    public void InvalidDefinitionsRetainTryDefaultsAndThrowingContracts()
    {
        var macro = new Unclassified();
        Assert.False(MacroFacts.TryGetKind(macro, out var kind));
        Assert.Equal(default, kind);
        Assert.False(MacroFacts.TryGetApplicationKind(macro, out var application));
        Assert.Equal(default, application);
        Assert.Empty(MacroFacts.GetParameters(macro));
        Assert.Equal(MacroTarget.None, MacroFacts.GetTargets(macro));
        Assert.Equal("macro", Assert.Throws<ArgumentException>(() => MacroFacts.GetKind(macro)).ParamName);
        Assert.Equal("macro", Assert.Throws<ArgumentException>(() => MacroFacts.GetDescriptor(macro)).ParamName);
        Assert.Equal("macro", Assert.Throws<ArgumentNullException>(() => MacroFacts.GetDescriptor(null!)).ParamName);
        Assert.Throws<ArgumentNullException>(() => MacroFacts.TryGetKind(null!, out _));
        Assert.Throws<ArgumentNullException>(() => MacroFacts.GetParameters(null!));
        Assert.Throws<ArgumentNullException>(() => MacroFacts.GetTargets(null!));
    }

    private static MacroExecutorParameter Parameter(string name, MacroParameterSource source, int ordinal, Type? type = null)
        => new(name, type ?? typeof(int), "display", source, 0, ordinal, false, "default");

    public sealed class Reflected : IMacroDefinition
    {
        public object? Expand(TokenTreeMacroContext context, IMacroTokenStream stream, ExpressionSyntax<int> expression, int count = 7) => null;
    }
    public sealed class Declaration : IMacroDefinition
    {
        public object? Expand(FreestandingMacroDeclarationSyntax declaration) => null;
    }
    public sealed class Attached : IMacroDefinition
    {
        public MacroExpansionResult Expand(ClassDeclarationSyntax target) => MacroExpansionResult.Empty;
    }
    [MacroCarrier(MacroCarrierKinds.ExpressionHeader, MacroBodyRequirement.Optional)]
    public sealed class TypeCarrier : IMacroDefinition
    {
        public Type ExpressionResultType => typeof(int);
        public MacroCarrierKinds CarrierKinds => MacroCarrierKinds.Declaration;
        public MacroBodyRequirement BodyRequirement => MacroBodyRequirement.Required;
        [MacroCarrier(MacroCarrierKinds.TokenTree, MacroBodyRequirement.Required)]
        public object? Expand() => null;
    }
    public sealed class MethodCarrier : IMacroDefinition
    {
        [MacroCarrier(MacroCarrierKinds.TokenTree)]
        public object? Expand(IMacroTokenStream stream) => null;
    }
    public sealed class PropertyCarrier : IMacroDefinition
    {
        public MacroCarrierKinds CarrierKinds => MacroCarrierKinds.Declaration;
        public MacroBodyRequirement BodyRequirement => MacroBodyRequirement.None;
        public bool AcceptsArguments => true;
        public object? Expand(IMacroTokenStream stream) => null;
    }
    public sealed class Executor : IMacroExecutor
    {
        public MacroApplicationKind ApplicationKind { get; set; } = MacroApplicationKind.Attached;
        public ImmutableArray<MacroExecutorParameter> Parameters { get; set; } = [];
        public MacroTarget Targets { get; set; }
        public bool HasTokenBody { get; set; }
        public MacroExecutionResult Expand(MacroExecutionContext context) => MacroExecutionResult.Attached(null);
    }
    public sealed class Unclassified : IMacroDefinition { }
}
