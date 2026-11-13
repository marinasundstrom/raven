using System;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MethodMacroFactsContractTests
{
    [Theory]
    [InlineData(typeof(Plain), true)]
    [InlineData(typeof(Inherited), false)]
    [InlineData(typeof(Overloaded), false)]
    [InlineData(typeof(Generic), false)]
    [InlineData(typeof(Static), false)]
    [InlineData(typeof(Executor), false)]
    public void DiscoveryRequiresOneDeclaredInstanceNonGenericMethod(Type type, bool expected)
    {
        Assert.Equal(expected, MethodMacroFacts.TryGetExpandMethod(type, out var method));
        if (expected)
        {
            Assert.NotNull(method);
            Assert.Equal("Expand", method.Name);
            Assert.Equal(type, method.DeclaringType);
        }
        else
            Assert.Null(method);
    }

    [Theory]
    [InlineData(typeof(ClassDeclarationSyntax), MacroTarget.Type)]
    [InlineData(typeof(CaseDeclarationSyntax), MacroTarget.Type)]
    [InlineData(typeof(MethodDeclarationSyntax), MacroTarget.Method)]
    [InlineData(typeof(FunctionStatementSyntax), MacroTarget.Method)]
    [InlineData(typeof(PropertyDeclarationSyntax), MacroTarget.Property)]
    [InlineData(typeof(IndexerDeclarationSyntax), MacroTarget.Property)]
    [InlineData(typeof(FieldDeclarationSyntax), MacroTarget.Field)]
    [InlineData(typeof(ConstDeclarationSyntax), MacroTarget.Field)]
    [InlineData(typeof(EventDeclarationSyntax), MacroTarget.Event)]
    [InlineData(typeof(ParameterSyntax), MacroTarget.Parameter)]
    [InlineData(typeof(AccessorDeclarationSyntax), MacroTarget.Accessor)]
    [InlineData(typeof(ConstructorDeclarationSyntax), MacroTarget.Constructor)]
    [InlineData(typeof(ParameterlessConstructorDeclarationSyntax), MacroTarget.Constructor)]
    public void SyntaxTargetsRetainTheirCategories(Type type, MacroTarget expected)
        => Assert.Equal(expected, MethodMacroFacts.GetTarget(type));

    [Fact]
    public void ApplicationAndTargetRulesRespectReturnAndContextTypes()
    {
        var attached = typeof(Attached).GetMethod("Expand")!;
        Assert.Equal(MacroApplicationKind.Attached, MethodMacroFacts.GetApplicationKind(attached));
        Assert.Equal(MacroTarget.Type, MethodMacroFacts.GetTargets(attached));
        var context = typeof(ContextAttached).GetMethod("Expand")!;
        Assert.Equal(MacroApplicationKind.Attached, MethodMacroFacts.GetApplicationKind(context));
        Assert.Equal(MethodMacroFacts.AllTargets, MethodMacroFacts.GetTargets(context));
        Assert.Equal(MethodMacroFacts.AllTargets, MethodMacroFacts.GetTarget(typeof(object)));
        Assert.Equal(MacroTarget.None, MethodMacroFacts.GetTargets(typeof(Plain).GetMethod("Expand")!));
    }

    [Fact]
    public void ParameterProjectionSeparatesInjectedAndInvocationOrdinals()
    {
        var parameters = MethodMacroFacts.GetParameters(typeof(Mixed).GetMethod("Expand")!);
        Assert.Equal(new[] { MacroParameterSource.Context, MacroParameterSource.TokenBody, MacroParameterSource.DeclarationInput,
            MacroParameterSource.SyntaxInput, MacroParameterSource.SyntaxInput, MacroParameterSource.Value }, parameters.Select(p => p.Source));
        Assert.Equal(new int?[] { null, null, null, 0, 1, 2 }, parameters.Select(p => p.InvocationArgumentOrdinal));
        Assert.Equal(Enumerable.Range(0, 6), parameters.Select(p => p.DeclarationOrdinal));
        Assert.True(parameters[3].IsRequired);
        Assert.True(parameters[4].IsRequired);
        Assert.False(parameters[5].IsRequired);
        Assert.Equal("ExpressionSyntax<int>", parameters[4].TypeDisplayName);
        Assert.Equal("int", parameters[5].TypeDisplayName);
        Assert.Equal("7", parameters[5].DefaultValueDisplay);
        var attached = MethodMacroFacts.GetParameters(typeof(Attached).GetMethod("Expand")!);
        Assert.Equal(MacroParameterSource.AttachedTarget, attached[0].Source);
        Assert.Equal(MacroParameterSource.Context, attached[1].Source);
        Assert.Equal(0, attached[2].InvocationArgumentOrdinal);
    }

    [Fact]
    public void SuccessfulMethodOutputHasConditionalNullabilityMetadata()
    {
        var method = typeof(MethodMacroFacts).GetMethod(nameof(MethodMacroFacts.TryGetExpandMethod))!;
        var attribute = method.GetParameters()[1].GetCustomAttribute<System.Diagnostics.CodeAnalysis.NotNullWhenAttribute>();
        Assert.NotNull(attribute);
        Assert.True(attribute.ReturnValue);
    }

    private class Plain { public object Expand(int value = 1) => value; }
    private sealed class Inherited : Plain { }
    private sealed class Overloaded { public void Expand() { } public void Expand(int value) { } }
    private sealed class Generic { public T Expand<T>(T value) => value; }
    private sealed class Static { public static void Expand() { } }
    private sealed class Executor : IMacroExecutor
    {
        public MacroApplicationKind ApplicationKind => MacroApplicationKind.Freestanding;
        public MacroExecutionResult Expand(MacroExecutionContext context) => MacroExecutionResult.Freestanding(null);
    }
    private sealed class Attached
    {
        public MacroExpansionResult Expand(ClassDeclarationSyntax target, AttachedMacroContext context, int value = 0) => MacroExpansionResult.Empty;
    }
    private sealed class ContextAttached { public object Expand(AttachedMacroContext context) => context; }
    private sealed class Mixed
    {
        public object Expand(MacroContext context, IMacroTokenStream body, FreestandingMacroDeclarationSyntax declaration,
            Raven.CodeAnalysis.Syntax.ExpressionSyntax syntax, ExpressionSyntax<int> typed, int value = 7) => value;
    }
}
