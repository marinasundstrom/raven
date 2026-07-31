using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BinderOverrideContractTests
{
    [Fact]
    public void MethodBodyBinder_ArrowExpressionBinding_OverridesBinderContract()
    {
        var parameterTypes = new[] { typeof(ArrowExpressionClauseSyntax) };
        var baseMethod = typeof(Binder).GetMethod(
            "BindArrowExpressionClause",
            BindingFlags.Instance | BindingFlags.NonPublic,
            binder: null,
            parameterTypes,
            modifiers: null);
        var overrideMethod = typeof(MethodBodyBinder).GetMethod(
            "BindArrowExpressionClause",
            BindingFlags.Instance | BindingFlags.NonPublic,
            binder: null,
            parameterTypes,
            modifiers: null);

        Assert.NotNull(baseMethod);
        Assert.NotNull(overrideMethod);
        Assert.True(baseMethod.IsVirtual);
        Assert.Equal(baseMethod, overrideMethod.GetBaseDefinition());
        Assert.Equal(typeof(BoundNode), overrideMethod.ReturnType);
    }
}
