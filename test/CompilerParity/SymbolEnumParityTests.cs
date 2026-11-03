extern alias Seed;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SymbolEnumParityTests
{
    public static IEnumerable<object[]> EnumTypes()
    {
        yield return [typeof(Seed::Raven.CodeAnalysis.SymbolKind), typeof(Raven.CodeAnalysis.SymbolKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.Accessibility), typeof(Raven.CodeAnalysis.Accessibility)];
        yield return [typeof(Seed::Raven.CodeAnalysis.MethodKind), typeof(Raven.CodeAnalysis.MethodKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.IteratorMethodKind), typeof(Raven.CodeAnalysis.IteratorMethodKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.RefKind), typeof(Raven.CodeAnalysis.RefKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.ScopedKind), typeof(Raven.CodeAnalysis.ScopedKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.TypeKind), typeof(Raven.CodeAnalysis.TypeKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.TypeParameterConstraintKind), typeof(Raven.CodeAnalysis.TypeParameterConstraintKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.VarianceKind), typeof(Raven.CodeAnalysis.VarianceKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.TypeParameterOwnerKind), typeof(Raven.CodeAnalysis.TypeParameterOwnerKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.SpecialType), typeof(Raven.CodeAnalysis.SpecialType)];
        yield return [typeof(Seed::Raven.CodeAnalysis.Macros.MacroKind), typeof(Raven.CodeAnalysis.Macros.MacroKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.Macros.MacroApplicationKind), typeof(Raven.CodeAnalysis.Macros.MacroApplicationKind)];
        yield return [typeof(Seed::Raven.CodeAnalysis.Macros.MacroInvocationTargets), typeof(Raven.CodeAnalysis.Macros.MacroInvocationTargets)];
        yield return [typeof(Seed::Raven.CodeAnalysis.Macros.MacroTarget), typeof(Raven.CodeAnalysis.Macros.MacroTarget)];
        yield return [typeof(Seed::Raven.CodeAnalysis.Macros.MacroParameterRole), typeof(Raven.CodeAnalysis.Macros.MacroParameterRole)];
    }

    [Theory]
    [MemberData(nameof(EnumTypes))]
    public void PortOwnsEnumsWithMatchingMetadata(Type seed, Type port)
    {
        Assert.NotSame(seed.Assembly, port.Assembly);
        Assert.Equal(typeof(Raven.CodeAnalysis.Text.TextSpan).Assembly, port.Assembly);
        Assert.Equal("Raven.CodeAnalysis.Raven", port.Assembly.GetName().Name);
        Assert.True(port.IsPublic);
        Assert.Equal(Enum.GetUnderlyingType(seed), Enum.GetUnderlyingType(port));
        Assert.Equal(seed.IsDefined(typeof(FlagsAttribute), false), port.IsDefined(typeof(FlagsAttribute), false));
        Assert.Equal(Enum.GetNames(seed), Enum.GetNames(port));
        Assert.Equal(Enum.GetValues(seed).Cast<object>().Select(Convert.ToInt64),
            Enum.GetValues(port).Cast<object>().Select(Convert.ToInt64));
    }
}
