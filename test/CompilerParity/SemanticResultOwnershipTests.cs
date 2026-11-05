extern alias Seed;

using System.Reflection;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;

using TypeInfo = Raven.CodeAnalysis.TypeInfo;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SemanticResultOwnershipTests
{
    [Fact]
    public void ResultTypesAndTheirSymbolSignaturesBelongToPort()
    {
        var port = typeof(Symbol).Assembly;
        foreach (var type in new[] { typeof(Conversion), typeof(SymbolInfo), typeof(CandidateReason), typeof(TypeInfo) })
            Assert.Same(port, type.Assembly);
        Assert.Equal(typeof(MethodSymbol), typeof(Conversion).GetProperty("MethodSymbol")!.PropertyType);
        Assert.Equal(typeof(MethodSymbol), typeof(Conversion).GetProperty("ConstructorSymbol")!.PropertyType);
        Assert.Equal(typeof(TypeSymbol), typeof(TypeInfo).GetProperty("Type")!.PropertyType);
        Assert.Equal(typeof(TypeSymbol), typeof(TypeInfo).GetProperty("ConvertedType")!.PropertyType);
        Assert.Equal(typeof(Conversion), typeof(TypeInfo).GetProperty("Conversion")!.PropertyType);
        Assert.Equal(typeof(Symbol), typeof(SymbolInfo).GetProperty("Symbol")!.PropertyType);
        Assert.Equal(typeof(Symbol), typeof(SymbolInfo).GetProperty("CandidateSymbols")!.PropertyType.GetGenericArguments().Single());
        Assert.NotSame(typeof(Seed::Raven.CodeAnalysis.Symbol).Assembly, port);
        foreach (var type in new[] { typeof(Conversion), typeof(SymbolInfo) })
        {
            Assert.True(type.IsDefined(typeof(IsReadOnlyAttribute), inherit: false));
            Assert.All(type.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic),
                field => Assert.True(field.IsInitOnly, $"{type.Name}.{field.Name} must be readonly"));
        }
    }

    [Fact]
    public void CandidateReasonsPreserveNamesAndValues()
    {
        Assert.Equal(Enum.GetNames<Seed::Raven.CodeAnalysis.CandidateReason>(), Enum.GetNames<CandidateReason>());
        Assert.Equal(Enum.GetValues<Seed::Raven.CodeAnalysis.CandidateReason>().Select(value => (int)value),
            Enum.GetValues<CandidateReason>().Select(value => (int)value));
    }
}
