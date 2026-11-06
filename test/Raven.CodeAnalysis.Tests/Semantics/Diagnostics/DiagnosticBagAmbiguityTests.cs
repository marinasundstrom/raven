using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

[Trait("Target", "Shared")]
public sealed class DiagnosticBagAmbiguityTests
{
    [Theory]
    [InlineData(0, "Pick", "Pick")]
    [InlineData(1, "First() -> int", "First() -> int")]
    [InlineData(2, "First() -> int", "Second() -> int")]
    [InlineData(3, "First() -> int", "Second() -> int")]
    public void ReportsFirstTwoNonNullCandidatesWithRealSignatures(int count, string first, string second)
    {
        var candidates = new[] { Method("First"), Method("Second"), Method("Ignored") };
        var input = ImmutableArray<MethodSymbol>.Empty.Add(null!)
            .AddRange(candidates.Take(count)).Add(null!);
        var bag = new DiagnosticBag();
        bag.ReportCallIsAmbiguous("Pick", input, Location.None);
        var actual = Assert.Single(bag.AsEnumerable());
        var expected = Diagnostic.Create(CompilerDiagnostics.CallIsAmbiguous, Location.None, first, second);
        Assert.Equal(expected.Id, actual.Id);
        Assert.Equal(expected.GetMessage(), actual.GetMessage());
        Assert.Same(Location.None, actual.Location);
    }

    private static MethodSymbol Method(string name)
    {
        var integer = DispatchProxy.Create<TypeSymbol, SymbolProxy>();
        Set(integer, "get_Name", "Int32");
        Set(integer, "get_TypeKind", TypeKind.Struct);
        Set(integer, "get_SpecialType", SpecialType.System_Int32);
        Set(integer, "ToFullyQualifiedMetadataName", "System.Int32");
        var method = DispatchProxy.Create<MethodSymbol, SymbolProxy>();
        Set(method, "get_Name", name);
        Set(method, "get_Kind", SymbolKind.Method);
        Set(method, "get_MethodKind", MethodKind.Ordinary);
        Set(method, "get_ExplicitInterfaceImplementations", ImmutableArray<MethodSymbol>.Empty);
        Set(method, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(method, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(method, "get_Parameters", ImmutableArray<ParameterSymbol>.Empty);
        Set(method, "get_ReturnType", integer);
        foreach (var flag in new[] { "IsConstructor", "IsLambda", "IsStatic", "IsAbstract", "IsFinal", "IsOverride", "IsVirtual", "IsAsync" })
            Set(method, "get_" + flag, false);
        return method;
    }

    private static void Set(object target, string name, object? value)
        => ((SymbolProxy)target).Values[name] = value;

    public class SymbolProxy : DispatchProxy
    {
        public Dictionary<string, object?> Values { get; } = new();
        protected override object? Invoke(MethodInfo? method, object?[]? args)
            => Values.TryGetValue(method!.Name, out var value) ? value : throw new InvalidOperationException(method.Name);
    }
}
