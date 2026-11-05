using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class RuntimeContractValueTests
{
    public static IEnumerable<object[]> Contracts()
    {
        yield return [new RuntimeUnitContract("Core", "System.Unit"), new RuntimeUnitContract("Core", "System.Unit"), new RuntimeUnitContract("Other", "System.Unit")];
        yield return [new RuntimeTypeOfContract("Core", "Info", "Context"), new RuntimeTypeOfContract("Core", "Info", "Context"), new RuntimeTypeOfContract("Core", "Info", "Other")];
        yield return [new RuntimeSelfTypeContract("Core", "Self"), new RuntimeSelfTypeContract("Core", "Self"), new RuntimeSelfTypeContract("Core", "Other")];
        yield return [new RuntimePropagationContract("Core", "Propagation"), new RuntimePropagationContract("Core", "Propagation"), new RuntimePropagationContract("Other", "Propagation")];
        yield return [new RuntimeDisposalContract("Core", "Disposable"), new RuntimeDisposalContract("Core", "Disposable"), new RuntimeDisposalContract("Core", "Disposable", false)];
        yield return [new RuntimeIterationContract("Core", "Iterable", "Iterator"), new RuntimeIterationContract("Core", "Iterable", "Iterator"), new RuntimeIterationContract("Core", "Other", "Iterator")];
        yield return [new RuntimeAsyncIterationContract("Iterable", "Iterator"), new RuntimeAsyncIterationContract("Iterable", "Iterator"), new RuntimeAsyncIterationContract("Other", "Iterator")];
    }

    [Theory]
    [MemberData(nameof(Contracts))]
    public void ContractDataUsesValueEquality(object original, object equal, object different)
    {
        Assert.NotSame(original, equal);
        Assert.Equal(original, equal);
        Assert.Equal(original.GetHashCode(), equal.GetHashCode());
        Assert.NotEqual(original, different);
        Assert.False(original.Equals(null));
        Assert.Equal(2, new HashSet<object> { original, equal, different }.Count);
    }

    [Fact]
    public void UnitAndDisposalFlagsRetainDefaultsAndImmutableCopies()
    {
        var unit = new RuntimeUnitContract("Core", "Unit");
        Assert.False(unit.MapClrVoidToUnit);
        var mapped = unit with { MapClrVoidToUnit = true };
        Assert.True(mapped.MapClrVoidToUnit);
        Assert.False(unit.MapClrVoidToUnit);
        var (assembly, name, mapVoid) = mapped;
        Assert.Equal("Core", assembly);
        Assert.Equal("Unit", name);
        Assert.True(mapVoid);
        var disposal = new RuntimeDisposalContract("Core", "Disposable");
        Assert.True(disposal.UseExceptionHandling);
        Assert.False(disposal.AllowAsyncScopeExitCleanup);
        var changed = disposal with { UseExceptionHandling = false, AllowAsyncScopeExitCleanup = true };
        Assert.False(changed.UseExceptionHandling);
        Assert.True(changed.AllowAsyncScopeExitCleanup);
        Assert.Equal("Disposable", changed.InterfaceTypeName);
        Assert.True(disposal.UseExceptionHandling);
        Assert.False(disposal.AllowAsyncScopeExitCleanup);
    }

    [Fact]
    public void IterationDefaultsAndNestedCopiesPreserveIndependentContracts()
    {
        var original = new RuntimeIterationContract("Core", "Iterable", "Iterator");
        Assert.Equal("GetIterator", original.AcquisitionMethod);
        Assert.Equal("MoveNext", original.AdvanceMethod);
        Assert.Equal("Current", original.CurrentProperty);
        Assert.False(original.ArraysImplementIterable);
        Assert.Null(original.ArrayShapeTypeName);
        Assert.Null(original.Async);
        var async = new RuntimeAsyncIterationContract("AsyncIterable", "AsyncIterator");
        Assert.Equal("GetAsyncIterator", async.AcquisitionMethod);
        Assert.Equal("MoveNextAsync", async.AdvanceMethod);
        Assert.Equal("Current", async.CurrentProperty);
        Assert.Equal("DisposeAsync", async.DisposalMethod);
        Assert.False(async.AllowGeneratorBodies);
        var configured = original with { Async = async with { AllowGeneratorBodies = true }, ArrayShapeTypeName = "Array", ArraysImplementIterable = true };
        Assert.Null(original.Async);
        Assert.False(async.AllowGeneratorBodies);
        Assert.True(configured.Async!.AllowGeneratorBodies);
        Assert.True(configured.ArraysImplementIterable);
        Assert.Equal("Array", configured.ArrayShapeTypeName);
        Assert.Equal("Core", configured.AssemblyName);
        Assert.Equal("Iterable", configured.IterableTypeName);
        Assert.Equal("Iterator", configured.IteratorTypeName);
        Assert.Equal(original, configured with { Async = null, ArrayShapeTypeName = null, ArraysImplementIterable = false });
    }
}
