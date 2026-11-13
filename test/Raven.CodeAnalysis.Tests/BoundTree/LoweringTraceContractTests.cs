using System.Collections.Immutable;
using System.Reflection;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class LoweringTraceContractTests
{
    [Fact]
    public void PositionalEntrySupportsDeconstructionAndInitOnlyCopying()
    {
        var original = Entry();
        var (owner, method, receiver, fromInvocation, arguments) = original;
        Assert.Same(original.ContainingSymbol, owner);
        Assert.Same(original.Method, method);
        Assert.Same(original.ReceiverType, receiver);
        Assert.Equal(original.ReceiverCameFromInvocation, fromInvocation);
        Assert.Equal(original.ArgumentTypes, arguments);

        var copy = original with { ReceiverCameFromInvocation = true };
        Assert.False(original.ReceiverCameFromInvocation);
        Assert.True(copy.ReceiverCameFromInvocation);
        Assert.NotEqual(original, copy);
        Assert.Equal(original, copy with { ReceiverCameFromInvocation = false });

        foreach (var property in typeof(ExtensionInvocationLoweringTrace).GetProperties())
        {
            Assert.NotNull(property.SetMethod);
            Assert.Contains(typeof(System.Runtime.CompilerServices.IsExternalInit),
                property.SetMethod.ReturnParameter.GetRequiredCustomModifiers());
        }
    }

    [Fact]
    public void EntryPreservesInputsAndHasRecordValueSemantics()
    {
        var owner = Identity(Make<MethodSymbol>(SymbolKind.Method, "Owner"));
        var method = Identity(Make<MethodSymbol>(SymbolKind.Method, "Extension"));
        var receiver = Identity(Named("Runtime", "Example.Receiver"));
        var arguments = ImmutableArray.Create<TypeSymbol>(receiver);
        var first = new ExtensionInvocationLoweringTrace(ContainingSymbol: owner, Method: method,
            ReceiverType: receiver, ReceiverCameFromInvocation: true, ArgumentTypes: arguments);
        var second = new ExtensionInvocationLoweringTrace(owner, method, receiver, true, arguments);
        Assert.Same(owner, first.ContainingSymbol);
        Assert.Same(method, first.Method);
        Assert.Same(receiver, first.ReceiverType);
        Assert.True(first.ReceiverCameFromInvocation);
        Assert.Equal(arguments, first.ArgumentTypes);
        Assert.NotSame(first, second);
        Assert.Equal(first, second);
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.IsAssignableFrom<IEquatable<ExtensionInvocationLoweringTrace>>(first);
        Assert.False(first == (ExtensionInvocationLoweringTrace?)null);
        Assert.True(first != (ExtensionInvocationLoweringTrace?)null);
        Assert.NotEqual(first, new ExtensionInvocationLoweringTrace(owner, method, receiver, false, arguments));
    }

    [Fact]
    public void RecordUsesStoredImmutableArrayEqualityWithoutChangingInputShape()
    {
        var first = Entry();
        var defaultArguments = new ExtensionInvocationLoweringTrace(first.ContainingSymbol, first.Method,
            first.ReceiverType, false, default);
        Assert.True(defaultArguments.ArgumentTypes.IsDefault);
        var emptyArguments = new ExtensionInvocationLoweringTrace(first.ContainingSymbol, first.Method,
            first.ReceiverType, false, ImmutableArray<TypeSymbol>.Empty);
        Assert.False(emptyArguments.ArgumentTypes.IsDefault);
        Assert.NotEqual(defaultArguments, emptyArguments);
        var distinctStorage = ImmutableArray.Create(first.ArgumentTypes[0]);
        Assert.NotEqual(first, new ExtensionInvocationLoweringTrace(first.ContainingSymbol, first.Method,
            first.ReceiverType, first.ReceiverCameFromInvocation, distinctStorage));
    }

    [Fact]
    public void EnabledLogRetainsDuplicatesOrderAndSnapshotIsolation()
    {
        var log = new LoweringTraceLog();
        var sink = (ILoweringTraceSink)log;
        var first = Entry();
        var second = Entry();
        Assert.Empty(log.ExtensionInvocations);
        sink.RecordExtensionInvocation(first);
        var snapshot = log.ExtensionInvocations;
        sink.RecordExtensionInvocation(second);
        sink.RecordExtensionInvocation(first);
        Assert.Same(first, Assert.Single(snapshot));
        Assert.Collection(log.ExtensionInvocations,
            value => Assert.Same(first, value),
            value => Assert.Same(second, value),
            value => Assert.Same(first, value));
    }

    [Theory]
    [InlineData(false, 0)]
    [InlineData(true, 1)]
    public void ConstructorFlagControlsRecording(bool enabled, int count)
    {
        var log = new LoweringTraceLog(isEnabled: enabled);
        ((ILoweringTraceSink)log).RecordExtensionInvocation(Entry());
        Assert.Equal(count, log.ExtensionInvocations.Length);
    }

    [Fact]
    public void SinkRemainsExplicitAndLogRetainsReferenceIdentity()
    {
        Assert.Null(typeof(LoweringTraceLog).GetMethod("RecordExtensionInvocation", BindingFlags.Public | BindingFlags.Instance));
        Assert.NotEqual(new LoweringTraceLog(), new LoweringTraceLog());
    }

    [Fact]
    public void ConcurrentWritersDoNotLoseOrDeduplicateEntries()
    {
        var log = new LoweringTraceLog();
        var sink = (ILoweringTraceSink)log;
        var entry = Entry();
        Parallel.For(0, 128, _ => sink.RecordExtensionInvocation(entry));
        var snapshot = log.ExtensionInvocations;
        Assert.Equal(128, snapshot.Length);
        Assert.All(snapshot, value => Assert.Same(entry, value));
    }

    private static T Identity<T>(T symbol) where T : class, Symbol
    {
        Set(symbol, "Equals", (Func<object?[]?, object?>)(arguments => ReferenceEquals(symbol, arguments![0])));
        return symbol;
    }

    private static ExtensionInvocationLoweringTrace Entry()
    {
        var owner = Identity(Make<MethodSymbol>(SymbolKind.Method, "Owner"));
        var method = Identity(Make<MethodSymbol>(SymbolKind.Method, "Extension"));
        var receiver = Identity(Named("Runtime", "Example.Receiver"));
        return new ExtensionInvocationLoweringTrace(owner, method, receiver, false, ImmutableArray.Create<TypeSymbol>(receiver));
    }
}
