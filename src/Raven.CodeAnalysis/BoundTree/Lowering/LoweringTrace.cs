using System.Collections.Concurrent;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public interface ILoweringTraceSink
{
    void RecordExtensionInvocation(ExtensionInvocationLoweringTrace entry);
}

public sealed class LoweringTraceLog(bool isEnabled) : ILoweringTraceSink
{
    private readonly bool _isEnabled = isEnabled;
    private readonly ConcurrentQueue<ExtensionInvocationLoweringTrace> _extensionInvocations = new();

    public LoweringTraceLog()
        : this(isEnabled: true)
    {
    }

    public ImmutableArray<ExtensionInvocationLoweringTrace> ExtensionInvocations
        => _extensionInvocations.ToImmutableArray();

    void ILoweringTraceSink.RecordExtensionInvocation(ExtensionInvocationLoweringTrace entry)
    {
        if (!_isEnabled)
            return;

        _extensionInvocations.Enqueue(entry);
    }
}

public sealed record class ExtensionInvocationLoweringTrace(
    Symbol ContainingSymbol,
    MethodSymbol Method,
    TypeSymbol ReceiverType,
    bool ReceiverCameFromInvocation,
    ImmutableArray<TypeSymbol> ArgumentTypes);
