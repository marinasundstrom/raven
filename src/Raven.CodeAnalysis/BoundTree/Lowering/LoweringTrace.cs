using System.Collections.Concurrent;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public interface ILoweringTraceSink
{
    void RecordExtensionInvocation(ExtensionInvocationLoweringTrace entry);
}

public sealed class LoweringTraceLog : ILoweringTraceSink
{
    private readonly bool _isEnabled;
    private readonly ConcurrentQueue<ExtensionInvocationLoweringTrace> _extensionInvocations = new();

    public LoweringTraceLog()
        : this(isEnabled: true)
    {
    }

    public LoweringTraceLog(bool isEnabled)
    {
        _isEnabled = isEnabled;
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

public sealed class ExtensionInvocationLoweringTrace
{
    public ExtensionInvocationLoweringTrace(
        Symbol containingSymbol,
        MethodSymbol method,
        TypeSymbol receiverType,
        bool receiverCameFromInvocation,
        ImmutableArray<TypeSymbol> argumentTypes)
    {
        ContainingSymbol = containingSymbol;
        Method = method;
        ReceiverType = receiverType;
        ReceiverCameFromInvocation = receiverCameFromInvocation;
        ArgumentTypes = argumentTypes;
    }

    public Symbol ContainingSymbol { get; }

    public MethodSymbol Method { get; }

    public TypeSymbol ReceiverType { get; }

    public bool ReceiverCameFromInvocation { get; }

    public ImmutableArray<TypeSymbol> ArgumentTypes { get; }
}
