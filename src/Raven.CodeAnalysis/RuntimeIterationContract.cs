namespace Raven.CodeAnalysis;

/// <summary>
/// Selects the nominal synchronous iteration protocol for an alternative runtime.
/// Null on CompilationOptions preserves the standard .NET interface/pattern rules.
/// This selects symbols, not assembly resolution policy or automatic disposal behavior.
/// </summary>
public sealed record RuntimeIterationContract(
    string AssemblyName,
    string IterableTypeName,
    string IteratorTypeName,
    string AcquisitionMethod = "GetIterator",
    string AdvanceMethod = "MoveNext",
    string CurrentProperty = "Current",
    bool ArraysImplementIterable = false,
    string? ArrayShapeTypeName = null,
    RuntimeAsyncIterationContract? Async = null);

/// <summary>
/// Selects an alternative nominal asynchronous iteration protocol. Acquisition is
/// parameterless; advancement must await to Boolean and disposal to void or unit.
/// This describes consumption, not generator synthesis or runtime suspension.
/// </summary>
public sealed record RuntimeAsyncIterationContract(
    string IterableTypeName,
    string IteratorTypeName,
    string AcquisitionMethod = "GetAsyncIterator",
    string AdvanceMethod = "MoveNextAsync",
    string CurrentProperty = "Current",
    string DisposalMethod = "DisposeAsync",
    bool AllowGeneratorBodies = false);
