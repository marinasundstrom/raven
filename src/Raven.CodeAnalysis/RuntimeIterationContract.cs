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
    string CurrentProperty = "Current");
