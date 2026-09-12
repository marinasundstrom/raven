namespace Raven.CodeAnalysis;

/// <summary>Selects an alternative target's three-parameter propagation interface.</summary>
public sealed record RuntimePropagationContract(string AssemblyName, string InterfaceTypeName);
