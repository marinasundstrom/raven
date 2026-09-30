namespace Raven.CodeAnalysis;

/// <summary>Metadata transport marker for a target runtime's native implementing-type contract.</summary>
/// <remarks>Requires TargetPlatform.NeoCLR. This does not lower Self to a CLI generic parameter and is not executable on the CLR.</remarks>
public sealed record RuntimeSelfTypeContract(string AssemblyName, string TypeName);
