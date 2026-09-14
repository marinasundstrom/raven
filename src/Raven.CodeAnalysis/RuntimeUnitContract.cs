namespace Raven.CodeAnalysis;

/// <summary>Selects the target value type that represents the language's unit value.</summary>
public sealed record RuntimeUnitContract(string AssemblyName, string TypeName);
