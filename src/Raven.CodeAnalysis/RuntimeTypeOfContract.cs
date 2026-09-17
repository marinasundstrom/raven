namespace Raven.CodeAnalysis;

/// <summary>Selects a runtime context whose Current.GetTypeInfoFromHandle resolves language typeof expressions.</summary>
public sealed record RuntimeTypeOfContract(string AssemblyName, string TypeInfoTypeName, string ContextTypeName);
