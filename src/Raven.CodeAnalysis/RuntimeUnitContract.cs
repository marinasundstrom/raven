namespace Raven.CodeAnalysis;

/// <summary>Selects the target value type that represents the language's unit value.</summary>
/// <param name="AssemblyName">Assembly owning the unit value representation. NeoCLR permits an explicit source or native-artifact owner; the .NET policy remains tied to the selected core.</param>
/// <param name="TypeName">Metadata name of its public empty value type.</param>
/// <param name="MapClrVoidToUnit">Explicit .NET bootstrap policy: bind source CLR Void type syntax as unit; imported no-result returns remain void.</param>
public sealed record RuntimeUnitContract(string AssemblyName, string TypeName, bool MapClrVoidToUnit = false);
