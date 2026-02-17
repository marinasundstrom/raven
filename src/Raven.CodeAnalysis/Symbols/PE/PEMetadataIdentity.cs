namespace Raven.CodeAnalysis.Symbols;

internal readonly record struct PETypeIdentity(string AssemblyName, string MetadataName);

internal readonly record struct PETypeParameterIdentity(
    TypeParameterOwnerKind OwnerKind,
    int Ordinal,
    string OwnerIdentity,
    string Name);
