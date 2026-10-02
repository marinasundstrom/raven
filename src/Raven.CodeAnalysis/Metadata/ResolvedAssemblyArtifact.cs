namespace Raven.CodeAnalysis.Metadata;

// Compiler-owned input identity, containing values only. Backends reconstruct their
// own assembly references; no reader, resolver, token or reflection handle crosses.
internal sealed record ResolvedAssemblyArtifact(
    string Name, Version Version, string Culture, string PublicKeyToken, uint Flags,
    string Sha256);
