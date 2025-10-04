# Code generation notes

Raven is moving toward a metadata-only emission pipeline built on
`System.Reflection.Metadata` (SRM). The code generator must never mix handles from
`System.Reflection.Emit` with those produced by `MetadataLoadContext`. Instead, resolve
framework helpers and reference metadata through the shared SRM-based catalog so the
compiler remains agnostic of the host runtime.

- Obtain framework types via the runtime type resolver exposed by `CodeGenerator`. It sources
  definitions from SRM metadata and, when necessary, projects them into runtime handles using
  the bridge outlined in [the metadata migration strategy](../../design/metadata-migration-strategy.md).
- Avoid `typeof(...)` inside the emitter. If you need a CLR handle during the transition
  period, request it through the runtime resolver so tests and the CLI use the same pathway.
- Prefer translating `ITypeSymbol` instances into SRM `EntityHandle`s or symbol descriptors.
  Only fall back to reflection types in legacy helpers that have not yet been ported.
- When adding new emitters, shape their inputs around SRM handles and let the adapter layer
  convert them for Reflection.Emit until the Roslyn-like backend is complete.

Following these rules keeps the code generator aligned with the SRM catalog while we retire
`MetadataLoadContext`, enabling interchangeable back ends that work with reference
assemblies.
