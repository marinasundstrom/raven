# Strategy: Phasing out `MetadataLoadContext`

## Background

Raven's current code-generation pipeline assumes that reference assemblies are surfaced
through `System.Reflection.MetadataLoadContext` (MLC). The abstraction made it easy to reuse
reflection-centric helpers, but it couples the compiler to a runtime-only feature set and
prevents us from sharing infrastructure with Roslyn-style back ends. The limitations are
showing up in three places:

- **Context mixing** – Reflection.Emit builders and `MetadataLoadContext` expose incompatible
  `System.Type` handles. Anything that needs to instantiate `Nullable<T>`, tuple helpers, or
  other framework primitives across emitted types falls over unless we mirror Roslyn's
  metadata-only pipeline.
- **Targeting flexibility** – MLC requires live runtime assemblies and lacks first-class
  support for true reference packs (e.g., `Microsoft.NETCore.App.Ref`). It also does not honor
  assembly identities the way Roslyn's metadata readers do, complicating multi-targeting.
- **Future back ends** – A Roslyn-like emitter works exclusively with SRM metadata builders and
  symbol->handle translation. The current design blocks incremental migration because the
  entire lowering stack expects `System.Type` and `MemberInfo` instances.

## Goals

1. Remove the dependency on `System.Reflection.MetadataLoadContext` from the compilation and
   code-generation layers.
2. Continue compiling against reference assemblies (no IL bodies) exactly like the .NET SDK
   reference packs ship today.
3. Establish data structures that map naturally onto Roslyn's `CommonReferenceManager` and
   `PEModuleBuilder` so we can adopt a metadata-only emission pipeline.
4. Preserve the ergonomics we need while migrating (unit tests, Reflection.Emit shims) without
   regressing nullable/union emission.

## Guiding principles

- **One source of truth:** All reference metadata is loaded via `System.Reflection.Metadata`
  (SRM) readers. We never ask the runtime to materialize `System.Type` for reference-only
  assemblies.
- **Layered abstraction:** Introduce a compilation-time `ReferenceAssemblyCatalog` that owns
  the SRM readers and surfaces both Roslyn-friendly metadata handles and any temporary
  reflection bridges that legacy emitters still require.
- **Explicit bridging:** When we must talk to Reflection.Emit (during the transition period),
  we map SRM handles to runtime types through a `RuntimeAssemblyBridge` that loads real
  implementation assemblies using `AssemblyLoadContext`. The bridge is opt-in and built on
  assembly identity matching so it works for any reference pack with a matching runtime pack.
- **Roslyn compatibility first:** New APIs should shape their inputs/outputs as SRM handles or
  Raven symbol abstractions instead of `System.Type`.

## Proposed architecture

### 1. Reference metadata ingestion

- Create `ReferenceAssemblyCatalog` under `Raven.CodeAnalysis` that:
  - Accepts a set of file paths (reference assemblies) and lazily opens them using
    `PEReader`/`MetadataReaderProvider`.
  - Interns `AssemblyIdentity` data (name, version, culture, public key token) so lookups are
    deterministic.
  - Exposes Roslyn-style helpers (`TryGetMetadataReader`, `GetAssemblyDefinitionHandle`, etc.)
    to the binder and semantic model.
- Replace `Compilation.MetadataLoadContext` with a catalog instance. Symbols and binders consult
  the catalog for metadata rather than `Type` objects.

### 2. Symbol materialization

- Update `TypeSymbolExtensionsForCodeGen` (and similar utilities) to operate on SRM handles.
  Instead of returning `System.Type`, expose methods that translate `ITypeSymbol` into either:
  - `EntityHandle`/`TypeReferenceHandle` for metadata-only back ends, or
  - `RuntimeTypeHandle` via the temporary bridge when Reflection.Emit is still in play.
- Persist both metadata handles and optional runtime bridges in a lightweight cache keyed by
  `ITypeSymbol` to avoid repeated reader traversals.

### 3. Runtime bridge (transitional)

- Introduce `RuntimeAssemblyBridge` that:
  - Resolves implementation assemblies using the target framework moniker (TFM) and
    `Microsoft.NETCore.App.Runtime.*` (or Mono/Wasmtime equivalents when present).
  - Validates assembly identities before loading so we never accidentally bind against the host
    runtime.
  - Provides APIs to map a catalog identity to `Assembly`/`Type` only for constructs that still
    require Reflection.Emit (e.g., tests or interim features).
- Gate all Reflection.Emit usage behind the bridge so removing it later is a localized effort.

### 4. Code-generation pipeline alignment

- Refactor `CodeGenerator` to consume SRM handles instead of `Type`:
  - Separate lowering (which produces bound trees) from emission (which walks bound nodes and
    produces SRM metadata + IL blobs via `MethodBodyStreamEncoder`).
  - Model the emitter after Roslyn's `PEModuleBuilder`: maintain tables for `TypeDefinition`,
    `MethodDefinition`, etc., and leverage SRM encoders to build the final image.
  - Encapsulate any remaining Reflection.Emit helpers in an adapter layer that translates the
    Roslyn-friendly representation into runtime types only during tests that rely on dynamic
    invocation.
- Add a thin façade so the CLI can choose between the new SRM-backed emitter and the legacy
  Reflection.Emit path while the migration is in progress.

### 5. Testing strategy

- Rewrite code-generation tests to assert over SRM metadata (e.g., inspect
  `MetadataReader` outputs) instead of reflection types.
- Provide regression tests for nullable unions, tuple helpers, and attribute emission using the
  new pipeline.
- Keep one or two smoke tests that still exercise the runtime bridge to ensure dynamic
  execution keeps working until the bridge is removed.

## Migration plan

1. **Bootstrap catalog** – Add `ReferenceAssemblyCatalog`, swap `Compilation` to use it for
   metadata queries, and remove direct `MetadataLoadContext` allocations.
2. **Adopt SRM in utilities** – Update `TypeSymbolExtensions*`, expression/statement generators,
   and runtime type resolution helpers to speak SRM handles; introduce bridge shims where
   Reflection.Emit is unavoidable.
3. **Rework emitters** – Incrementally replace Reflection.Emit emission with SRM builders
   (start with attributes and type references, then method bodies). During this phase the code
   generator should produce both SRM metadata and, optionally, runtime types via the bridge.
4. **Drop bridge-only paths** – Once the Roslyn-like emitter can produce runnable PE images,
   delete the bridge and any lingering Reflection.Emit code.
5. **Finalize** – Remove the `System.Reflection.MetadataLoadContext` package dependency, update
   documentation/tooling, and add diagnostics to ensure reference packs are resolved through
   the catalog.

## Open questions

- How do we best locate runtime packs for arbitrary TFMs on developer machines? We may need a
  resolver that understands `dotnet --info` layouts similar to Roslyn's
  `AppPathTypeResolverFactory`.
- Should the catalog own lifetime/disposal for all `MetadataReaderProvider` instances, or should
  the `Compilation` cache them globally? This affects incremental compilations.
- To what extent can we reuse Roslyn's `PortableExecutableReference` abstractions versus rolling
  custom ones tailored to Raven?

Answering these questions is part of the migration, but the architecture above keeps us aligned
with Roslyn's design and lets us retire `MetadataLoadContext` without losing reference-assembly
compatibility.
