# System.Reflection.Metadata transition plan

## Background

Raven's back end currently relies on `System.Reflection.Emit` to materialize
runtime `TypeBuilder`, `MethodBuilder`, and related artifacts while lowering
bound trees into IL. The generator caches the runtime builders in several
places: `TypeGenerator` stores the active `TypeBuilder` and exposes generated
`Type` instances, and it manipulates builder APIs to configure inheritance,
interfaces, and nested types.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L1-L160】
`CodeGenerator` keeps global maps from compiler symbols to runtime
`MemberInfo` objects, uses `GenericTypeParameterBuilder` to encode constraints,
and resolves attributes and literal values by asking symbols for their CLR
`Type` instances.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L1-L175】

The symbol layer mirrors this dependency: the `PE*Symbol` family wraps
reflection metadata (`Assembly`, `Module`, `TypeInfo`, etc.) and resolves
metadata members by walking runtime types. `PEAssemblySymbol` forwards queries
through a real `Assembly`, and `PEModuleSymbol` manufactures symbols by caching
`System.Type` instances. `PENamedTypeSymbol` itself dereferences a
`System.Reflection.TypeInfo` to answer almost every shape question.【F:src/Raven.CodeAnalysis/Symbols/PE/PEAssemblySymbol.cs†L1-L80】【F:src/Raven.CodeAnalysis/Symbols/PE/PEModuleSymbol.cs†L1-L187】【F:src/Raven.CodeAnalysis/Symbols/PE/PENamedTypeSymbol.cs†L1-L120】

Finally, helper layers turn compiler symbols back into runtime `System.Type`
values. `TypeSymbolExtensionsForCodeGen` and `TypeSymbolExtensions` stitch
runtime types together for generics, tuples, attributes, and unions, while
`TypeResolver` walks `System.Reflection` metadata when binding references.
These helpers are pervasive and assume the presence of runtime type objects.
【F:src/Raven.CodeAnalysis/TypeSymbolExtensionsForCodeGen.cs†L1-L200】【F:src/Raven.CodeAnalysis/TypeSymbolExtensions.cs†L1-L120】【F:src/Raven.CodeAnalysis/TypeResolver.cs†L1-L194】

Switching to `System.Reflection.Metadata` (SRM) requires a structural change:
SRM only manipulates metadata rows and IL blobs. It neither produces runtime
types nor accepts them as input. Therefore we need an intermediate model that
captures metadata handles instead of `System.Type` and a new writer that
serializes those handles into a PE file.

## Goals

* Emit assemblies using SRM (`MetadataBuilder`, `BlobBuilder`,
  `InstructionEncoder`) instead of `System.Reflection.Emit`.
* Stop depending on `System.Reflection`/`System.Type` in the symbol layer and
  code generation pipeline.
* Maintain feature parity (custom attributes, generics, async/iterator support,
  unions, etc.) throughout the migration.

## Proposed architecture

### 1. Introduce a metadata-centric emission model

Create a set of lightweight data structures that capture the information
currently spread across `TypeGenerator`, `MethodGenerator`, `FieldBuilder`, and
friends:

* `MetadataModuleBuilder` – owns an SRM `MetadataBuilder`, a `BlobBuilder` for
  IL, and tables for string/user strings. Responsible for final PE assembly
  layout.
* `MetadataType` – represents a type definition being emitted. Stores symbolic
  references to base types, interface implementations, layout flags, generic
  parameter descriptors, field and method collections, and custom attribute
  data (in compiler-friendly form).
* `MetadataMethod`/`MetadataField`/`MetadataProperty` – hold signature
  descriptors, IL streams (via `InstructionEncoder`), locals, exception
  regions, and attribute lists.

This intermediate model lets the generator stage the entire assembly without
committing to runtime builders. It also provides a natural location to cache SRM
`EntityHandle` values once they are assigned.

Refactor `CodeGenerator` and `TypeGenerator` to populate these metadata objects
instead of creating `TypeBuilder` instances. The `IILBuilder` abstraction
already isolates IL emission; we can replace its backing implementation with an
SRM encoder that writes directly into a `BlobBuilder` while still supporting the
existing visitor pipeline. During migration we can keep both implementations by
introducing a new `SrmILBuilderFactory` behind the same interface until the
Reflection.Emit path is removed.

### 2. Replace `System.Type` lookups with symbol/handle resolution

The helpers that currently call `GetClrType` need to stop materializing runtime
`Type` objects. Instead, introduce services that can translate compiler symbols
into:

* `EntityHandle` for definitions (types defined in the current compilation).
* `EntityHandle` or `Handle` wrappers for references (types/methods from
  metadata references).
* `StandaloneSignatureHandle` for method/field signatures used by call sites.

A dedicated `MetadataReferenceMapper` can provide these handles by consulting
symbol tables and SRM metadata readers. This mapper replaces usages of
`TypeSymbolExtensionsForCodeGen.GetClrType` and mirrors Roslyn's
`MetadataBuilder` pipeline, but scoped to Raven's needs.

Custom attribute emission should operate on compiler-level `AttributeData`
only. Rather than building `CustomAttributeBuilder`, resolve constructor and
property handles using the mapper, then build the attribute blob with
`BlobEncoder`. The existing analysis (which already inspects `AttributeData`)
remains valid; only the serialization step changes.

### 3. Rework metadata binding (`PE*Symbol`)

Swap the reflection-backed symbol classes for SRM readers:

* Wrap each referenced assembly in a `MetadataReader`/`PEReader` pair instead of
  `Assembly`/`Module`. Persist tables such as `TypeDefinitionHandle` → symbol
  mapping inside `PEModuleSymbol`.
* Reimplement `PENamedTypeSymbol` to read shape data from the `MetadataReader`
  (flags, base type, layout, generics, nested types) rather than asking a
  `TypeInfo`. Keep the symbol surface identical so the rest of the compiler does
  not change.
* Replace `TypeResolver` with a reader that walks SRM handles. For example,
  retrieving parameter types should decode the method signature via
  `SignatureDecoder`, honoring nullability attributes by consulting the relevant
  custom attribute blobs.

SRM access requires caching handles to avoid repeatedly decoding the same
signature. Introduce dictionary caches keyed by handles instead of `System.Type`
instances (e.g., `Dictionary<TypeDefinitionHandle, INamedTypeSymbol>`).

### 4. Stage the migration

A safe migration plan can proceed in phases:

1. **Abstraction phase:** Add new metadata model types and teach the code
   generator to populate them while still producing Reflection.Emit builders.
   Use the metadata structures as the single source of truth for symbol → member
   mappings; the Reflection.Emit integration becomes a thin adapter.
2. **Emission phase:** Implement the SRM writer that walks the metadata model,
   assigning `EntityHandle`s, emitting IL via `InstructionEncoder`, and
   generating the final PE stream (headers, section tables, resources). Introduce
   a feature flag so both back ends can coexist during validation.
3. **Binding phase:** Rewrite `PE*Symbol` classes and `TypeResolver` to consume
   SRM metadata directly. Provide a shim that can fall back to reflection when a
   metadata reference lacks SRM (for example, during bootstrap) so the transition
   does not break existing tests immediately.
4. **Removal phase:** Once both emission and binding use SRM, delete the
   Reflection.Emit adapters, the `GetClrType` helpers, and any remaining
   `System.Type` caches. Update tests to operate purely on emitted metadata (for
   example by inspecting the PE with SRM or ILSpy rather than runtime loading).

### 5. Testing and tooling

* Extend existing IL verification tests to accept SRM-produced IL blobs. The
  current tests instantiate `System.Reflection.Emit` builders to re-run IL; they
  will need to read the emitted PE via `PEReader` and decode method bodies.
* Provide diagnostic dumps (e.g., via `MetadataVisualizer` or a custom
  visitor) to compare metadata rows between the old and new pipelines during the
  transition.
* Ensure custom attribute encoding for unions and nullable metadata matches the
  expectations of `TypeResolver`'s replacement.

## Open questions / risks

* **Nullability and union metadata:** `TypeResolver` currently relies on
  `NullabilityInfoContext` and ad-hoc decoding of `TypeUnionAttribute` using
  runtime reflection.【F:src/Raven.CodeAnalysis/TypeResolver.cs†L14-L118】 We need
  SRM-based readers for these attributes, which implies rethinking how we encode
  nullability annotations during emission.
* **Dynamic loading scenarios:** The existing pipeline can execute emitted
  assemblies in-memory via `AssemblyBuilder`. After the SRM switch we must either
  write to disk and load via `AssemblyLoadContext` or embed a lightweight
  metadata reader for validation. Clarify whether in-memory execution is still a
  requirement.
* **Third-party dependencies:** Some tests use `RecordingILBuilderFactory` and
  similar helpers that wrap `System.Reflection.Emit`. They will require SRM-aware
  equivalents once the primary pipeline moves away from runtime builders.

## Status checklist

- ✅ Seed metadata module/type definition model and populate it from the existing `TypeGenerator`.
- ✅ Add metadata method/field/property representations and switch generation to populate them instead of Reflection.Emit builders.
- ⬜ Implement SRM-backed IL/PE writer that consumes the metadata model.
- ⬜ Replace reflection-backed `PE*Symbol` readers and `TypeResolver` with SRM handle decoding.
- ⬜ Introduce handle-based mappers for definitions/references and remove `GetClrType` helpers.
- ⬜ Update IL/codegen tests (and supporting tooling) to validate SRM output directly.
- ⬜ Delete the remaining Reflection.Emit adapters and any residual `System.Type` usage once SRM emission/binding ships.

Executing this plan will let Raven emit and consume assemblies without touching
`System.Reflection.Emit`, aligning the compiler with modern metadata tooling and
reducing runtime coupling.
