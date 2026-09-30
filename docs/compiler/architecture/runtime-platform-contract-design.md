# Runtime/platform contract selection and compatibility

Status: implementation design, 2026-09-30. Implements the direction of
[ADR-0003](decisions/0003-target-owned-metadata-and-emission.md); the selection
pipeline below is not implemented yet. The existing compiler still uses .NET
symbol loading and CLI emission, with experimental target options.

## Ownership

A runtime/platform contract defines the environment in which a Raven program
has meaning: its available type universe, runtime semantics, representations,
required protocols, and supported features. A target framework can supply much
of this information. Compiler-owned rules for known platforms supplement it.
The contract is more than an output format or target-framework name.

| Component | Owns | Must not assume |
| --- | --- | --- |
| Contract selection | Profile/version, explicit configuration, applicable compiler rules, compatible source/backend choices | A host framework is the selected target |
| Symbol source | Opening supported inputs and exposing declarations, identities, signatures, attributes, and provenance | Every source is a file, CLI assembly, or executable library |
| Symbol environment | Combining source contributions into the contract's type universe, resolving cross-source identities, lazy symbol projection | Equal names imply equal identities, or a missing target type can come from the host |
| Semantic model | Raven binding, conversions, operations, diagnostics, and target-sensitive meaning | Roslyn API shape or CLR type categories are mandatory |
| Code generator | Validating supported input, target lowering, layout, calling conventions, and output/debug artifacts | Any bound compilation can be emitted for any platform |
| Compiler host | Running the compiler and extensions, I/O, tooling, and deployment coordination | Compiler-host libraries belong to the application's type universe |

The selected contract can admit multiple symbol sources and multiple code
generators. A source supplies definitions; a generator must implement their
meaning and representation under that contract. Registering an implementation
of an interface alone does not establish compatibility.

Keep initial APIs internal and replaceable. Names and signatures follow tested
ownership boundaries rather than becoming a plugin ABI. Migrate controlled
consumers together when redesigning a contract. Assembly-shaped discovery APIs
introduced during extraction are .NET-facing steps, not the universal source API.

## Selection and compilation lifecycle

1. **Collect configuration.** Project evaluation or the API caller supplies a
   runtime/platform profile, reference/source inputs, explicit overrides, and an
   optional backend choice. The .NET profile is the initial default. Compiler
   host settings are separate inputs.
2. **Resolve contract requirements.** Resolve profile identity/version and known
   compiler rules. Reject contradictory configuration; do not infer a platform
   solely from a core assembly's simple name. Select registered compatible source
   implementations. This stage can validate configuration without imported symbols.
3. **Open the symbol environment.** Materialize source sessions, dependency
   closure, identities, and source revisions. Resolve required types/protocols
   through semantic symbols. Validate their shapes against the contract. This
   produces the resolved immutable semantic configuration for the compilation.
4. **Bind.** Create source symbols and bind Raven against that environment.
   Contract-owned rules participate where they affect types, conversions, or
   feature availability. Unsupported source constructs receive diagnostics at
   their source locations; missing requirements do not silently change platform.
5. **Select and validate code generation.** Choose one of the contract's compatible
   generators. Validate ABI/representation, supported operations, required runtime
   helpers, and requested artifact/debug format. A backend choice supplied earlier
   permits earlier diagnostics; late selection still validates before writing output.
6. **Lower and emit.** Run shared language lowering where applicable, followed by
   generator-owned lowering and output production. Preserve provenance linking the
   contract, source environment, and selected generator to the result.

Contract resolution has configuration and symbol-validation stages to avoid a
cycle: source sessions do not require an already-bound application to open, and
contract type validation does not require emitting the application. Metadata-only
types must never need executable loading into the compiler host for validation.

The initial emitter may consume the current bound representation behind an
internal boundary. Inventory its CLI assumptions before naming it a universal
IR. A backend explicitly declares the input form it understands; lowering adapters
must preserve semantic meaning and have their own tests.

## Compatibility rules

| Situation | Required decision |
| --- | --- |
| Multiple sources under one contract | Validate symbol identity, reference closure, type categories, and ABI/representation agreement; resolve collisions explicitly. |
| A source cannot express the contract's required definitions | Reject its contribution with an input/configuration diagnostic; do not synthesize host substitutes. |
| A feature is unsupported by the platform | Diagnose during binding/semantic validation, even if a backend could encode an instruction sequence. |
| A platform feature is supported but a backend cannot implement it | Diagnose backend incompatibility before emission; another compatible backend may work. |
| A feature can be lowered or emulated | Support it only when the selected lowering and runtime helpers are present and validated. |
| A target framework omits an otherwise ordinary API | Use normal missing-type/member diagnostics; do not misclassify every missing library method as a language-feature restriction. |
| Contract/type environment changes | Construct a new compilation environment and rebind affected semantics. |
| Backend changes without semantic/ABI changes | Reuse semantic results only when input compatibility is established; invalidate target lowering and emission state. |
| Backend selection changes representation visible to semantics | Treat it as a contract change, not an emit-only option. |
| Debug/output-format options change | Preserve semantics if the generator supports the requested artifact; rebuild the output. |

Feature support cannot be reduced to one global boolean list. Rules may depend on
the construct, type arguments, available protocols, or backend. Examples to audit
include async/exception behavior, function values, unit, tuples, array variance,
by-reference types, reflection, generics, and memory/layout restrictions. These
are audit areas, not assertions that any particular platform supports them.

Known-target rules belong in cohesive compiler-owned platform policies. Shared
binders request semantic decisions or required capabilities instead of scattering
checks for names such as `NeoCLR.CoreProbe`. Where existing behavior is intentional,
lock it with tests before moving it into a policy. Where it is a compiler defect,
fix it rather than making the defect a contract requirement.

Use structured diagnostics for configuration, source-admission, unsupported-feature,
and backend-compatibility failures. Allocate diagnostic IDs when implementing the
specific validation; this design does not invent new IDs or redefine RAVT003.
Cancellation and violated compiler invariants retain their own failure semantics.

## Identity, lifetime, and incremental reuse

The compiler must distinguish contract semantic identity from source revisions
and backend identity. A reusable semantic environment requires the same resolved
contract/rules and equivalent source contributions, including order where lookup
precedence depends on it. A path alone is not a source revision.

Provider-private handles carry environment ownership internally. Cross-environment
references need explicit remapping or rejection. Shared sessions may outlive one
compilation, but cannot retain that compilation's symbols or binder state. Keep
reuse keys and lifetime bookkeeping compiler-private.

Changing an unresolved requirement must not reuse an older successful resolution.
Cached missing results must be invalidated when sources or contract requirements
change. Syntax-only reuse can remain independent where valid. Semantic APIs and
editor services observe one coherent compilation snapshot.

The current collection-based .NET metadata-session lifetime remains unchanged
until deterministic shared-session disposal has a separate ownership design.

## Existing code to migrate

| Current location | Observed responsibility | Migration |
| --- | --- | --- |
| `CompilationOptions` and project evaluation | Import options, runtime protocol options, core selection, variance/character/async settings | Classify each as language policy, contract semantics, source configuration, or emission option; build one resolved contract from authoritative inputs. |
| `Compilation.Setup`, `Metadata/DotNetMetadata*` | Reference selection, import context, snapshots of reference inputs | .NET source implementation plus a contract-owned symbol environment; preserve tested lookup policy until intentionally redesigned. |
| `Compilation.TargetCore.cs`, `EmitOptions` | Validate core identity and resolve emission overrides | Move semantic identity into contract resolution; emit options cannot secretly switch type universes. |
| `Compilation.cs`, `PENamedTypeSymbol`, `BoundNodeFacts` | Known neoCLR name checks for callable/tuple representations and related behavior | Inventory and relocate intentional rules to platform policy with independent regressions. |
| `CompilationSymbolLookup`, imported discovery capabilities | Semantic queries over source/imported namespaces and assemblies | Generalize by required facts; preserve provider-independent composition. |
| `Compilation.Emit.cs`, `CodeGen` | Direct code generator construction, reflection maps, CLI/debug output | Backend selection/validation entry point, then extraction of .NET-specific lowering and encoding. |
| Macro compilation/execution | Uses compiler-host contracts alongside application options | Resolve host-executable macro artifacts separately from application target selection. |

Do not mechanically wrap every current option in a new settings class. Some
fields duplicate target identity, some are backend restrictions, and some affect
language semantics. The authoritative owner must be chosen before migrating each
family. Unsupported mixed old/new configuration should fail explicitly rather
than follow undocumented override precedence.

## Implementation slices and acceptance gates

1. Inventory all consumers of the existing options and target-name checks. Record
   classification and source/semantic/backend invalidation requirements. Include
   project evaluation, CLI, macros, workspace snapshots, and semantic APIs.
2. Introduce resolved .NET contract selection internally, with the existing
   reference/type environment. Validate contradictory core requirements and keep
   normal .NET behavior covered. Convert controlled callers deliberately.
3. Define source/session contributions with a .NET implementation and an in-memory
   test source. Verify identities, cross-source references, duplicate/conflicting
   definitions, missing symbols, source revision changes, and snapshot isolation.
4. Route emission through a contract-compatible generator interface, initially
   using the existing .NET generator. Test rejected combinations and failure
   before output is written. Test double generators can establish dispatch rules
   without claiming another functioning backend.
5. Move known-platform policies in bounded families with semantic/operation and
   runtime tests. Evaluate replacing assembly-shaped symbols and CLR-specific
   type assumptions where actual target requirements demand it.

For each slice, establish its focused baseline, validate the changed behavior,
update compiler/API docs, and commit independently. Full .NET regression and
bootstrap gates remain required at their established milestones. .NET tests do
not establish execution on neoCLR or another native platform.
