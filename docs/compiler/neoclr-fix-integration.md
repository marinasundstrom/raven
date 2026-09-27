# General compiler fixes extracted from the neoCLR experiment

2026-09-14. Branch `codex/compiler-fixes-integration`, based on upstream main
`d92b02812740ae052f277c23151e9cc208f7672d`. The experimental target branch remains
separate. This branch does not add neoCLR target selection, array invariance,
Void-as-a-generic-argument policy or runtime protocol renaming.

## Reviewed batches

| Batch | Original experiment commits | Ordinary .NET evidence |
| --- | --- | --- |
| Numeric operations, operator applicability and pointer substitution | `26907410f`, `c1431bea1`, `406962312`, `09cf60417`, `bc0ec8046`, `809aef0fe`, `e51da7a48` | 122 selected tests: 12 failures before applying compiler fixes; all pass afterwards. |
| Binding, dispatch, receivers, enum context and constructor completion | `a843844e4`, `6ab473fbc`, `9b269f9d0`, `62105de24`, `3142f2f13`, `55c0f7ef5`, `854cd4d3d` | 32 selected tests: nine failures before this batch; all pass afterwards. |

Changes and relevant specs were applied individually; this is not a merge of the
experimental branch. Tests for interface flags, imported indexers and expression
receivers were separated from target-specific fixtures. The method-group regression
was extracted without bringing in an unrelated explicit-Void delegate test.

The checks cover observable CLR execution, metadata flags, conversion classification,
invalid operator operands, inherited-interface ambiguity and ordinary semantic-model
completion. They do not establish that every changed path independently failed before
its fix. No emitted-opcode sequence assertions were introduced.

The fresh worktree ran `scripts/codex-build.sh`. Initial focused tests built their
project references; subsequent focused tests reused those foundational library builds
and rebuilt the compiler/test assemblies as needed. Host SDK:
`11.0.100-rc.1.26425.128`; focused execution target: `net11.0`.
After both batches, `scripts/test-baseline.sh` completed successfully: 5,489
passed, no failures or skips (including its compiler, editor and supporting test
projects). The focused execution checks above cover the changed runtime paths.
This is not a full Raven release gate or validation of NanoFramework. The branch
is prepared for review; this record does not claim it has been merged into main.

The language specifications updated with the fixes cite the C# numeric-conversion
and operator-applicability baselines. These corrections restore ordinary .NET
behavior; the experiment discovered them but does not own their semantics.

## Main integration directive — 2026-09-14

The author directed that fixes benefiting Raven generally belong on Raven main,
while experimental neoCLR support stays on a separate feature branch. This is the
workflow for future fixes too: extract and validate general behavior independently;
do not merge the experimental branch wholesale.

The namespace fixes from `f80902d70` were extracted onto this main-based integration
branch. The regression was rewritten with ordinary .NET framework references and
default compilation options, removing its dependency on experimental MetadataImportOptions.
It failed on imported-member completion before the fix. Metadata-first marker lookup
also uses the existing general reference-type resolver; no new target option is added.
The original numeric/binding batches remain unchanged.

General cross-target emission candidates listed in neoCLR's assessment still require
individual dependency review and ordinary-target evidence. They are not classified as
permanently experimental merely because neoCLR discovered them. Iteration/propagation
renaming, alternative array semantics and inhabited Void remain on the experiment
branch until separately designed and approved for Raven's general target model.

Main-integration validation: 47 focused namespace tests passed, followed by
`scripts/test-baseline.sh`: 5,490 passed, zero failures/skips. The latter includes the
new default-options namespace regression. The prior numeric and binding runtime
checks remain the evidence for those unchanged batches. This is source integration,
not a Raven release or NanoFramework certification.

## Pointer metadata emission — 2026-09-14

Extracted the pointer reconstruction fix from `3df1b54b0` on the main-based
`codex/general-pointer-emission` branch. The original default-options test already
passed on main, so it was extended to exercise the existing EmitOptions target-core
contract as well. That case failed with an unsupported `Unit*` metadata type before
the fix (six other normalizer checks passed). The change recursively preserves
pointer element types during method-reference reconstruction. It introduces no
experimental metadata import options or neoCLR-specific type semantics.

Validation: all 53 focused normalizer, pointer code-generation, pointer semantic
and pointer syntax checks passed. The compiler build and whitespace formatting
completed. This scoped check is not a new full release gate.

## Closed-generic reference metadata — 2026-09-14

Extracted `17c9f8b82` on `codex/general-generic-metadata`, based on main after the
pointer fix. The regression uses normal .NET references and default CompilationOptions;
it does not import the experimental MetadataImportOptions contract. Before the fix,
emission failed while resolving the reference-only `Contracts.Container<T>` type.
Afterwards, all 18 focused metadata and generic-invocation tests passed.

When EmitOptions selects a target core library, named metadata types and their
closed constructions remain in the metadata context. Generic member references
preserve definition parameters on a constructed owner, nested value types retain
their value-type flag, and method proxies preserve by-reference parameter shapes.
Default emission policy is unchanged. The metadata test checks signatures, locals,
nested generic getters and an out-parameter union extractor; it does not execute a
reference assembly. Mixed source/metadata constructions, constructors and generic
methods still require the subsequent independent reviews.

The repository target-framework matrix passed with SDK `11.0.100-rc.1.26425.128`,
building the libraries and building/running representative .NET 10 and .NET 11
projects with the repository toolchain. This is not the full Raven release gate.

## Reference-only constructors — 2026-09-14

Extracted `995a4c982` on the main-based `codex/general-constructor-metadata`
branch. Extending the normal-reference fixture to construct nested generic cases
and their carriers reproduced a MetadataLoadContext mismatch before the fix.
Target-core emission now allocates temporary constructor tokens and rewrites them
to the original metadata signatures in the final PE. Temporary proxy types are
removed; default constructor resolution remains unchanged. Tests assert the final
constructor signatures and absence of the temporary types, not opcode sequences.

Completed integration branches `codex/compiler-fixes-integration`,
`codex/general-pointer-emission` and `codex/general-generic-metadata` were deleted
locally and remotely after confirming they were ancestors of main. Superseded
`codex/neoclr-target-contracts` and `codex/neoclr-target-resolution` were likewise
removed after confirming their history is contained in the active
`codex/neoclr-namespace-metadata` experiment. Unrelated branches were retained.

Validation: 26 focused metadata, generic-invocation and constructor checks passed,
as did the repository .NET 10/.NET 11 build/run matrix with SDK
`11.0.100-rc.1.26425.128`. This is a scoped integration, not a full release gate.

## Closed generic method calls — 2026-09-14

Independently extracted `11e9964f2` on `codex/general-method-metadata` from main.
The regression uses ordinary .NET references and default CompilationOptions with
the existing EmitOptions target-core setting. Before the fix, calling `Echo<int>`
from a metadata-only assembly failed with a MetadataLoadContext mismatch; the
existing generic-type/constructor test still passed.

Closed generic metadata calls now use method proxies whose final MethodSpec retains
concrete type arguments and the generic definition's parameter/return signature.
Assembly scope normalization includes those method type arguments. Open generic
arguments continue through the existing resolver. Tests inspect metadata identity
and generic parameter kind/position, rather than requiring a particular opcode
sequence. This adds no neoCLR target configuration or language syntax.

Validation: all 27 focused metadata, generic-invocation and constructor checks
passed. The repository .NET 10/.NET 11 build/run matrix passed with SDK
`11.0.100-rc.1.26425.128`. This is not the full release gate.

## Closed generic fields — 2026-09-14

Independently extracted `4af98e7c1` on `codex/general-field-metadata` from main.
The default-options, ordinary-reference fixture reproduced a MetadataLoadContext
mismatch when reading `Box<int>.Value` before the fix. Field tokens now preserve
the closed declaring type and the generic definition's field signature. The
regression also covers static reads/writes and checks that temporary proxy types
are removed from the final assembly. Default emission uses the existing resolver.

The author explicitly clarified that fixes benefiting Raven on .NET Framework or
NanoFramework also belong on main. Repository instructions now preserve this
boundary: general compiler fixes on main, neoCLR-specific integration on its
experimental branch. Test results must identify the actual target; modern .NET
matrix success does not claim .NET Framework or NanoFramework execution.

Validation: all 22 focused metadata, field and generic-invocation checks passed.
The repository .NET 10/.NET 11 build/run matrix passed with SDK
`11.0.100-rc.1.26425.128`. No .NET Framework or NanoFramework runtime test was run.

## Generic delegate construction — 2026-09-14

Extracted `5f274c063` independently on `codex/general-delegate-metadata` from main.
The normal-reference, default-CompilationOptions regression failed before the fix
while constructing a metadata-only `Callback<int>` delegate. Delegate normalization
now preserves an existing metadata constructor proxy instead of re-resolving it
through reflection and mixing compiler-host and metadata-context types. The final
constructor retains its target assembly, closed owner and Object/IntPtr signature.
This is a general emitter correction and introduces no neoCLR-specific mapping.

The intended target architecture is reusable compiler mechanisms with explicit
framework-contract mappings; neoCLR's experimental configuration and unresolved
semantics stay separate. A compiler defect discovered through neoCLR remains a
general fix when the same metadata contract applies to other frameworks.

The author further clarified that neoCLR-specific tests and mappings must not enter
main yet. These fixtures use ordinary CLI metadata contracts and no neoCLR options.
A possible future emission backend is an open evaluation, outside this stabilization.

The delegate candidate passed 26 focused tests and the modern .NET target matrix
before it was set aside for the [main stability audit](main-stability-audit.md).
That audit found and corrected an attribute serialization regression at `5a67d5d4c`;
the delegate fix was then restored on top for combined focused validation.

Combined validation after restoring the delegate fix: 55 focused metadata, attribute,
delegate and generic-call tests passed. The broader audit remains tied to its stated
commits; it is not reported as a rerun on this delegate commit.

## Direct out-parameter forwarding (2026-09-19)

A direct invocation that passes an enclosing out parameter to a callee out parameter
establishes assignment on normal return. Previously the method-body checker tracked
explicit assignments but missed this call guarantee, producing RAV0269. The fix uses
CLI parameter ref kinds; ref/in arguments do not provide the same guarantee.

The independent regression uses a source generic setter and .NET Math.DivRem,
then executes the emitted code and observes 42. It failed with RAV0269 before the
fix. All 41 focused parameter semantic/runtime checks pass, including deferred and
conditional forwarding that must still report missing assignment. This is limited
to direct invocation expressions; it does not redesign flow analysis for calls
nested in conditional expressions. No Runtime Contract option or neoCLR policy is
introduced. .NET Framework and NanoFramework were not executed.

## Same-name constructor arities (2026-09-19)

Unqualified constructor lookup must retain all accessible named types in the current
namespace before choosing a generic arity. A namespace containing both Box and
Box<T> previously selected Box alone: Box<T>.Create calling Box<T>(value) silently
bound an error expression and returned a default value. The independent ordinary
.NET execution regression returned 0 before the correction and 42 afterward.
Invalid constructor arities and extra type arguments on a constructed alias now
produce RAV0305, preventing emission of these unresolved expressions.

The fix changes candidate discovery and diagnostics, not emitted metadata contracts.
It introduces no Runtime Contract setting or neoCLR policy. All 78 focused generic,
alias, namespace and accessibility checks pass on .NET 11. .NET Framework and
NanoFramework were not executed. Namespace access checks and candidate deduplication
remain in force; this does not claim to resolve every nested-case lookup issue.

## Enum backing-field metadata (2026-09-19)

The final PE metadata pass preserves `SpecialName | RTSpecialName` on the instance
`value__` field of CLI enums, as required by ECMA-335 II.14.3. Raven already requests
both bits when defining the field, but the host PersistedAssemblyBuilder masks
reserved field attributes. The correction applies to normal and explicitly retargeted
emission, including nested enum definitions, without changing source semantics,
Runtime Contract configuration or underlying integral storage.

An independent ordinary .NET regression inspects the emitted PE field attributes,
then loads the enum and checks its underlying type and literal value. It failed
before the correction; all 13 focused enum/target-core checks pass on .NET 11.
.NET Framework and NanoFramework execution are not established by these checks.
The final metadata normalization is necessary while the host emission layer drops
the bit; it does not admit malformed enums or add target-specific enum policies.

Sources: [CLI standard](https://ecma-international.org/publications-and-standards/standards/ecma-335/),
[Persisted field builder](https://github.com/dotnet/runtime/blob/main/src/libraries/System.Reflection.Emit/src/System/Reflection/Emit/FieldBuilderImpl.cs).

## Async method shared captures (2026-09-27)

Isolated on `codex/async-mutable-captures`, based on main `2e04d3f4e`.
Ordinary .NET regressions reproduce the same storage failure that neoCLR exposed:
when a callback writes an async caller's mutable local, the caller and a second
callback can observe separate values. The direct and `Task.Run` suspension cases
reported `0` and `2` instead of `42` and `42` before the fix. Ten neighboring checks
passed in that baseline.

State-machine method emission now creates one retained shared closure per invocation.
The reference survives state-machine copies and resumptions. Existing capture emission
handles both caller and callback accesses; a scoped rewrite redirects the caller's
previously hoisted local field accesses, assignments and addresses to that shared
storage. Parameters and the receiver are initialized from state-machine storage.
Already synthesized local fields can remain unused; removing those fields is an
emission cleanup, not a correctness requirement for this slice.

This restores the shared-variable semantics described in the
[function specification](../lang/spec/functions.md#captured-values), matching the
.NET closure model. It adds a private closure-reference field to the generated
state machine, with no source API or Runtime Contract configuration changes and no
neoCLR policy dependency. Capturing a variable does not make simultaneous writes
atomic or synchronized.

The [generic-method repro](development/async-generic-capture.rvn) initially exposed
a separate metadata-normalization failure. The subsequent generic-capture slice
below repairs that method/type-parameter ownership path. Async-lambda-owned
locals and iterator closure planning are also outside this source-method fix.

Validation: all 17 focused async/capture runtime checks pass on .NET 11, including
caller/callback writes, sibling callbacks, forced suspension, `.NET Task.Run`,
per-invocation lifetime, receiver/parameter captures and local functions. The latter
also requires state-machine call emission to look up the host method’s closure.
Whitespace formatting and the compiler build pass. Bootstrap used
`scripts/codex-build.sh`; after the compiler and core builds, unrelated repeated
macro/core builds were stopped and the unchanged main macro artifact was reused.
Subsequent checks rebuild only the compiler and tests. No full suite or target
matrix is implied; .NET Framework and NanoFramework execution remain unverified.


## Simple imported type receivers (2026-09-27)

Isolated on `codex/imported-type-arity` from main. An ordinary C# reference assembly
containing Task<T> before a nongeneric static Task reproduces RAV0117 for Raven's
wildcard-imported `Task.Run()`. Reversing metadata declaration order passes. This
is independent of neoCLR contracts, async lowering and Task.Run scheduling.

BlockBinder simple-symbol lookup now prefers a nongeneric type from the same
declaring scope when the first candidate is generic. It retains the first local,
parameter or alias and does not replace it with an imported type. Explicit generic
annotations continue selecting their arity. This aligns simple static receivers
with existing Raven type lookup and the familiar CLR nongeneric/generic type-family
model; no Runtime Contract configuration or metadata format changes are introduced.

The regression emits and executes both metadata orders, requires Run to return 42,
and checks generic parameter/return signatures plus local and parameter shadowing.
The original generic-first case failed before the fix. All 28 focused regression,
import, alias and constructor lookup checks pass; .NET Framework and
NanoFramework execution is not claimed. The independent fix can be integrated into
main and then individually into neoclr; the target consumer remains neoCLR evidence.


## Overloaded block-lambda return inference (2026-09-27)

Isolated on `codex/task-run-block-lambda` from main. Ordinary .NET
`Task.Run<int>(() => { let value = 40; return value + 2 })` reported RAV1503,
converting int to unit, before overload resolution selected Func<int>. Initial
argument binding had used the first delegate overload (Action) as a target even
when the candidates disagreed.

Unannotated synchronous lambdas retain the initial delegate parameter hint, but
infer their return independently when that hint is completion-only and another
candidate can return a value. A unique target or explicit return annotation still
controls return checking. Existing common-target handling and overload replay remain
responsible for final shaping. Method groups and async lambda rules are unchanged. This follows the CLR ergonomic expectation
that a value-returning callback selects the appropriate Task.Run overload; it does
not change async scheduling, emitted metadata contracts or Runtime Contract options.

The ordinary CLR regression executes block callbacks with explicit and inferred
type arguments, completion-only work and a captured object. A unique Action target
still diagnoses an explicit integer return. All 64 focused regression, lambda inference,
async Task.Run and expression-tree checks pass, covering the changed target-selection path.
The general fix is suitable for main and individual integration into neoclr;
.NET Framework and NanoFramework execution is not claimed.


## Generic async capture parameter ownership — 2026-09-27

The ordinary .NET reproduction passed source diagnostics but failed CLI metadata
normalization: captured `T` fields and closure references in `MoveNext` retained
method-owned generic slots although their owners were synthesized types. This was
invalid Raven output, not a neoCLR runtime limitation or a target policy choice.

Closure field definition now temporarily maps source method parameters to the
closure's own type parameters. Async state-machine body emission maps them to its
state-machine parameters, including references built from original capture symbols.
The alias scopes always push and restore their mappings, including nested scopes
with identical values. This follows normal CLI generic ownership and .NET closure
semantics; source syntax, semantic-model contracts and Runtime Contract options
are unchanged. The implementation belongs on main and is individually applicable
to neoclr. No importer validation is weakened.

All 11 focused runtime checks pass, covering scalar and array captures with both value and
reference substitutions, alongside existing shared-capture and generic closure
consumers. The original compiler failed the scalar regression during normalization;
the corrected compiler emits and executes it. .NET Framework and NanoFramework
execution is not claimed.

A separate [generic containing-type reproduction](development/async-generic-containing-type.rvn)
fails with a state-machine generic-arity TypeLoadException on ordinary .NET even
without a capture. That async state-machine construction issue is retained for an
independent follow-up. Async-lambda-owned locals and iterator capture planning also
remain outside this bounded source-method fix.
