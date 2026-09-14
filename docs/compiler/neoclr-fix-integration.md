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
