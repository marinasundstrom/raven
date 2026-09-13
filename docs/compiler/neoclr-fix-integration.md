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
