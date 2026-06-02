---
name: raven-test-triage
description: Testing and stabilization workflow for the Raven compiler test suite. Use when establishing baselines, fixing failing tests, isolating runtime and emission-heavy failures, validating binder/semantic-model regressions, testing incremental compilation behavior, or converting unstable codegen assertions into behavior-focused coverage.
---

# Raven Test Triage

Use this skill when the task is primarily about test failures, stabilization, or regression coverage.

## Baseline Strategy

For large stabilization passes, use a planned task sequence: define the current
test or behavior slice, keep one step active, verify it before moving on, and
re-evaluate the next slice after each result. Tell the user what the next step
is after each verified slice. Prefer a reduced compiler test that locks the
invariant first, then broaden to LSP, sample builds, baseline, or runtime suites
only when the affected layer warrants it.

Establish the baseline with:

```bash
scripts/test-baseline.sh
```

This excludes runtime or emission-heavy noise from the initial stabilization pass.

Run isolated heavy suites separately with:

```bash
scripts/test-runtime-isolated.sh
```

Use `WarningLevel=0` for ad hoc test runs.

## Build Decision

Run `scripts/codex-build.sh` only when needed:
- first compile in a fresh workspace
- after changing generator inputs, syntax models, bound models, or related generated artifacts

Otherwise prefer targeted builds and targeted test runs.

## Test Philosophy

Prefer stable assertions:
- diagnostics
- symbol shape
- metadata shape
- operation shape
- observable runtime behavior
- binder-owned state when testing binder lifecycle, local/parameter ownership, scope contents, or binder-produced diagnostics
- public semantic API behavior when testing language-service-facing answers such as symbol lookup, type info, declared symbols, and diagnostics before/after edits

Avoid asserting:
- emitted opcodes
- exact lowered instruction sequences
- fragile internal IL shapes for stabilized features

## Legacy Codegen Tests

Treat legacy emitted-opcode and lowered-shape tests as unstable scaffolding.

When touching such tests:
- remove or replace them with behavior-focused coverage when possible
- if temporary instruction-level checks are still useful during development, move or keep them under `test/Raven.CodeAnalysis.Tests/CodeGen/Development`

Development-only codegen tests should stay out of normal baseline or runtime stabilization passes.

## Regression Locking

Compiler bug fixes must be locked with focused tests.
Choose the narrowest layer that proves the bug is fixed:
- parser issue: syntax test
- semantic issue: binding or diagnostic test
- binder ownership issue: binder or semantic-model test for the responsible scope
- incremental issue: before/after semantic-model or diagnostics test that proves stale binder state is not reused
- language-service issue: compiler API test first, then LSP presentation/scheduling test if editor behavior is also involved
- operation modeling issue: operations test
- runtime behavior issue: runtime or codegen behavior test
- lazy-binding issue: test both a cold semantic query and, when relevant, a second query that proves the first query populated compiler-owned state for reuse
- available-state optimization: include a negative or ambiguous case proving the code falls back to normal full binding instead of returning a partial or guessed answer
- cross-file incremental issue: add or update another document in the same project, then query symbols/diagnostics in the original document through the current compilation snapshot
- LSP scheduling issue: test that foreground semantic requests are not blocked by broad background gates, while background diagnostics/analyzers/inlays can skip, cancel, or requeue
- binder lifecycle issue: prove invalidated binders lose their owned symbols and diagnostics, and unchanged binders keep valid derived state only when their syntax and semantic context are equivalent

Keep documentation in step with stabilized behavior. When test triage exposes behavior that should be documented but is missing from `docs/`, consider adding the documentation instead of leaving the expectation only in tests.

`ITestOutputHelper` may be used for targeted diagnostics during investigation.

## Validation

1. Run the smallest targeted test set that proves the fix.
2. Run the appropriate baseline or isolated suite for the affected area.
3. Clean up outdated assertions in the touched area instead of preserving obviously stale expectations.
