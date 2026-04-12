---
name: raven-test-triage
description: Testing and stabilization workflow for the Raven compiler test suite. Use when establishing baselines, fixing failing tests, isolating runtime and emission-heavy failures, or converting unstable codegen assertions into behavior-focused coverage.
---

# Raven Test Triage

Use this skill when the task is primarily about test failures, stabilization, or regression coverage.

## Baseline Strategy

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
- operation modeling issue: operations test
- runtime behavior issue: runtime or codegen behavior test

`ITestOutputHelper` may be used for targeted diagnostics during investigation.

## Validation

1. Run the smallest targeted test set that proves the fix.
2. Run the appropriate baseline or isolated suite for the affected area.
3. Clean up outdated assertions in the touched area instead of preserving obviously stale expectations.
