---
name: raven-test-cleanup
description: Coverage-improvement workflow for Raven tests through cleanup. Use when increasing meaningful test coverage by reviewing, rewriting, repurposing, adding, deleting, or reorganizing stale tests after syntax or semantic behavior changed; when separating runtime/reflection-heavy coverage; or when proposing maintainability improvements by feature area and related behavior.
---

# Raven Test Cleanup

Use this skill when the task is primarily about increasing meaningful coverage in the Raven test suite. Cleanup is the mechanism: make tests match current language and compiler behavior, add focused coverage where behavior is missing, remove or replace stale coverage, run the relevant tests, and leave the affected test set passing.

## Start

1. Establish the test baseline once before code changes with `scripts/test-baseline.sh`.
2. Inspect the requested test files or feature area and identify the behavior each test is trying to protect and any documented behavior that lacks focused coverage.
3. Check the intended behavior in `docs/lang/spec/` first. Use `docs/lang/proposals/` and old investigations only as historical context.
4. If the spec, implementation, and tests disagree, do not assume the test is correct. Reduce the repro, compare against the spec, and either fix the compiler, update the test, or flag the inconsistency.
5. After edits, run the focused tests and iterate until they pass. Broaden validation when the cleanup touches shared helpers, compiler behavior, runtime/emit paths, or cross-feature expectations.

Use `WarningLevel=0` for ad hoc test runs.

## Related Skills

When the tests touch a specialized area, also apply the relevant Raven skill:

- `raven-feature-workflow` for syntax, semantics, lowering, operations, codegen, language feature behavior, docs, or changelog updates
- `raven-debug-compiler` for parser, binder, semantic model, lowering, emit, or runtime investigation
- `raven-test-triage` for broad test failures, stabilization, baselines, and regression locking
- `raven-lsp-debug` for hover, completion, diagnostics, semantic tokens, inlays, document symbols, request scheduling, or other language-server/editor behavior

Use the cleanup skill to increase useful coverage by deciding whether tests are stale, missing, duplicated, misplaced, or poorly asserted. Use the related skill for the domain-specific workflow and validation expectations.

## Cleanup Decisions

Classify each stale or suspicious test before editing it:

- **Keep** when it still describes current documented behavior and fails because the compiler regressed.
- **Rewrite** when the behavior is still relevant but syntax, diagnostics, APIs, or assertion style changed.
- **Repurpose** when the old scenario is obsolete but the setup covers a nearby feature gap.
- **Add** when cleanup reveals current documented behavior with no focused coverage, especially after deleting stale tests or replacing broad legacy coverage.
- **Delete** when it only encodes removed behavior, duplicate coverage, implementation scaffolding, or an assertion that is no longer meaningful.
- **Move** when it belongs in a clearer feature area, shared behavior area, or isolated runtime/reflection suite.

Prefer fixing clearly wrong compiler behavior over preserving stale expectations in tests.

When adding coverage, keep it focused on the missing behavior and place it with the feature area or compiler API surface that owns the behavior. Do not compensate for vague stale tests by adding large overlapping suites.

## Assertion Style

Prefer stable, feature-level assertions:

- parser shape and recovery for syntax behavior
- diagnostics for rejected programs
- symbol, type, metadata, and operation shape for semantic behavior
- observable runtime behavior for execution
- public semantic API behavior for language-service-facing scenarios

Avoid stable tests that assert emitted opcodes, exact lowered instruction sequences, incidental diagnostic ordering, or private implementation details. If instruction-level checks are temporarily useful during development, keep them under `test/Raven.CodeAnalysis.Tests/CodeGen/Development`.

## Organization

Reorganize tests around the behavior a maintainer would look for:

- feature areas for syntax and semantics, such as functions, properties, patterns, imports, aliases, unions, expressions, statements, type compatibility, and control flow
- cross-cutting compiler APIs, such as semantic model, operations, diagnostics analyzers, incremental compilation, workspaces, and language-server presentation
- runtime/reflection-heavy coverage in isolated locations or isolated scripts

When moving tests, preserve useful names and update namespaces to match the destination structure. Consolidate repeated setup only when it reduces real duplication without hiding the scenario under test.

## Runtime And Reflection

Keep runtime, emit, execution, and reflection-heavy tests separate from normal cleanup and baseline work.

- Run normal baseline cleanup with `scripts/test-baseline.sh`.
- Run heavy suites with `scripts/test-runtime-isolated.sh` when the change affects emit, runtime execution, metadata loading, or reflection.
- Do not turn runtime behavior into fragile opcode or lowered-shape assertions.

## Maintainability Review

When a cleanup uncovers broader structure issues, propose concrete follow-up changes:

- feature folders that better match `docs/lang/spec/`
- documentation updates for current behavior that should live in `docs/` but is missing, stale, or only implied by tests
- shared helpers for repeated compilation, diagnostics, or symbol assertions
- renamed tests that describe current behavior rather than implementation history
- removal of duplicate legacy coverage after equivalent behavior-focused tests exist
- focused new coverage for documented behaviors that were previously only implied by stale or deleted tests
- a split between fast compiler tests and runtime/reflection-heavy tests

Keep the proposal close to the touched area unless the user asked for a full test-suite redesign.

## Autonomy

Try to resolve stale tests independently by reducing examples, reading the spec, and checking nearby tests. Stop and ask for input only when the intended language behavior is genuinely unclear, the spec is contradictory, or deleting/rewriting coverage would erase a product decision rather than stale scaffolding.

## Validation

1. Run the smallest targeted test set that proves the edited tests now express the intended behavior.
2. Fix failures in that target set unless they are clearly unrelated to the cleanup and documented as pre-existing baseline noise.
3. Run the relevant baseline or isolated suite for the affected area and leave it passing relative to the established baseline.
4. Use `scripts/test-runtime-isolated.sh` for runtime, emit, execution, metadata loading, or reflection-heavy coverage.
5. Format touched C# files with `dotnet format whitespace ... --include ... --no-restore`.
   Use `dotnet format style` or `dotnet format analyzers` only when intentionally applying those fixes; analyzer/style formatters can rewrite code beyond whitespace.
6. Summarize which tests were kept, rewritten, repurposed, added, deleted, or moved, which commands were run, and call out any spec questions left unresolved.
