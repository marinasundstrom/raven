# Sample Case Strategy

Goal: keep samples useful for both docs and compiler safety.

Core purpose of samples (all three at once):
- Demonstrate language features clearly.
- Serve as runnable checks for those features.
- Expose regressions quickly when compiler behavior breaks.

## Rules
- Keep **one canonical sample file per concept**.
- Keep additional files only when they test a **distinct axis** (edge case, lowering path, interop shape, parser ambiguity, async/runtime variant).
- Separate conceptual demos from regression/smoke coverage.
- Each concept file should use small realistic domain/business-flavored code, while keeping one clear feature as the focus.
- Do not add explicit "business scenario" narration comments unless needed for clarity.
- Prefer the simplest code that demonstrates the feature; add helper models only when they improve clarity.
- Scale scenario richness with feature complexity: keep basic features minimal, and allow advanced features to mix related constructs when the primary feature remains obvious.
- Always present the simplest/common feature form first; place advanced/specialized variants in separate files.

## Two-track model
1. **Concept cases (primary)**
- Stable, named by concept.
- Used for docs and feature verification.

2. **Regression coverage (secondary)**
- Allowed to overlap conceptually.
- Exists to catch breakages in parsing/binding/lowering/emission/runtime.
- Includes bug repros, legacy compatibility shapes, and stress samples.

## Canonical concept set (one file per concept)
- Top-level execution: `samples/entrypoints/main-top-level-basic.rav`
- Function declaration: `samples/entrypoints/main-function-sync-basic.rav`
- Async function entry: `samples/entrypoints/main-function-async-task-basic.rav`
- Class-based hello world: `samples/entrypoints/main-static-method-sync-basic.rav`
- Class-based async hello world: `samples/entrypoints/main-static-method-async-task-basic.rav`
- If expression: `samples/control-flow/if-expression-in-function.rav`
- Match statement (control flow): `samples/control-flow/match-statement.rav`
- Match expression (patterns): `samples/patterns/match-expression.rav`
- For loop (collection iteration): `samples/control-flow/for-loop.rav`
- For-range loop: `samples/control-flow/for-range.rav`
- Range pattern: `samples/patterns/range-pattern-basic.rav`
- Property patterns: `samples/patterns/property-pattern-basic.rav`
- Try expression + pattern matching: `samples/patterns/try-expression-match.rav`
- Union basics: `samples/unions/union-basic.rav`
- Generic unions: `samples/unions/union-generic-result-basic.rav`
- Result basics: `samples/result-and-options/result-basic.rav`
- Option basics: `samples/result-and-options/option-basic.rav`
- Propagation (`?`): `samples/result-and-options/result-propagation-basic.rav`
- Async propagation: `samples/async/async-result-propagation-using-success.rav`
- Async/await: `samples/async/async-await.rav`
- ValueTask: `samples/async/async-valuetask.rav`
- Classes: `samples/oop/class-basic.rav`
- Interfaces: `samples/oop/interface-disposable-basic.rav`
- Inheritance: `samples/oop/property-override-compatibility-basic.rav`
- Enums: `samples/oop/enum-basic.rav`
- Sealed hierarchies: `samples/oop/sealed-record-hierarchy-json-basic.rav`
- Records: `samples/patterns/record-pattern-basic.rav`
- Tuples: `samples/tuples/tuples-basic.rav`
- Nullability (reference): `samples/nullability/nullable-reference-option-conversion-basic.rav`
- Nullability (value): `samples/nullability/nullable-value-option-conversion-basic.rav`
- Function types: `samples/functions/function-types-basic.rav`
- Lambdas: `samples/functions/lambda-basic.rav`
- Delegates: `samples/functions/delegate-basic.rav`
- Generics: `samples/generics/generic-list-basic.rav`
- Extensions: `samples/extensions/extension-methods-basic.rav`
- LINQ: `samples/linq/linq-where-basic.rav`
- Reflection interop: `samples/runtime/reflection-basic.rav`
- Expression trees: `samples/runtime/expression-trees-basic.rav`
- JSON interop: `samples/runtime/json-basic.rav`
- Pointers/unmanaged: `samples/unmanaged/pointers.rav`

## Distinctness criteria for keeping extra files
Keep a second file for the same concept only if it adds at least one of:
- Different syntax form (statement vs expression).
- Different type-system behavior (nominal type vs nullable).
- Different lowering/codegen path (sync vs async, iterator, exception path, runtime-async).
- Different interop boundary (.NET APIs, project file restore, target framework).
- Previously observed regression signature (linked issue/bug repro).

## Practical cleanup approach
- Keep canonical files as the source of truth for feature demos.
- Move duplicate legacy files into an explicit regression bucket over time.
- Rename non-descriptive files (`test*`, `sample*`, `foo*`, `bar*`) when promoted.
- Do not delete historical regressions unless equivalent coverage exists and passes.
