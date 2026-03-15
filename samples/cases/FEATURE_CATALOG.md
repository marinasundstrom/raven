# Feature Catalog for Sample Promotion

This catalog groups `samples/*.rav` by feature intent so we can promote them into stable, named feature cases.

## Spec-aligned case tracks
- `docs/lang/spec/control-flow.md` -> Control flow and pattern matching cases (`samples/control-flow/*`, `samples/patterns/*`, and control-flow root demos).
- `docs/lang/spec/error-handling.md` -> Error/exception/propagation cases (`Result`, `Option`, `try`, `throw`, and async propagation samples).
- `docs/lang/spec/type-system.md` -> Type system cases (unions, nullability, tuples, function types, generics, collections, unit).
- `docs/lang/spec/classes-and-members.md` -> OOP/member/declaration cases (classes, interfaces, records, enums, inheritance, extensions).

Pattern note:
- `match` **statement** is categorized under control flow.
- `match` **expression** is categorized under pattern expressions.
- Both remain pattern-driven features.

## 1) Getting Started
- `samples/entrypoints/main-top-level-basic.rav`
- `samples/entrypoints/main-function-sync-basic.rav`
- `samples/entrypoints/main-function-async-task-basic.rav`
- `samples/entrypoints/main-static-method-sync-basic.rav`
- `samples/entrypoints/main-static-method-async-task-basic.rav`
- `samples/cases/ledger-shaping-linq-summary.rav`
- `samples/cases/status-ledger-enum-summary.rav`
- `samples/control-flow/while-loop-list-processing-basic.rav`

## 2) Control Flow Statements (`docs/lang/spec/control-flow.md`)
- `samples/control-flow/match-statement.rav`
- `samples/control-flow/if-expression-in-function.rav`
- `samples/control-flow/if-statement-implicit-return.rav`
- `samples/result-and-options/null-coalescing-guard-result-basic.rav`
- `samples/control-flow/for-loop.rav` (canonical collection-based `for` loop)
- `samples/control-flow/for-range.rav` (`for` loop with range expression)

## 3) Pattern Expressions and Matching (expression-oriented patterns)
- `samples/patterns/match-expression.rav` (match expression)
- `samples/patterns/match-type-pattern-basic.rav`
- `samples/tuples/tuple-pattern-match-basic.rav`
- `samples/patterns/is-constant-pattern-basic.rav`
- `samples/patterns/range-pattern-basic.rav`
- `samples/patterns/property-pattern-basic.rav`
- `samples/patterns/match-nullable-pattern-basic.rav`
- `samples/patterns/collection-pattern-basic.rav`
- `samples/patterns/sequence-segment-patterns.rav`
- `samples/patterns/try-expression-match.rav`
- `samples/cases/character-filter-patterns.rav`

## 4) Type Unions, DUs, and Records (`docs/lang/spec/type-system.md`)
- `samples/discriminated-union/union-basic.rav`
- `samples/discriminated-union/union-case-construction-basic.rav`
- `samples/discriminated-union/union-shape-match-basic.rav`
- `samples/discriminated-union/union-result-case-patterns.rav`
- `samples/discriminated-union/union-generic-result-basic.rav`
- `samples/discriminated-union/union-generic-result-option-mapping.rav`
- `samples/patterns/record-pattern-basic.rav`
- `samples/discriminated-union/union-cases-generic-basic.rav`

## 5) Error Handling and Propagation (`docs/lang/spec/error-handling.md`)
- `samples/result-and-options/result-basic.rav`
- `samples/result-and-options/result-unit-basic.rav`
- `samples/result-and-options/option-basic.rav`
- `samples/result-and-options/either-basic.rav`
- `samples/result-and-options/option-propagation-basic.rav`
- `samples/result-and-options/result-propagation-basic.rav`
- `samples/result-and-options/result-propagation-try-expression-basic.rav`
- `samples/result-and-options/result-propagation-throwing-call-basic.rav`
- `samples/result-and-options/result-extensions-linq-style.rav`
- `samples/async/async-result-propagation-error-path.rav`
- `samples/async/async-result-propagation-using-success.rav`
- `samples/result-and-options/result-parse-match-basic.rav`
- `samples/result-and-options/result-parse-static-helper-basic.rav`
- `samples/result-and-options/result-error-state-basic.rav`
- `samples/result-and-options/result-propagation-double-optional.rav`
- `samples/result-and-options/result-propagation-member-access-default.rav`
- `samples/result-and-options/nullable-to-result-match.rav`
- `samples/result-and-options/result-option-item-name-sync.rav`
- `samples/result-and-options/result-option-item-name-async-propagation.rav`
- `samples/result-and-options/option-user-match-async.rav`
- `samples/result-and-options/process-numbers-result-propagation.rav`
- `samples/result-and-options/option-isokor-to-result-basic.rav`
- `samples/result-and-options/option-result-state-flags.rav`

## 6) Async and Await (cross-cuts control flow + error handling)
- `samples/async/async-await.rav`
- `samples/async/async-task-return.rav`
- `samples/async/async-valuetask.rav`
- `samples/async/async-file-io.rav`
- `samples/async/async-generic-task-return.rav`
- `samples/async/async-await-inference.rav`
- `samples/async/async-option-invocation-basic.rav`
- `samples/async/async-option-invocation-null-check.rav`
- `samples/async/async-try-catch.rav`
- `samples/async/async-result-workflow-basic.rav`
- `samples/async/async-try-match-expression.rav`
- `samples/async/http-client.rav`
- `samples/async/http-client-result.rav`
- `samples/async/http-client-result-extension.rav`
- `samples/async/http-client-result-propagation.rav`

## 7) Classes, Members, and OOP (`docs/lang/spec/classes-and-members.md`)
- `samples/oop/class-basic.rav`
- `samples/oop/class-members-advanced.rav`
- `samples/oop/class-inheritance-overrides-basic.rav`
- `samples/oop/interface-disposable-basic.rav`
- `samples/cases/order-handler-interface-async.rav`
- `samples/oop/property-override-compatibility-basic.rav`
- `samples/oop/event-basic.rav`
- `samples/oop/enum-basic.rav`
- `samples/oop/enum-explicit-values-basic.rav`
- `samples/oop/sealed-class-hierarchy-pattern-basic.rav`
- `samples/oop/sealed-record-hierarchy-match-basic.rav`
- `samples/oop/sealed-record-hierarchy-json-basic.rav`
- `samples/nullability/nullable-reference-option-conversion-basic.rav`
- `samples/nullability/nullable-value-option-conversion-basic.rav`
- `samples/oop/required-members-basic.rav`
- `samples/oop/with-expression-record-basic.rav`
- `samples/oop/with-expression-record-advanced.rav`
- `samples/oop/init-only-property-basic.rav`
- `samples/tuples/tuples-basic.rav`
- `samples/tuples/tuples-pattern-match.rav`
- `samples/tuples/tuples-nullable-match.rav`
- `samples/tuples/tuples-return-from-function.rav`
- `samples/types/unit-type-basic.rav`

## 8) Functions, Generics, Extensions, and LINQ (`docs/lang/spec/type-system.md`)
- `samples/functions/lambda-basic.rav`
- `samples/functions/delegate-basic.rav`
- `samples/functions/function-types-basic.rav`
- `samples/functions/function-reference-basic.rav`
- `samples/functions/generator-yield-basic.rav`
- `samples/generics/generic-list-basic.rav`
- `samples/generics/generic-box-basic.rav`
- `samples/generics/generic-method-type-arguments-basic.rav`
- `samples/generics/nested-generic-types-basic.rav`
- `samples/extensions/extension-methods-basic.rav`
- `samples/extensions/extension-method-overload-resolution.rav`
- `samples/extensions/static-extensions.rav`
- `samples/extensions/trait-invocation-null-conditional.rav`
- `samples/linq/linq-result-first-or-error-basic.rav`
- `samples/linq/linq-result-first-or-error-union-branching.rav`
- `samples/linq/linq-result-first-or-error-try-expression.rav`
- `samples/linq/linq-where-basic.rav`
- `samples/linq/linq-count-predicate.rav`
- `samples/linq/linq-option-result-bridging.rav`
- `samples/linq/linq-select-option-projection.rav`
- `samples/cases/quote-summary-linq-result-option.rav`

## 9) Interop, Runtime, and Standard Library Usage (`docs/lang/spec/dotnet-implementation.md`)
- `samples/runtime/io-basic.rav`
- `samples/runtime/json-basic.rav`
- `samples/runtime/reflection-basic.rav`
- `samples/runtime/expression-trees-basic.rav`
- `samples/runtime/object-dumper-extension-basic.rav`
- `samples/runtime/raven-api-syntax-tree-basic.rav`
- `samples/runtime/documentation-comment-basic.rav`
- `samples/runtime/dsl-window-builder-basic.rav`
- `samples/runtime/nameof-basic.rav`
- `samples/strings/string-interpolation-basic.rav`
- `samples/strings/string-interpolation-expression-basic.rav`
- `samples/strings/string-interpolation-multiline-basic.rav`
- `samples/data/array-basic.rav`
- `samples/data/collection-spread-basic.rav`
- `samples/runtime/parse-number-loop-basic.rav`

## 10) Unmanaged and Low-Level
- `samples/unmanaged/pointers.rav`
- `samples/unmanaged/test.rav`
- `samples/literals/type-suffixes-basic.rav`

## 11) Project System and Integration
- `samples/projects/nuget-demo/src/main.rav`
- `samples/projects/aspnet-minimal-api/src/main.rav`
- `samples/projects/runtime-async-net11/src/main.rav`
- `samples/projects/analyzer-editorconfig/src/main.rav`
- `samples/projects/efcore-expression-trees/src/main.rav`
- `samples/projects/raven-msbuild-integration/src/raven/main.rav`

## 12) Composed Multi-Feature Cases
- `samples/cases/platform-tour-composed.rav`

## 13) Explicit Bug Repros (keep separate from feature demos)
- `samples/bugs/prio1/operator-bug.rav`
- `samples/bugs/advanced/generic-math.rav`
- `samples/bugs/advanced/tokenizer.rav`

## 14) Scratch / Legacy / Rename Candidates
These should not be promoted as feature cases until renamed and scoped:
- `samples/legacy/bar.rav`
- `samples/legacy/bar2.rav`
- `samples/legacy/bar4.rav`
- `samples/legacy/foo2.rav`
- `samples/legacy/foo3.rav`
- `samples/legacy/hallo.rav`
- `samples/legacy/hallo2.rav`
- `samples/legacy/sample100.rav`
- `samples/legacy/sample101.rav`
- `samples/legacy/sample102.rav`
- `samples/legacy/sample103.rav`
- `samples/legacy/t.rav`
- `samples/legacy/test.rav`
- `samples/legacy/test2.rav`
- `samples/legacy/test3.rav`
- `samples/legacy/test9.rav`
- `samples/legacy/test10.rav`
- `samples/legacy/test11.rav`
- `samples/legacy/test20.rav`
- `samples/legacy/test23.rav`
- `samples/legacy/test100.rav`
- `samples/legacy/test101.rav`
- `samples/legacy/testar.rav`
- `samples/legacy/testar2.rav`
- `samples/legacy/testar2323.rav`
- `samples/legacy/testo.rav`
- `samples/legacy/testo2.rav`
- `samples/legacy/editor-test.rav`
- `samples/legacy/constr.rav`
- `samples/projects/target-framework-attribute-basic.rav`

## Promotion Rule
Promote to proper cases only when all are true:
- File name is feature-specific (no `test*`, `sample*`, `foo*`, `bar*`).
- Demonstrates one primary feature clearly.
- Has stable expected compile/run behavior.
- Includes a short header comment describing the feature being demonstrated.

## Notes
- Use `samples/control-flow/for-range.rav` as the canonical for-range sample.
