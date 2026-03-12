# Raven Changelog

Behavior-focused timeline covering **2025-09-12** to **2026-03-12**.

## 2026-03-12

### Added
- Expanded Operations API coverage for newer language constructs and bound nodes.
- Added targeted sample coverage around generic parsing with static interface constraints.
- Added style analyzer + code fix to convert expression-bodied members to block-bodied form.

Impact:
- Compiler API consumers can inspect more semantics directly.
- Regressions in generic-constraint scenarios are easier to catch with samples.

### Changed
- Null-assignment diagnostics were tightened and message quality improved (clearer assignment errors and hint formatting).
- Static interface member resolution and generic constraint checks were corrected for `IParsable<T>`-style flows.
- Cascade behavior after failed generic binding was reduced to avoid misleading downstream errors.
- Generic method calls with explicit type arguments now follow C# more closely by skipping extra method-type inference passes for later lambda arguments, and overload reporting suppresses more downstream cascades when an argument already carries an error type.
- Several binder/codegen regression fixes landed (including interpolation/object-dumper/runtime sample paths).
- Hover/signature display for promoted primary-constructor parameters now preserves binding keyword semantics (`val`/`var`) when the parameter maps to a property.
- Compiler projects were retargeted from `net10.0` to `net10.0` (including build scripts/default framework switches).

Impact:
- Fewer false diagnostics and better first-error quality.
- Fewer compile-success/runtime-fail scenarios in generic and interpolation-heavy code.

### Removed
- Removed stale/incorrect operation naming in favor of updated terminology alignment (for example, moving from switch-centric naming toward match-centric naming where applicable).

Impact:
- Operations API is more consistent with current language semantics.

---

## 2026-03 (early to mid)

### Added
- Added destructuring and pattern expressiveness upgrades: nested patterns, explicit value patterns, sequence deconstruction support across more shapes.
- Added collection builder support and spread/target-type inference improvements.
- Added analyzers/code fixes for expression-body preferences and diagnostic suppression directives.

Impact:
- Pattern-based code became more expressive and concise.
- Collection inference became more predictable in real-world generic code.

### Changed
- Function syntax direction shifted toward first-class function expressions and updated signature/hint presentation.
- Parameter deconstruction support expanded (including lambda parameter deconstruction).
- Parser hardening for argument lists and continuation/newline-sensitive forms.

Impact:
- Improved ergonomics for functional style and lambda-heavy APIs.
- Reduced parser drift on edge-case call syntaxes.

### Removed
- Removed residual syntax/display traces that no longer match current function and parameter terminology.

Impact:
- Tooling output better matches current language surface.

---

## 2026-02

### Added
- Added/expanded language server capabilities: hover docs, completions, signature help, code actions, symbol outline, logging hooks.
- Added project-system and runtime integration work: framework references, NuGet support, output layout improvements, .editorconfig participation.
- Added async runtime support/stabilization work (including runtime async and ValueTask-oriented paths).
- Added richer pattern and control-flow support: range patterns, guarded matching improvements, return-expression and throw-expression support.
- Added OOP surface enhancements: abstract classes, interface support maturation, property/accessor and constructor-related semantics.

Impact:
- Authoring/debugging experience improved materially in editor workflows.
- More practical .NET integration for non-trivial Raven projects.
- Broader set of control-flow/pattern constructs compile and run reliably.

### Changed
- Match semantics and exhaustiveness checks were repeatedly hardened (including diagnostics and generic display improvements).
- Async lowering behavior was stabilized across edge cases (implicit return interactions, try/catch flows, lambda paths).
- Accessibility/default-member behavior and declaration rules evolved, with related diagnostic updates.

Impact:
- Fewer runtime surprises in async/match heavy code.
- Stricter, clearer declaration behavior for class members and access control.

### Removed
- Removed type unions and type literals from active language surface (and associated normalization/parsing paths).
- Removed named constructors feature.
- Removed legacy `Try*` LINQ extension route.

Impact:
- Breaking change for code depending on union/literal type syntax.
- Language surface became narrower and easier to stabilize.

---

## 2025-09 (from 2025-09-12 onward)

### Added
- Added generics foundation and constraints across types/methods.
- Added interface declarations and base-list support for classes/interfaces.
- Added extension-method consumption and lowering support (including staged parity improvements).
- Added Operations API initial infrastructure.
- Added attribute support across assembly/type/member/parameter/return contexts.
- Added control-flow/codegen support for break/continue, goto/labels, and more lowering targets.
- Added CLI and diagnostics tooling improvements (`-bt`, diagnostics-only highlighting, source-symbol/bound dumps).

Impact:
- Major expansion in language expressiveness and tooling introspection.
- Better parity with .NET expectations for attributes, interfaces, and generic constraints.

### Changed
- Overload resolution and conversion logic was hardened (nullable/lambda/extension interactions, byref matching, generic substitution paths).
- Match lowering and diagnostics were corrected for null/literal/value-type cases and exhaustiveness flows.
- Parser robustness improved for rewinds, continuations, skipped tokens, and missing-terminator recovery.

Impact:
- More deterministic binding decisions.
- Better diagnostic precision and fewer parser-induced semantic cascades.

### Removed
- Removed or phased out unstable/unsupported intermediate behavior around extension and union-related paths as the model converged.

Impact:
- Some experimental edge behavior no longer compiles; diagnostics are now more explicit.

---

## Migration Notes

- If old code assigns `null` to non-nullable types, migrate to nullable/optional forms.
- If old code uses union/type-literal syntax, migrate to current Raven constructs.
- Re-check overload-heavy calls (especially lambdas/extensions/generics) because binder behavior is now stricter and more correct.
- For compiler API integrations, prefer current Operations API names/shapes aligned to match-oriented semantics.
