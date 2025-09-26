# Language Feature Implementation Checklist

Use this checklist to plan the work required for a new language feature. Each step lists when it should be considered and how it
relates to Raven's compiler architecture.

## 1. Shape the feature
- **Design review** — Validate the feature against existing language goals and the specification. Consider this when the feature
  changes syntax or semantics in a way that might conflict with prior design principles.
- **Feature flag or versioning strategy** — Decide whether the feature needs to be guarded behind a flag or staged rollout. Use
  this when the change is experimental or may need to ship disabled by default.

## 2. Update the specification and docs
- **Language specification** — Amend grammar, examples, and explanations. Do this whenever the user-facing behavior changes.
- **Compiler docs** — Document the compiler changes (parser, binder, lowering, code generation) and any new diagnostics.
- **Migration guidance** — Provide upgrade notes if existing source code might be affected by the feature.

## 3. Extend syntax and parsing
- **Tokens and trivia** — Add or update token definitions, including keyword reservation and contextual keywords. Required when
  introducing new lexical elements.
- **Grammar production changes** — Modify the parser, grammar rules, and syntax node structures (including generator inputs).
  Apply this step for new syntax forms or when existing productions need to recognize new combinations.
- **Incremental parsing behavior** — Evaluate incremental and error recovery scenarios. Needed when the feature affects how
  partially edited code is parsed in the IDE.

## 4. Introduce syntax trees and factories
- **Syntax node types** — Create new node kinds and update node generators. Consider when the feature needs new AST shapes.
- **Green and red tree integration** — Ensure parent/child relationships, slots, and trivia binding are correct. Always required
  for new node kinds to guarantee tree integrity.
- **Factory helpers and syntax normalization** — Update any syntax factory or formatting helpers when the feature should be
  constructible via APIs.

## 5. Semantic analysis and symbols
- **Binder updates** — Teach the binder how to bind the new syntax. Required whenever the feature influences binding or name
  lookup.
- **Symbol model changes** — Add or modify symbol types, members, or modifiers to represent the feature. Needed when the feature
  introduces new metadata, accessibility rules, or member kinds.
- **Type system integration** — Update conversion rules, overload resolution, inference, or constraint solving that the feature
  relies on.
- **Flow and data-flow analysis** — Adjust definite assignment, nullability, or control-flow checks if the feature impacts them.

## 6. Diagnostics and messages
- **New diagnostics** — Define diagnostic descriptors for new error cases or warnings. Consider whenever the feature introduces
  invalid states that need feedback.
- **Existing diagnostics** — Review current diagnostics for accuracy with the new feature. Necessary when prior messages or
  codes would misfire after the change.

## 7. Lowering and intermediate representation
- **Bound tree updates** — Add new bound node forms or extend existing ones to represent the feature in the lowered tree. Apply
  when the feature is not expressible via existing bound nodes.
- **Lowering passes** — Update rewriters or lowering steps so that the feature translates into existing runtime constructs.
- **Control-flow graph changes** — Ensure the CFG builder accounts for new control-flow shapes, especially loops, jumps, or
  pattern matching constructs.

## 8. Code generation and runtime
- **Emitter changes** — Modify the IL generator or backend to emit correct instructions for the feature. Required when the
  lowered representation produces new runtime behavior.
- **Runtime library dependencies** — Update or add runtime helpers when the feature depends on library support.
- **Metadata and attributes** — Ensure generated metadata (attributes, method flags, accessibility) reflect the new semantics.

## 9. Public APIs and tooling
- **Operations API** — Extend the operations tree or analyzer APIs if the feature should appear in analyzers. Consider when the
  feature must be analyzable by external tooling.
- **Workspace and IDE features** — Update completion, formatting, classification, or refactorings. Needed when the feature
  appears in IDE scenarios.
- **Command-line interface** — Adjust CLI switches or diagnostics output if the feature is user-configurable.

## 10. Testing strategy
- **Unit tests** — Add parser, binder, semantic, and code generation tests covering happy-path and error scenarios. Always
  required to validate the change.
- **Integration tests** — Create end-to-end or scenario tests when the feature impacts multiple compiler layers or runtime
  behavior.
- **Stress and regression tests** — Consider targeted stress cases (incremental parsing, concurrency, large projects) when the
  feature could impact performance or stability.

## 11. Tooling and automation
- **Generators** — Regenerate syntax nodes, diagnostics, or visitors if the feature affects generated assets.
- **Build integration** — Ensure CI scripts, formatting rules, and analyzers accommodate the new files or code paths.

## 12. Rollout and maintenance
- **Documentation cross-links** — Update TOC entries, sample code, and API references to make the feature discoverable.
- **Issue tracking** — File follow-up tasks for deferred work, tooling gaps, or dependent features.
- **Telemetry and feedback loops** — If available, add instrumentation or logging to evaluate the feature post-release.
