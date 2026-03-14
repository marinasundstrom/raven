# Raven Changelog

Behavior-focused timeline covering **2025-09-12** to **2026-03-13**.

## 2026-03-13

- Added Roslyn-style syntax formatting hooks: `Formatter.Annotation`,
  `SyntaxAnnotation.ElasticAnnotation`, and elastic trivia helpers on
  `SyntaxFactory`, with `SyntaxNormalizer` updated to honor formatter
  annotations and elastic whitespace.
- Clarified the syntax API docs to state that `SyntaxFactory` creates raw
  structured nodes that callers must format or attach trivia to explicitly.

### Added
- Added initial macro-system scaffolding: `#[MacroName]` syntax is now recognized as a distinct macro-style annotation surface, and public .NET plugin contracts were introduced under `Raven.CodeAnalysis.Macros`.
- Added targeted parser/semantic tests for macro-style attributes and plugin reference discovery.
- Added a sample project layout under `samples/projects` showing the intended `AddEquatable` Raven source and companion .NET macro plugin shape.
- Added project-system/compiler support for `RavenMacro` assembly references plus initial macro diagnostics for unknown/duplicate/invalid attached macros and plugin load failures.
- Added generic attached-macro expansion invocation and caching on `SemanticModel`, including plugin diagnostics and expansion-failure diagnostics, so tooling can inspect expansion results without compiler-side macro synthesis.
- Added optional replacement-declaration support to `MacroExpansionResult` so attached macros can move beyond additive member generation toward property/declaration rewriting scenarios.

### Changed
- Fixed generated TargetFramework handling so SDK-style projects with an explicit top-level `func Main() -> unit` no longer synthesize a competing entry point from the generated framework-attribute document.
- Fixed value-type indexer call emission so Raven-authored macro plugins can safely access struct-backed syntax collections without generating invalid IL.
- `use` declarations in async contexts now prefer `IAsyncDisposable.DisposeAsync()` when available and fall back to `IDisposable.Dispose()` otherwise, while keeping sync contexts on ordinary `Dispose()`.
- Attached macro replacement/introduction now participates in semantic declaration binding for type members, so replacement properties and generated members show up through declared-symbol lookup instead of remaining expansion-only metadata.
- Attached macro-generated syntax now participates in emit as well as semantic binding, so introduced methods and replacement properties change the generated IL instead of remaining tooling-only expansions.
- MSBuild `RavenMacro` items can now point at Raven macro projects directly, and the project system will build/load the current plugin assembly instead of silently using a stale checked binary.
- Added an initial macro-expansion editor experience: hovering a macro shows an expansion preview, and VS Code now offers a `Show macro expansion` code action that opens the rendered expansion in a preview editor.
- Fixed the Raven-authored `#[Observable]` sample macro to use the property type itself instead of the full type-annotation clause, so the sample now produces a real replacement setter and raises `PropertyChanged` as intended.
- Macro attributes now use `#[...]` instead of escaped attribute identifiers, `#` only tokenizes that way when immediately followed by `[`, and the VS Code grammar now highlights macro attributes separately from ordinary attributes.
- Macro project loading is now deterministic across target frameworks and dependencies: Raven-authored macro projects emit under framework-specific output folders, rebuild inputs include referenced project outputs, and macro load contexts no longer reuse arbitrary same-name process assemblies.
- Metadata methods with unreadable signatures no longer collapse to arity-zero methods during symbol loading; the compiler now preserves them as invalid signatures instead of silently rebinding them as parameterless APIs.
- Attached macro plugins now receive both the raw parsed argument list through `AttachedMacroContext.ArgumentList` and a convenience parsed view through `AttachedMacroContext.Arguments`, where each `MacroArgument` exposes both a richer constant representation and a direct CLR `Value`.
- Added `IMacroDefinition<TParameters>` as the public marker for the typed macro-parameter-object direction, so attached macros can move toward attribute-like argument binding and editor experience without changing invocation syntax again.
- Added `IAttachedDeclarationMacro<TParameters>` and the first compiler-bound typed-parameter path for attached macros: positional arguments bind through a single public constructor, named arguments bind through writable properties, and invalid names/conversions now report dedicated macro diagnostics before expansion.
- Added freestanding expression macros with `#name(...)` syntax, typed parameter binding, semantic-model expansion lookup, and initial language-server preview/definition support.
- Macro argument constant values are now evaluated without re-entering semantic diagnostics during expansion, so macros can read literal argument values without recursively re-triggering their own expansion and blowing the stack.
- Accessor parsing and formatting now preserve explicit same-line `;` separators, and `SyntaxNormalizer` inserts line breaks between adjacent accessors and block statements when raw generated syntax omits trivia, so macro expansion previews stay readable without requiring macros to hand-format every token.
- `SyntaxFactory.ArrowExpressionClause(...)` now defaults to the fat arrow token `=>` at the syntax-model level, so generated accessor and member expression bodies no longer drift back to pointer-style `->` after regeneration.
- `SyntaxFactory` token convenience members now return fresh token instances instead of reusing shared singleton tokens, so Raven-authored macros can safely use helpers like `CommaToken`, `SetKeyword`, and `NewLineToken` multiple times while building detached syntax trees.
- Statement factory convenience overloads now respect `DefaultToken` before optional-token fallback, and the statement syntax model now defaults `TerminatorToken` to `NewLineToken`, so raw `SyntaxFactory` statements carry Raven’s normal structural statement separator without requiring callers to pass it explicitly.
- `SyntaxFactory` convenience overloads can now be defined explicitly in `Syntax/Factories.xml`, and the node generator validates those overload definitions against the syntax model so invalid slot mappings and hazardous combinations like non-null `Body` plus `ExpressionBody` are rejected during generation.
- Delegate parameter inference is now covered for both direct metadata-delegate assignment and `PropertyChanged += (sender, args) => ...` event subscriptions, including the observable sample shape.
- The `macro-observable` sample now uses inferred lambda parameter types for its `PropertyChanged` handler, matching ordinary delegate assignment behavior.
- Lambda parameter declarations in target-typed function expressions now resolve through the same contextual semantic binding as identifiers inside the body, and compound assignment statements now surface stable assignment operations instead of crashing operation traversal.
- The language server now keeps project-backed documents stable across multi-project workspaces: sibling-project files can be resolved by URI on demand, and closing an open project document no longer removes it from the underlying workspace project graph.
- Language-server diagnostics now match source-backed compiler diagnostics by file path instead of requiring the exact same syntax-tree instance, so compiler `Info`/hint diagnostics keep showing up for open documents instead of only analyzer suggestions surviving the filter.
- Semantic diagnostics no longer crash on malformed invocations inside match arms; argument binding now tolerates missing argument nodes and continues reporting parser/binder diagnostics.
- Top-level and namespace parsing now correctly distinguishes sequence-pattern assignment statements from attribute/declaration preludes, so `[val first, val second] = values` no longer gets misparsed as a broken attribute list.
- Hover resolution inside lambda bodies is now more robust: member-name tokens are resolved before enclosing-block locals can hijack them, and lambda pattern locals no longer get misidentified as plain parameters.
- Attached macros can now return syntax built directly with `SyntaxFactory` without needing synthetic source rooting first; replacement members are contextualized against the real containing declaration before binding/emit, and detached generated syntax no longer crashes source symbol or method-body emission paths.
- Project-reference compilations now force source declaration symbols for referenced Raven projects before they are exposed as `CompilationReference`s, so sibling-project source types participate in name binding and editor navigation instead of degrading to `Error` across workspace boundaries.
- `Go to definition` now resolves `#[MacroName]` sites back to the macro declaration project when the macro project is open in the workspace, using the macro reference’s source project path to map the loaded macro type back to source.
- `Go to definition` and expansion preview now also work for freestanding macro invocations such as `#answer()`.
- Fixed `SeekableTextSource.PeekChar(offset, ...)` so offset-aware peeks actually honor the requested offset; this was required to keep `#pragma` on the directive path while adding freestanding `#name(...)` parsing.

Impact:
- Raven now has a stable syntax and host API foundation for attached macros without routing them through the normal CLR attribute pipeline.
- Plugin authors have a concrete contract to target, Raven projects can point at macro plugin assemblies, and the compiler can now execute attached macros generically while keeping generated-member semantics out of the compiler for now.
- Raven-authored macro plugins now load cleanly even when they index into value-type syntax collections, and SDK-style executable projects no longer hit spurious entry-point ambiguity from generated framework metadata.
- Macro-driven member replacement is now visible to semantic tooling, and the editor can surface the generated expansion without requiring a debugger or ad hoc compiler logging.
- The Raven-authored observable sample now exercises a real end-to-end replacement macro path instead of silently falling back to the source auto-property.
- Multi-target workspaces can now reference the same Raven-authored macro project without reusing the wrong plugin binary, and metadata probing no longer risks rebinding unreadable APIs as parameterless methods.
- Attached macros can now safely inspect literal argument constants during expansion, and the observable sample no longer appears to hang when the plugin reads `context.Arguments[0].Constant`.
- The macro contract now has an explicit typed-parameter direction, aligning future completion/signature help and argument diagnostics with the way normal attributes are presented in the IDE.
- Raw `SyntaxFactory`-built macro expansions now display with sensible accessor and statement layout in the editor even when the macro only supplies structural terminators instead of fully formatted trivia.
- Macro expansion hover/code-action previews are stable again after syntax regeneration, because detached `SyntaxFactory` expression bodies now render with `=>` consistently.
- Macro expansion hover/code-action previews no longer disappear when a macro builds syntax from repeated `SyntaxFactory` token helpers, because formatter rewrites now see distinct token identities instead of duplicate singleton token objects.
- `SyntaxFactory` statement builders now produce structurally terminated statements by default, keeping the API focused on syntax structure while leaving indentation and spacing to normal formatting.
- Public syntax-factory API shape is no longer forced to follow slot heuristics alone; explicit factory definitions now let Raven control convenience overloads separately from raw tree structure while keeping the generated API validated against the underlying slots.
- Cross-project workspace navigation is now reliable for both normal Raven project references and open Raven macro projects, so definition requests no longer fall back to same-file error locals or stay stuck on the `#[]` use site.
- Delegate inference behavior around event subscriptions is now locked by focused tests, and the observable sample demonstrates the inferred-parameter form directly.
- Hover/symbol lookup for inferred lambda parameters is now consistent with the compiler’s actual binding, and operation-based tooling no longer trips over `+=` statements while walking child operations.
- Hover/code-action requests for files in referenced sibling projects no longer lose their semantic model because the LSP workspace was deleting real project documents on close or relying solely on transient open-document ownership.
- Open-document diagnostics in the editor are now resilient to equivalent syntax-tree instances, which fixes missing compiler hints/information diagnostics in the normal LSP publish path.
- Broken source inside a match arm now degrades to diagnostics instead of throwing a null-reference exception during semantic-model construction.
- Sequence-pattern assignment now binds from the correct syntax shape at top level and inside namespaces, which restores parser/semantic coverage for destructuring assignment scenarios.
- Hover over member-access names and lambda pattern locals is now less sensitive to stale or over-broad fallback resolution, reducing false symbol results in the language server.
- Raven-authored macros can now construct generated declarations structurally and preserve reused source syntax such as property initializers, instead of having to round-trip through parsed helper strings or synthetic wrapper trees.

## 2026-03-12

### Added
- Expanded Operations API coverage for newer language constructs and bound nodes.
- Added targeted sample coverage around generic parsing with static interface constraints.
- Added style analyzer + code fix to convert expression-bodied members to block-bodied form.
- Added an MSBuild-backed Raven project-system service so workspaces can open SDK-style project files with `RavenCompile` items and traverse `ProjectReference` through the project-system abstraction.

Impact:
- Compiler API consumers can inspect more semantics directly.
- Regressions in generic-constraint scenarios are easier to catch with samples.
- Raven workspace consumers are no longer limited to the custom `.ravenproj` file format.

### Changed
- Null-assignment diagnostics were tightened and message quality improved (clearer assignment errors and hint formatting).
- Static interface member resolution and generic constraint checks were corrected for `IParsable<T>`-style flows.
- Cascade behavior after failed generic binding was reduced to avoid misleading downstream errors.
- Generic method calls with explicit type arguments now follow C# more closely by skipping extra method-type inference passes for later lambda arguments, and overload reporting suppresses more downstream cascades when an argument already carries an error type.
- Several binder/codegen regression fixes landed (including interpolation/object-dumper/runtime sample paths).
- Hover/signature display for promoted primary-constructor parameters now preserves binding keyword semantics (`val`/`var`) when the parameter maps to a property.
- Compiler projects were retargeted from `net10.0` to `net10.0` (including build scripts/default framework switches).
- The primary Raven CLI command name is now `rvn`, and project scaffolding/help now advertise SDK-style `.rvnproj` files.

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
- Changed: invocation arguments for `ref`, `out`, and `in` parameters now use explicit call-site keywords instead of `&` at ordinary call sites. Raven now supports `Set(ref value)`, `TryParse(text, out result)`, and declaration forms like `TryParse(text, out var result)` and `TryParse(text, out val result)`.
