# Macro implementation plan

This is the living implementation tracker for Raven's procedural macro system.
Update it when a macro slice is started, completed, deferred, or materially
redesigned.

The durable product and tooling model is documented in
[Macro and DSL developer experience](developer-experience.md).

## Plugin boundary

Macros are compiler plugins and must work through `Compilation` without a
`Workspace`. Analyzers and generators are workspace plugins: their discovery
and scheduling belong to workspace/build-host functionality. Project systems
may resolve macro assets, but must hand them to the compiler rather than owning
macro expansion.

Workspace analyzers may optionally query compiler-owned retained structure for
macros that explicitly provide it. An `ExpressionSyntax` embedded in that
structure should automatically enter ordinary Raven expression analysis when
an analyzer host is present. Unstructured macros return no structure;
analyzers must not infer one from tokens or expansion output. Macro correctness
and execution must never depend on an analyzer being loaded.

## Current foundation

Implemented before the token-tree work:

* attached declaration macros using `#[Name]`
* argument-based freestanding expression macros using `#name(...)`
* .NET and Raven-authored macro plugins referenced through the transitional
  `RavenMacro` project item
* typed parameter objects for argument-based macros
* expansion diagnostics, semantic binding, emit, expanded-document views,
  completion, hover, and definition support

## Active slice: raw token-tree expression macros

Status: **implemented and validated**

Target syntax:

```raven
val result = #query {
    from user in users
    where user.IsActive
    select user.Name
}
```

The first slice is intentionally expression-only. It must:

* preserve the macro body as lossless raw authored text rather than forcing it
  through Raven's lexer
* balance nested braces at the compiler-owned invocation-envelope layer
* allow arbitrary DSL characters without ordinary Raven lexer diagnostics
* expose a stable body-relative coordinate system
* allow the macro to parse the complete body itself
* allow selected body-relative spans to be delegated back to Raven's expression
  parser
* report macro diagnostics at authored body-relative spans
* lower expansion results through the existing freestanding-expression binding
  and emit path
* preserve existing `#name(...)` behavior

Current implementation work:

* [x] add raw token-tree body syntax to `FreestandingMacroExpressionSyntax`
* [x] add a compiler-owned raw delimited-body scanner
* [x] add `ITokenTreeExpressionMacro`
* [x] add `TokenTreeMacroContext` with raw text, body span, Raven-expression
  parsing, and body-relative diagnostics
* [x] integrate token-tree macro discovery, validation, and expansion
* [x] pass focused parser tests
* [x] pass focused semantic and runtime tests
* [x] update the language specification and changelog
* [x] run the complete macro feature suite

Validation record for this slice:

* `scripts/codex-build.sh`: passed
* `scripts/test-feature-suite.sh macros`: 39 passed
* focused parser tests: 5 passed
* focused semantic/runtime tests: 12 passed
* focused completion and raw-kind tests: 6 passed
* `Raven.LanguageServer` project build: passed
* focused macro expansion language-server tests: 10 passed

## Active slice: replaceable macro token streams

Status: **implemented and validated**

This slice turns the token-stream developer-experience design into public
compiler contracts:

* [x] add a common `IMacroTokenStream` whose output primitive is `SyntaxToken`
* [x] add compiler-discovered `IMacroTokenStreamProvider` capability hooks
* [x] add a default stream backed by Raven's normal lexer
* [x] add macro-local keyword and reserved-word overlays through
  `IMacroKeywordProvider`
* [x] preserve ordinary Raven `SyntaxKind` while exposing provider-owned
  `RawKind`
* [x] allow a macro to replace the default stream with a custom lexer-backed
  implementation
* [x] keep token positions body-relative while retaining the authored
  `BodySpan` for document mapping
* [x] add focused default-overlay and custom-provider tests
* [x] update the language specification and changelog
* [x] pass the complete macro feature suite

Validation record for this slice:

* `scripts/test-feature-suite.sh macros`: 41 passed
* focused default-overlay and custom-provider tests: 2 passed

## Active slice: direct-lowering macro MVP

Status: **implemented and validated**

The first developer-facing token-tree macro is intentionally small:

```raven
let shouldRetry = #guard {
    unless answer == 42
}
```

It proves the shortest useful path without introducing a secondary DSL syntax
tree:

* [x] implement the macro in Raven rather than only as an in-process test type
* [x] consume the standard Raven-backed token stream
* [x] recognize a body-scoped `unless` keyword through `RawKind`
* [x] delegate the remaining body-relative span to Raven's expression parser
* [x] lower directly to an ordinary logical-negation expression
* [x] bind an embedded Raven expression in the invocation's caller scope
* [x] run the sample through expansion, emit, and execution
* [x] add focused compiler runtime coverage
* [x] pass the complete fast macro feature suite

Validation record for this slice:

* Raven-authored macro project build: passed
* sample application runtime output: `42`, `False`
* focused `FreestandingMacroCodeGenTests`: 3 passed
* `scripts/test-feature-suite.sh macros`: 41 passed
* `scripts/test-feature-suite.sh macros --runtime`: 8 passed, with 2
  attached-property accessor identity failures in `MacroCodeGenTests` outside
  this direct-lowering sample slice

This is the MVP pattern to extend before adding retained custom structure:
tokenize, recognize a small DSL envelope, parse selected Raven fragments, and
lower to ordinary Raven syntax. A LINQ-like macro can add more clauses and
fragment boundaries to this path. The expression-only `#quote` implementation
can reuse the same raw-body and Raven-fragment infrastructure while returning
syntax as data rather than binding it as the replacement expression.

## Active slice: multi-clause direct-lowering MVP

Status: **implemented and validated**

The second executable token-tree macro extends the direct path to several
clauses and embedded Raven fragments:

```raven
let verdict = #choose {
    test answer == 42
    then "correct"
    otherwise "wrong"
}
```

* [x] recognize ordered `test`, `then`, and `otherwise` macro-local keywords
* [x] treat the clause words as reserved inside the DSL body
* [x] derive three body-relative Raven expression spans from stream tokens
* [x] parse the condition, result, and fallback independently
* [x] lower directly to an ordinary Raven `if` expression
* [x] bind the condition in the invocation's caller scope
* [x] report a body-mapped macro diagnostic for a missing clause
* [x] validate both runtime branches
* [x] run the Raven-authored sample end to end
* [x] pass the complete fast macro feature suite

Validation record for this slice:

* Raven-authored macro project build: passed
* sample application runtime output: `42`, `False`, `correct`
* focused `FreestandingMacroCodeGenTests`: 5 passed
* `scripts/test-feature-suite.sh macros`: 41 passed

This remains intentionally structure-free. It establishes the clause and
fragment mechanics needed before attempting a LINQ-like sequence of
`from`/`where`/`select` clauses.

## Active slice: LINQ-like query MVP

Status: **implemented and validated**

The first query-shaped macro supports one range variable, an optional filter,
and one projection:

```raven
let result = #query {
    from value in values
    where value > 2
    select value * 10
}
```

* [x] recognize macro-local `from` and `select` reserved words
* [x] reuse Raven's existing `in` and `where` tokens
* [x] parse the source, optional predicate, and projection as Raven fragments
* [x] create `Where` and `Select` lambdas using the authored range variable
* [x] lower directly to ordinary invocation syntax
* [x] support the query form without `where`
* [x] verify the range variable shadows an outer name only inside generated
  lambdas
* [x] report a body-mapped diagnostic when `select` is missing
* [x] run the Raven-authored sample through emit and execution
* [x] add focused runtime and diagnostic coverage
* [x] pass the complete fast macro feature suite

Validation record for this slice:

* Raven-authored macro project build: passed
* sample application runtime output: `42`, `False`, `correct`, `70`
* focused `QueryMacroCodeGenTests`: 4 passed
* `scripts/test-feature-suite.sh macros`: 41 passed

The MVP generates no hidden temporary identifiers: the only introduced binding
is the explicitly authored range variable used as each lambda parameter. This
keeps the initial hygiene story observable and deterministic. Repeated
generators, joins, ordering, continuation clauses, and retained query structure
remain later slices.

## Active slice: diagnostic-bearing Raven fragments

Status: **implemented**

Hybrid macros can now retain Raven parser recovery and diagnostics together:

```raven
let predicate = context.ParseExpressionResult(predicateSpan)

FreestandingMacroExpansionResult {
    Expression = predicate.Syntax
    Diagnostics = predicate.Diagnostics
}
```

* [x] preserve the existing `ParseExpression` syntax-only convenience API
* [x] add `ParseExpressionResult` for the complete body and selected spans
* [x] return recovered syntax, immutable diagnostics, and `HasErrors`
* [x] map native parser diagnostics to the authored invocation syntax tree
* [x] make the result generic for later syntax fragment categories
* [x] propagate embedded parser diagnostics through the query MVP
* [x] cover a malformed query fragment at its authored token location

The parser result is intentionally not a custom DSL syntax tree. It is the
shared boundary for delegating selected spans back to Raven. A macro remains
free to retain separate DSL structure before translating it to ordinary Raven
syntax.

## Active slice: Raven statement fragments

Status: **implemented**

Token-tree macros can delegate either the complete body or a selected
body-relative span to Raven's statement parser:

```raven
let statement = context.ParseStatementResult(statementSpan)
```

* [x] preserve concise `ParseStatement` syntax-only overloads
* [x] provide diagnostic-bearing `ParseStatementResult` overloads
* [x] accept the complete body or one selected body-relative span
* [x] reject trailing statement input
* [x] map native parser diagnostics to the authored invocation tree
* [x] share the generic fragment-parsing implementation with expressions

This adds an embedded-fragment category for DSL authors; it does not yet add a
statement-position macro carrier or a public statement-quote spelling.

## Validation case: compile-time file embedding

Status: **test macro implemented; tracked resource API deferred**

A test-only `#embedText(path)` procedural macro reads a file during expansion,
returns its contents as an ordinary string literal, and maps read failures to
the authored path argument. This demonstrates both compile-time execution and
early diagnostics.

Production file-reading macros still need compiler-owned path resolution,
dependency recording, incremental invalidation, determinism, cancellation, and
file-access policy. Direct file I/O is not promoted as the public resource
contract.

## Future API ergonomics: expansion-result factories

Add category-aware factory methods to macro expansion result types after the
supported result combinations stabilize. These should make success,
success-with-forwarded-diagnostics, diagnostic-only failure, attached
replacement-plus-introduced-members, and no-change results explicit without
requiring authors to assign mutable properties in the correct combination.

## Future SDK integration: provider-declared compiler plugins

Replace consumer-authored `RavenMacro` items with provider-declared
compiler-plugin assets:

1. a Raven macro project or package marks its output as a compiler plugin;
2. consumers use a normal project or package dependency;
3. the SDK resolves the marked asset and passes it to the compilation;
4. the compiler loads its manifest and exported macro contracts; an
   assembly-level marker may explicitly list plugin types or authorize fallback
   discovery of `IRavenMacroPlugin` implementations within that marked
   assembly; and
5. normal duplicate-name, compatibility, and load diagnostics continue to
   apply.

Do not scan every ordinary runtime reference for macro implementations.
Provider metadata is the explicit execution boundary; consumer source imports
are unnecessary because macro names are registered with the compilation.
Prefer explicit plugin types in the manifest and restrict reflection discovery
to assemblies carrying the compiler-plugin marker.

## Active slice: default environment and in-memory activation

Status: **implemented and validated**

* [x] centralize compiler-provided macro registration in a default environment
* [x] use the same default environment for binding and completion
* [x] keep `#quote` automatically available without imports or project items
* [x] allow a macro assembly image to be activated without writing it to disk
* [x] prove the image path with a Raven-authored macro that uses `#quote`

The default environment is the common registration point for future
compiler-intrinsic and SDK-bundled macros such as `#embedFile`. In-memory image
activation is the execution boundary needed by the Playground and the future
same-project compile-time partition; it does not yet identify or compile that
partition.

Validation record for this slice:

* focused `MacroReferenceTests`: 5 passed
* `scripts/test-feature-suite.sh macros`: 45 passed

## Active slice: same-project macro source partition

Status: **same-project and incremental-cache MVP implemented and validated**

* [x] accept explicitly classified macro implementation syntax trees through
  `Compilation.AddMacroSyntaxTrees`
* [x] compile and activate that partition in memory before binding consumer
  invocations
* [x] automatically reference the compiler macro contracts from the partition
* [x] forward partition diagnostics through the consumer compilation
* [x] exclude compile-time-only macro implementation types from runtime emit
* [x] include activated local macros in compiler-owned completion
* [x] preserve compiler-only operation without `Workspace`, MSBuild, or an
  on-disk plugin artifact
* [x] automatically classify a source file containing a
  `[LocalMacroPlugin]` declaration in compiler and workspace construction
* [x] preserve semantic-model access to the compile-time-only file
* [x] build an SDK project that declares and consumes a macro without a
  `RavenMacro` item or explicit compiler-contract reference
* [x] classify `[LocalMacro]` declarations independently from consumer
  declarations in the same source file
* [x] run same-buffer local macros in the browser Playground
* [x] add runnable Playground examples for `#quote` and project-local macros
* [x] cache the activated partition independently from consumer-only edits
* [x] invalidate dependent expansions when the partition changes
* [x] add declaration-granular dependency-cycle diagnostics
* [x] route authored positions to the current macro or consumer semantic
  projection through `Compilation.GetSemanticModel(tree, position)` and
  `Document.GetSemanticModelAsync(position)`
* [x] use position-aware projections for macro-author hover and completion
* [x] use position-aware projections for macro-author definition, references,
  and rename

The automatic MVP uses a syntax-only, dedicated-file rule. When an ordinary
attribute named `LocalMacroPlugin` or `LocalMacroPluginAttribute` appears on a
type declaration, the complete syntax tree is moved into the compile-time
partition. The attribute itself is declared by `Raven.CodeAnalysis.Macros` and
is distinct from both `#[...]` macro invocations and the future assembly-level
provider marker for reusable compiler-plugin dependencies.

Keeping the rule syntax-only avoids binding consumer source before plugin
activation. Keeping it file-granular preserves the initial acyclic boundary:
the marked file receives metadata references and other macro references, but
not consumer source declarations. Macro implementations and supporting types
therefore belong in a dedicated file for this MVP.

`Compilation.AddSyntaxTreesWithLocalMacros` applies the same classification
without requiring a `Workspace`. Workspace and SDK compilation construction use
that API automatically. `AddMacroSyntaxTrees` remains available when a host
already has an explicit partition.

For a mixed source file, `[LocalMacro]` marks one top-level type declaration and
all declarations nested within it as compile-time-only. Every top-level support
type required by the plugin must also carry `[LocalMacro]`. The classifier
creates position-preserving macro and consumer projections by replacing the
other partition with whitespace, so diagnostics retain authored offsets. The
consumer partition receives the compatible compiler contracts automatically.

The Playground now uses this declaration-granular path. Its examples include
an expression quote and a same-buffer plugin that defines an attached
declaration macro, an argument-style expression macro, and a raw token-tree
expression macro.

Incremental compilations reuse the emitted and activated local-plugin artifact
when the macro projection, compilation and parse options, metadata references,
macro references, and assembly identity remain equivalent. The macro
compilation itself is recreated for every snapshot so semantic models remain
owned by the current projected trees. Cached success and failure diagnostics
are remapped to those current trees before they are reported. A macro-source or
reference change invalidates the artifact; ordinary consumer-only edits do not.
Dependent expansions are consequently rebuilt from the current macro registry
when the artifact changes.

If an unresolved reference in the local macro compilation resolves only when
the consumer projection is added, Raven replaces the generic missing-name
diagnostic with `RAVM003`. The diagnostic explains the phase cycle at the
authored macro reference and directs the author to move the dependency into the
local macro partition or a referenced assembly. The semantic probe avoids
misclassifying ordinary typos or genuinely missing dependencies as cycles and
works for both mixed and dedicated macro files.

The compiler and Workspace APIs now accept an authored position when requesting
a semantic model. `Compilation.GetSemanticModel(tree, position)` supports
one-shot compiler hosts, and `Document.GetSemanticModelAsync(position)`
delegates the same decision for Workspace callers. Positions inside a
declaration marked `[LocalMacro]` route to the current macro projection; other
positions route to the consumer projection. The returned model exposes its
projected `SyntaxTree`, whose position-preserving nodes can be passed directly
to ordinary semantic queries. Routing is recomputed for each Workspace snapshot
and also works for documents without file paths. Existing positionless calls
retain their consumer-oriented behavior.

The language server now asks `DocumentStore` for an analysis context at the
request position before it runs hover, completion, definition, references, or
rename. The context retains the authored offsets while selecting the
compiler-owned macro projection inside `[LocalMacro]` declarations. Hover can
therefore resolve local macro symbols, and ordinary Raven completion inside
macro implementation bodies sees the macro class, compiler contracts, and
referenced compile-time APIs. Definition resolves those symbols to their
authored declaration. Reference search scans both compiler-owned projections
of each mixed document, but symbol identity selects only the matching semantic
universe; returned locations and rename edits use the original document text.

Layered project-local macro bootstrapping, where one local macro generates
another macro implementation, remains out of scope until the phase model is
proven.

Validation record for this slice:

* `scripts/test-feature-suite.sh macros`: 52 passed
* focused compiler automatic-partition test: passed
* focused Workspace automatic-partition and semantic-model test: passed
* focused Workspace partition reuse, invalidation, and diagnostic-remapping
  tests: passed
* focused SDK same-project build without `RavenMacro`: passed
* focused mixed-declaration compiler test: passed
* focused mixed-file and dedicated-file dependency-cycle tests: passed
* focused position-aware semantic routing and incremental-edit tests: passed
* focused macro-author hover and completion integration tests: passed
* focused macro-author definition, references, and rename integration tests:
  passed
* browser Playground smoke test, including every example: passed

Next planned slice: expose compiler-owned structured macro regions to analyzer
hosts so embedded Raven expressions can participate in ordinary analysis
without making analyzers a compiler requirement.

## Architectural invariants

Keep these true as new DSL cases are added:

1. The authored macro body is the source of truth. Custom lexing is derived
   state and must not replace or rewrite the body stored in the syntax tree.
2. DSL token kinds are macro-owned integer identities exposed as `RawKind`.
   They must not be added to Raven's `SyntaxKind` enum. Tokens produced by the
   standard Raven stream additionally project their raw kind to `SyntaxKind`.
3. All DSL tokens, diagnostics, highlighting classifications, and completion
   edits use body-relative spans that can be mapped to the authored syntax tree.
4. A macro may use complete custom parsing, Raven fragment parsing, or a hybrid
   of both.
5. Expansion always produces ordinary Raven syntax before normal binding and
   emit continue.
6. Compiler APIs own macro semantic truth. The language server only schedules
   and presents macro-provided editor results.
7. Macro plugins are trusted build extensions in the initial in-process model.
   `AssemblyLoadContext` is dependency isolation, not a security sandbox.
8. Macro execution must remain available from the compiler-only `Compilation`
   API. It must not acquire a dependency on `Workspace`, MSBuild, or analyzer
   and generator orchestration.
9. Workspace analyzers may query compiler-owned retained macro structure.
   Typed Raven fragments inside that structure can trigger their corresponding
   analysis pipeline; unstructured macro regions remain opaque.

## Planned follow-up slices

### Custom DSL tokenization

The initial replaceable token-stream contract is implemented. Raven supplies
the default stream using its normal lexer over the authored macro body; a macro
may provide a custom stream implementation backed by its own lexer. Parsers
consume the same stream interface in either case.

The ordinary extension case is a macro-local keyword overlay, not a new Raven
token kind. The standard stream first applies Raven lexing, then reclassifies
matching identifier tokens with provider-owned raw kinds and keyword or
reserved-word classifications. This overlay is scoped to that macro body and
must not change `SyntaxKind`, `SyntaxFacts`, or lexing in ordinary Raven source.
A fully custom lexer is reserved for DSLs with a genuinely different lexical
grammar.

Each stream token carries:

* a provider-owned integer `RawKind`
* body-relative span
* raw text and optional decoded value

The provider owns the mapping from `RawKind` to kind names and future editor
classifications such as keyword, reserved word, identifier, literal, operator,
punctuation, or comment. This avoids semantic collisions between independent
DSLs that use the same integer. `SyntaxToken.RawKind` provides the equivalent
projection for ordinary Raven tokens. Custom tokens remain a stream-level
primitive and are not inserted into ordinary Raven green trees.

The compiler should cache default or custom lexer results per macro body,
stream provider, and plugin identity. The initial token-tree expansion contract
does not require a custom lexer; macros may parse raw text directly.

### Editor services

Add optional macro editor providers for:

* semantic-token classifications and syntax highlighting
* completion items and replacement spans
* hover information
* signature help

Editor providers consume the same body text and tokenization snapshot used by
expansion. Results remain compiler-owned and versioned with the containing
document snapshot.

Retained DSL structure should mark incomplete slots and parsed regions with an
expected language category. Completion routing can then delegate expression,
statement, type, pattern, or member regions to Raven's ordinary completion
service while leaving clause and DSL-specific positions with the macro
provider. When DSL bindings such as query range variables are visible inside a
Raven region, the macro/compiler contract must also describe that semantic
scope without leaking generated implementation names.

### Custom DSL syntax trees

Allow a macro parser to build a secondary syntax tree over its token stream.
Special wrapper nodes may model DSL-specific expressions, statements,
declarations, and other categories with provider-owned raw kinds, child
nodes/tokens, body-relative spans, diagnostics, visitors, and rewriters. These
wrappers do not derive from Raven `ExpressionSyntax`, `StatementSyntax`, or
other ordinary grammar node types.

The containing Raven syntax tree continues to store one lossless macro body as
the authored source of truth. Custom DSL nodes are derived compiler-owned state
for expansion and tooling; Raven's ordinary binder does not bind them directly.
The macro lowers a custom tree through a typed expression, statement, member,
or declaration expansion boundary into ordinary Raven syntax.

Raven may define special host-language invocation carrier variants for those
typed positions. `FreestandingMacroExpressionSyntax` is the expression carrier;
future statement and member/declaration carriers can occupy those Raven grammar
slots while preserving the macro name and raw body. These compiler-known
carriers mean "expand here" and are distinct from macro-defined DSL wrapper
nodes. After expansion, the binder receives ordinary Raven syntax matching the
carrier category.

Use a dedicated wrapper abstraction that preserves Raven's familiar
node/token/span conventions without weakening invariants of ordinary Raven
syntax trees. Direct plugin subclassing of ordinary Raven grammar nodes is not
the goal.

### Macro capability hooks

The macro definition is the registration point for optional compiler-owned
capabilities:

* token-stream provider: standard Raven stream with keyword overlays, or a
  fully custom lexer-backed stream
* structure parser: produces the secondary DSL wrapper tree
* expansion/lowering service: converts the wrapper tree to typed ordinary Raven
  syntax
* editor services: completion, classifications, hover, and signature help over
  the token/structure snapshot
* Raven-fragment regions: expected syntax categories plus any macro-introduced
  semantic scope needed for ordinary Raven tooling inside the DSL

The compiler resolves these capabilities through the macro registry, associates
derived tokens and wrapper trees with the invocation plus document snapshot,
and owns caching, cancellation, diagnostics, and invalidation. The language
server asks compiler APIs for results and does not load or call macro plugins
directly.

The intended flow is:

1. preserve the authored raw macro body;
2. obtain the macro's standard or custom token stream;
3. either translate the token stream directly to ordinary Raven syntax or
   optionally build a structured DSL wrapper tree first;
4. serve macro-provided editor requests against any retained structure
   snapshot;
5. lower through the macro to ordinary Raven syntax; and
6. continue through normal Raven binding and emit.

### Raven fragment parsing

Expression and statement helpers now provide diagnostic-bearing parse results.
Extend the same pattern to members, declarations, types, and patterns while
preserving authored locations for parser diagnostics and expansion source
maps.

### Additional expansion positions

Add compiler-known invocation carriers plus typed contracts for statement and
member/declaration results. The parser can place those carriers in valid Raven
grammar slots, and the compiler validates that expansion returns the matching
ordinary Raven category. Do not introduce an untyped "return any syntax node"
contract.

### Expansion driver and isolation

Move expansion scheduling, caching, recursion detection, source mapping, and
resource limits into a compilation-owned macro driver. Consider an
out-of-process host only after the in-process contracts and performance model
are stable.

### Compiler-owned `#quote`

Status: **expression quote and expression-hole MVPs implemented**

`#quote` is now a compiler-registered token-tree macro and needs no plugin
reference. The first slice:

* [x] parses exactly one complete Raven expression
* [x] preserves tokens and trivia
* [x] forwards native parser diagnostics at authored body locations
* [x] rejects trailing input and missing-token recovery
* [x] emits fully qualified ordinary `SyntaxFactory` construction syntax
* [x] parses the generated factory expression before substitution
* [x] binds and emits the resulting syntax object through the ordinary pipeline
* [x] reports the explicit `Raven.CodeAnalysis` runtime-reference requirement
* [x] participates in macro-name completion without a plugin reference
* [x] accepts one or more `#(expression)` holes whose values bind as
  `ExpressionSyntax`
* [x] preserves authored diagnostic positions with equal-width parser
  placeholders instead of new lexer kinds
* [x] forwards native parser diagnostics from malformed hole expressions and
  reports empty holes explicitly
* [x] compiles inside a Raven-authored macro project and constructs the sample
  `#add` expansion from quoted syntax plus argument holes

Later slices add contextual category selection, statement/member/declaration
quote categories, member/declaration fragment parsers, compiler-owned
bind/equivalence verification, SDK reference convenience, and
token/identifier/list/repetition splice categories.
