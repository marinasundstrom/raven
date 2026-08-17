# Macro implementation plan

The planned standard JSON/XML DSLs and their shared developer-experience gates
are tracked in [Standard JSON and XML macro plan](standard-data-literal-macros.md).

This is the living implementation tracker for Raven's procedural macro system.
Update it when a macro slice is started, completed, deferred, or materially
redesigned.

The durable product and tooling model is documented in
[Macro and DSL developer experience](developer-experience.md).

## Application-model redesign gate

Status: **MVP implemented and validated**

The next compiler work follows the normalized contract in the
[macro application model](application-model.md#normalized-compiler-model).
Because Raven has no macro compatibility requirement yet, this is an intentional
replacement of the current cross-product model rather than another compatibility
layer.

The implementation must preserve working expression and attached behavior while
it changes the internal projection. The gate is complete only when:

* application kind is independent from invocable grammar targets;
* invocable targets are projected from the macro return type;
* attached targets are projected from exactly one typed `on` parameter;
* parameter descriptors distinguish caller-supplied and compiler-supplied values
  without category-specific role proliferation;
* symbols, registry lookup, binding, lowering, diagnostics, and language services
  consume the same normalized descriptor;
* expansion results are category-safe through validation rather than casts;
* malformed declarations and invocations retain stable diagnostic states for
  compiler and language-server queries; and
* existing macro, query DSL, HTML/component-template, hover, completion,
  definition, semantic-token, and incremental editing coverage remains green.

The dependency-ordered slices are:

1. normalized metadata with adapters for current providers (implemented);
2. registry, symbol, argument-binding, and language-service migration (provider
   descriptors implemented; source-declaration unification continues with the
   syntax slices);
3. `on` parameter syntax and removal of `MacroTargetClauseSyntax` (implemented);
4. return-type projection and declaration diagnostics (implemented);
5. generalized single-node expansion results and category validation
   (implemented);
6. raw-body statement invocation and recovery coverage (implemented); and
7. later member, type, pattern, and quote-category work.

After the application model is stable, evaluate macro-only typed syntax wrappers
such as `ExpressionSyntax<T>` and `LiteralExpressionSyntax<T>`. They wrap
existing ordinary nodes with compiler-verified semantic constraints; they are
not additions to the Raven syntax tree, and untyped/category-only syntax nodes
remain fully supported.

Each slice is committed independently. Syntax-model changes require generator
rebuilds; compiler behavior changes require focused macro tests, and editor-facing
changes require the corresponding language-server tests. A broad baseline is
reserved for the cross-cutting migration gate before integration.

Final integration validation completed with:

* the complete non-runtime repository baseline;
* all 131 macro feature tests;
* focused macro expansion, code generation, completion, hover, and language-
  server integration coverage;
* successful builds and executions of every maintained console macro sample;
* successful server and WebAssembly builds of the component-template showcase;
* a clean documentation build with warnings treated as errors; and
* a successful Playground WebAssembly build and browser smoke test.

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
* argument-based invocable macros using `name!(...)`
* .NET and Raven-authored macro plugins activated through compiler-owned
  `MacroReference` instances
* typed parameter objects for argument-based macros
* expansion diagnostics, semantic binding, emit, expanded-document views,
  completion, hover, and definition support

## Infrastructure MVP gate

The infrastructure MVP prioritized a dependable macro system before dedicated
declaration syntax. It established the object-oriented macro contracts across
the normal compile, project, and token-stream paths before the function-oriented
authoring layer was added.

* [x] compiler-owned macro discovery, registration, diagnostics, and expansion
  through `Compilation` without requiring a `Workspace`
* [x] attached, argument-style expression, and raw token-tree expression
  invocation carriers
* [x] replaceable `IMacroTokenStream` implementations whose output primitive is
  `SyntaxToken`, including provider-owned `RawKind` values and keyword overlays
* [x] Raven expression and statement fragment parsing inside token-tree bodies
* [x] direct lowering to ordinary Raven syntax, source-located diagnostics,
  cancellation, expansion caching, and expansion-result factories
* [x] same-project and Playground activation through an in-memory compile-time
  partition
* [x] Raven compiler-plugin project references with explicit entry-point
  manifests and bare-marker fallback discovery
* [x] C# compiler-plugin project references using the same provider marker and
  manifest
* [x] metadata-marker discovery for direct assembly and package references
* [x] finish the representative authoring and project integration tests needed
  to treat these contracts as stable enough for broader use

Retained DSL structure, custom editor providers, and additional syntax
categories remain post-MVP layers. The first dedicated declaration syntax,
`macro`, now lowers to the object-oriented contracts rather than replacing
them.

A compiler-backed compile-and-load macro is also post-MVP work. The compiler
APIs already let an ordinary host emit an assembly image, load it into the same
process, and inspect it through reflection. That host-driven workflow is more
dynamic: the running program decides what to compile, load, and execute.
Exposing compilation from a macro would instead be a build-time operation and
requires a compiler-owned nested-compilation, dependency, invalidation,
diagnostic, load-context, caching, and execution-policy contract.

Macro activation has two origins but one result:

* same-compilation declarations are emitted as an isolated in-memory
  compile-time partition; and
* portable assembly references are inspected once per compilation setup for
  the assembly marker and activated from their manifest.

Both paths produce the same active `MacroReference` registry consumed by
binding and expansion. The registry does not branch on origin. The
function-oriented source layer represents `macro` declarations with
`IMacroDeclarationSymbol` and `SymbolKind.Macro`; this symbol is distinct
from both `IMethodSymbol` and the object-oriented provider instance held by the
registry. Projecting an active provider back to a common macro symbol remains a
later tooling layer and was not required for the infrastructure MVP.

Direct macro definitions are the primary activation unit. Same-compilation
definitions are discovered in the local compile-time partition, while
referenced definitions must be exported by their assembly manifest.
Each definition must implement exactly one category-specific macro interface;
`MacroKind` is derived by compiler-owned `MacroFacts` and cannot be supplied or
overridden by the implementation. Target applicability is declared only by
`IAttachedDeclarationMacro`; `MacroFacts.GetTargets` normalizes invocable
definitions to `MacroTarget.None` for common tooling.
The function-oriented authoring model currently synthesizes local provider
adapters and typed parameter objects from dedicated syntax. These are
transitional contracts. The target model is the nominal definition type,
designated `Expand` method, canonical parameter bindings, and erased executor
defined in [Macro ABI](abi.md).

The active set belongs to one immutable compilation snapshot. Editing local
macro declarations, adding or removing a portable reference, or changing a
referenced assembly fingerprint creates a new activation set and registry.
Unchanged local partitions and metadata-load state remain eligible for the
existing incremental reuse paths.

`MacroReference.Macros` exposes that activation result as a cached
`ImmutableArray<IMacroDefinition>`. Repeated compiler and tooling queries see
the same definition instances for the snapshot rather than re-running an
enumerable factory. File- and image-backed snapshots retain their collectible
assembly load context for the full `MacroReference` lifetime so lazily invoked
macro dependencies remain resolvable during expansion.

### MVP authoring and activation matrix

The representative integration paths exercise the direct-macro model:

| Origin | Registration | Representative coverage |
| --- | --- | --- |
| Same Raven compilation | automatic direct-class discovery in the compile-time partition | `RavenProject_BuildsSameProjectMacroWithoutMacroProjectItem`, single-file workspace tests, and Playground local-macro samples |
| Referenced Raven project | `[assembly: RavenCompilerPlugin(typeof(MacroType))]` | direct attached-macro project-reference test and the invocable sample project |
| Referenced C# project | `[assembly: RavenCompilerPlugin(typeof(MacroType))]` | direct token-tree project-reference test and the attached-macro sample project |
| Direct DLL or metadata image | assembly manifest | macro-reference tests for file, image, multiple exports, invalid exports, and bare-marker fallback |
| NuGet package | assembly manifest on selected compile/runtime assets | package tests for direct, split reference/runtime, adjacent helper, and transitive runtime dependencies |
| Compiler built-in | compiler registration | `#quote` semantic, runtime, completion, and Playground coverage |

Across those origins, focused compiler suites cover attached declaration,
argument-style expression, and raw token-tree expression macros. The
same-project path deliberately has no assembly export attribute: local macro
classes are available only to their declaring compilation unless the eventual
emitted assembly explicitly exports them. Macro aggregation containers are not
part of the compiler contract; each macro is registered directly.

Current direct-contract validation:

* complete macro feature suite: 58 passed
* focused compiler/project activation and partition tests: 17 passed
* focused language-server expansion and definition tests: 12 passed
* live macro-project refresh tests: 2 passed; watched-file redesign case remains
  explicitly skipped

## DSL tooling MVP gate

Status: **implemented**

The first editor-facing contract is deliberately smaller than a public custom
syntax-tree API. For one token-tree invocation, the compiler owns and caches a
`MacroInputSnapshot` containing:

* the selected standard or provider token stream, with raw kinds, stable kind
  names, authored spans, and optional presentation classifications;
* body-relative and authored spans for embedded Raven expression, statement,
  type, pattern, compilation-unit, or member fragments; and
* deterministic narrowest-region lookup at a cursor, including zero-width
  expected slots.

Optional metadata failures degrade per token. Provider failures do not escape
into unrelated semantic or language-service queries. The language server
consumes the combined snapshot for semantic highlighting. The macro's own
parse tree remains private and no macro-specific nodes enter Raven's global
syntax or bound trees.

This was the initial MVP boundary: enough information for stable highlighting
and for a future editor to route a cursor into ordinary Raven language
services. Follow-on slices now provide ordinary fragment completion and the
first typed-local scope bridge. DSL-specific completion, public retained
structure, and Playground preview support remain outside this boundary.

### MVP exit checklist

* [x] one immutable compiler-owned snapshot combines tokens and fragments
* [x] authored and body-relative spans have documented coordinate systems
* [x] cursor lookup is deterministic for complete and zero-width regions
* [x] optional provider failures cannot break unrelated semantic queries
* [x] repeated queries reuse the owning semantic model's cached result
* [x] semantic highlighting consumes the compiler snapshot
* [x] the checked-in HTML macro proves token metadata, embedded-expression
  regions, cursor lookup, and authored diagnostics end to end
* [x] the HTML/Blazor projects build and their generated fragments execute
* [x] the checked-in LINQ-like query macro proves that the same fragment and
  caller-completion contract applies to a non-HTML DSL

Cancellation stops an active query rather than returning partial data. Editing
creates a new compilation and semantic-model snapshot; no mutable provider
state is carried across that boundary by this contract.

## Predicted post-MVP DSL tooling slices

These are dependency-ordered predictions, not promises. Each slice should be
accepted only when the preceding use case demonstrates its value.

1. **Project tooling contributions from `macro` (implemented).** The
   `fragment` and `token` contributions publish fragment kind/span pairs,
   typed fragment locals, stable token kind names, and lightweight token
   classifications through the ordinary expansion result. Common macros do not
   need provider classes, and the generated adapter stays private.
2. **Route ordinary completion through fragment spans (implemented).** At a
   cursor selected by `FindFragmentRegion`, reuse Raven's existing expression,
   statement, type, pattern, or member completion against the authored
   fragment. Do not add a DSL completion API yet.
3. **Bridge macro-introduced semantic scope (typed-local MVP implemented).**
   Add a compiler-owned description
   of names and types visible inside a fragment only after query-like or
   template-like examples prove caller scope alone is insufficient. Define
   hygiene and shadowing here rather than in the editor.
4. **Route ordinary hover and definition through fragment spans
   (implemented).** Resolve expression and statement fragment symbols through
   the compiler-owned semantic model. Reuse normal Raven hover and definition
   presentation. Optional declaration spans give macro-introduced locals a
   navigation target without introducing DSL nodes. Nested token-tree macros
   recursively inherit the lexical bindings at their invocation, so composed
   DSLs use the same compiler API without language-server special cases.
5. **Project ordinary symbols from outer DSL tokens (implemented).** An
   optional token-symbol provider and `MacroTokenInfo.Symbol` let component
   tags, schema identifiers, and similar tokens reuse normal Raven hover and
   definition. Metadata failures degrade per token, and no DSL structure or
   presentation API is exposed. Token lookup descends through reported Raven
   fragments into nested token-tree macros, and an explicit token association
   takes precedence over a containing fragment. Macro invocation hints remain
   on the invocation name rather than occupying unresolved text in its body.
6. **Add optional non-symbol DSL affordance providers.** Introduce narrowly
   scoped completion, hover, and navigation contributions for outer DSL tokens
   only when token kind, fragment semantics, and ordinary Raven hover cannot
   express the experience. Compiler APIs normalize and cache them; the language
   server remains a presenter.
7. **Retain private structure snapshots when measurement justifies it.** Permit
   a provider-owned immutable snapshot shared between expansion and tooling if
   repeated parsing is materially expensive. Do not standardize or expose its
   node model to Raven analyzers by default.
8. **Harden execution boundaries.** Add compiler-owned recursion, work, output,
   and nested-compilation limits before supporting more powerful syntax-tree
   construction or compilation APIs. Consider process isolation only when the
   package and host model requires it.
9. **Lower natural callbacks to Blazor event callbacks (implemented).** When a component
   attribute resolves to an `EventCallback` property, accept an ordinary Raven
   callback reference or an inline lambda expression and let the HTML macro
   emit `EventCallback.Factory.Create(self, callback)`. Attribute bodies remain
   ordinary Raven expression fragments, so both
   `Toggled={callback}` and `Toggled={(id) => toggleTodo(id)}` use the same
   lowering. `EventCallback<T>` uses `Action<T>` target typing and the generic
   factory call, as demonstrated by the Todo item ID callback. Keep this
   Blazor-aware conversion out of the generic macro symbol/tooling contracts.
10. **Extract the HTML/Blazor macros as distributable libraries.** Prove restore,
   reference, diagnostics, generated Blazor shape, and an external consumer
   before treating the React-like sample as a reusable product surface.
11. **Prototype Playground preview as a library consumer.** After distribution,
   add compile-and-inspect component discovery, then explicit interactive
   mounting. The Playground must not copy or special-case the HTML parser.

Public custom DSL trees, a universal macro AST, and automatic structural editor
integration are intentionally absent. They can be reconsidered after slices 2
through 6 provide evidence that spans, token semantics, and narrow capability
providers are insufficient.

## Active slice: concise macro declarations

Status: **initial executable slice implemented and validated**

`macro` is the source-level authoring layer over the stable provider
contracts:

* [x] dedicated compilation-unit and namespace-member syntax
* [x] `IMacroDeclarationSymbol` semantic identity, parameters, generic parameters,
  constraints, target information, and semantic classification
* [x] same-compilation lowering for non-generic compilation-unit declarations
* [x] argument-style, attached, `ExpressionSyntax`, and `IMacroTokenStream`
  parameter roles
* [x] synchronous `expand`, `replace`, and `introduce` contribution statements
* [x] project and Playground samples
* [x] curated parser, semantic, expansion, and project-runtime coverage

Generic invocation and executable namespace-member declarations remain later
layers. Namespace-qualified lookup, imported short names, and a common symbol
projection for class-authored providers also remain open tooling work.

The next compatibility-breaking architecture slice is the stable macro ABI:

* project every macro as a nominal definition type with one designated
  `Expand` method;
* make that type the owner of macro generic parameters and constraints;
* attach caller-supplied and compiler-supplied binding sources to the method's
  ordinary parameter symbols;
* move equality, lookup, construction, and tooling to those canonical symbols;
* dispatch compiled providers through one immutable, erased execution context;
  and
* remove generated parameter objects and category-specific provider bridges
  after built-in and class-authored providers migrate.

Typed expression facades, generic inference, and overloads are deliberately
outside this slice. The ABI only preserves the symbolic type and syntax-input
information they will require later.

The current adapter lowering reparses generated Raven source inside the local
macro partition. Compiler-generated locals are allocated against authored
identifiers so macro parameters and target bindings cannot collide with
adapter plumbing. A later structural lowering/source-map slice should replace
the textual adapter boundary before generated diagnostics are expected to map
precisely into arbitrary macro bodies.

Validation record for this slice:

* `scripts/test-feature-suite.sh macros`: 89 passed
* `scripts/test-feature-suite.sh macros --runtime`: 17 passed
* focused macro parser and symbol tests: 19 passed
* `macro-declarations` project build and runtime output: `42`, `42`, `6`

## Active slice: macro invocation completion

Status: **implemented and validated**

Macro names are discoverable from the first `#`, including incomplete syntax:

* [x] trigger language-server completion on `#` and `[`
* [x] offer only invocable and token-tree macros in expression positions
* [x] offer only attached macros in declaration attribute positions
* [x] insert `#[Macro]` when completion starts from a bare declaration `#`
* [x] retain partial-name, invocation-shape, and typed-parameter completion

Validation record for this slice:

* focused compiler macro-completion tests: 14 passed
* focused language-server completion registration and mapping tests: passed

## Active slice: typed token-tree inputs

Status: **implemented and validated**

Token-tree macros may combine ordinary typed invocation arguments with one
unrestricted brace-delimited body:

```raven
let result = query!(Dialect: "sql") {
    from user in users
    select user.Name
}
```

This applies the existing parameter-object model where it matters without
constraining DSL content:

* [x] parse a parenthesized argument list followed by a token-tree body
* [x] add `ITokenTreeMacro<TParameters>` and
  `TokenTreeMacroContext<TParameters>`
* [x] expose raw `ArgumentList` and parsed `Arguments` on token-tree contexts
* [x] bind positional and named constant arguments through the shared
  `MacroParameterBinder`
* [x] preserve custom token-stream and keyword-provider capabilities for typed
  macros
* [x] reject arguments supplied to non-generic token-tree macros
* [x] demonstrate typed arguments plus custom raw-body lexing in the executable
  token-stream sample
* [x] pass the complete macro feature suite and sample validation

Validation record for this slice:

* focused parser, semantic, completion, and interface-conformance tests:
  49 passed
* complete macro feature suite: 61 passed
* Raven-authored `macro-token-stream` provider and application build: passed
* `macro-token-stream` runtime output: `42`

## Active slice: typed parameter tooling schema

Status: **implemented and validated**

Typed parameter objects are the .NET-native value-input contract for macros.
They do not replace syntax-role inputs such as raw token streams or embedded
Raven expressions.

* [x] add compiler-owned `MacroParameterDescriptor` and
  `MacroParameterKind`
* [x] expose normalized parameter types and descriptors through `MacroFacts`
* [x] describe required/defaulted positional constructor parameters
* [x] describe writable or init-style properties as named parameters
* [x] offer unused named parameters in attached, argument-style, and token-tree
  macro completion
* [x] include Raven-facing parameter types in completion descriptions
* [x] pass the complete macro feature suite and sample validation

Validation record for this slice:

* focused parameter API and macro completion tests: 12 passed
* complete macro feature suite: 61 passed
* Raven-authored `macro-token-stream` provider and application build: passed
* `macro-token-stream` runtime output: `42`

## Active slice: compiler-owned macro signature help

Status: **implemented**

Macro signature help builds on the same typed parameter descriptors as binding
and completion:

* [x] expose `SemanticModel.GetMacroSignatureHelp(...)` and the corresponding
  `Compilation` convenience API
* [x] resolve attached, argument-style, and token-tree macro invocations in the
  compiler
* [x] identify the active positional or named parameter
* [x] expose Raven-facing parameter type names
* [x] keep protocol formatting in the language server while preventing it from
  rediscovering macro contracts
* [x] show the token-tree body shape in editor signature help

Embedded-language classification is a separate input/structure concern.
`StringSyntaxAttribute` metadata can be imported for actual string-valued macro
parameters, consistent with ordinary Raven parameters. It must not become the
token-tree body contract: bodies such as `quote!{ ... }` retain tokens and
source spans and need a general syntax-content descriptor that can identify
Raven, a standard embedded format, or a custom macro-defined language.

Validation record for this slice:

* focused compiler semantic-model tests: 2 passed
* complete macro feature suite: 61 passed
* language-server signature-help tests: 7 passed

## Active slice: raw token-tree expression macros

Status: **implemented and validated**

Target syntax:

```raven
let result = query!{
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
* lower expansion results through the existing invocable-expression binding
  and emit path
* preserve existing `name!(...)` behavior

Current implementation work:

* [x] add raw token-tree body syntax to `InvocableMacroExpressionSyntax`
* [x] add a compiler-owned raw delimited-body scanner
* [x] add `ITokenTreeMacro`
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
* [x] add a runnable custom-token-stream sample project
* [x] update the language specification and changelog
* [x] pass the complete macro feature suite

Validation record for this slice:

* `scripts/test-feature-suite.sh macros`: 41 passed
* focused default-overlay and custom-provider tests: 2 passed
* `macro-token-stream` runtime output: `42`

## Active slice: direct-lowering macro MVP

Status: **implemented and validated**

The first developer-facing token-tree macro is intentionally small:

```raven
let shouldRetry = guard!{
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
* focused `InvocableMacroCodeGenTests`: 3 passed
* `scripts/test-feature-suite.sh macros`: 41 passed
* `scripts/test-feature-suite.sh macros --runtime`: 15 passed after the
  attached-property accessor identity hardening slice below

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
let verdict = choose!{
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
* focused `InvocableMacroCodeGenTests`: 5 passed
* `scripts/test-feature-suite.sh macros`: 41 passed

This remains intentionally structure-free. It establishes the clause and
fragment mechanics needed before attempting a LINQ-like sequence of
`from`/`where`/`select` clauses.

## Active slice: LINQ-like query MVP

Status: **implemented and validated**

The first query-shaped macro supports one range variable, an optional filter,
and one projection:

```raven
let result = query!{
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

InvocableMacroExpansionResult {
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

## Active slice: Raven type, pattern, compilation-unit, and member fragments

Status: **implemented**

The fragment toolbox now applies the expression/statement API shape to three
additional categories:

```raven
let type = context.ParseTypeResult(typeSpan)
let pattern = context.ParsePatternResult(patternSpan)
let unit = context.ParseCompilationUnitResult(declarationSpan)
```

* [x] provide concise syntax-only and diagnostic-bearing overloads
* [x] accept the complete body or a selected body-relative span
* [x] reject trailing input outside the selected fragment
* [x] retain authored positions for recovered syntax and diagnostics
* [x] expose matching `SyntaxFactory` entry points for standalone/generated
  text
* [x] require exactly one top-level declaration for member parsing
* [x] diagnose empty, multiple, import/alias/attribute-only, and global-
  statement inputs without silently selecting one

Compilation-unit parsing already enables macros to parse arbitrary Raven
declaration text into an immutable syntax tree. The narrower
`ParseMemberDeclarationResult` helper returns recovered member syntax together
with `RAVM022` when its exact-one contract is not satisfied. The standalone
`SyntaxFactory.ParseMemberDeclaration` form returns null for the same shape
failures.

## Active slice: embedded Raven fragment regions

Status: **implemented**

Token-tree macros can implement `IMacroFragmentProvider` to surface the spans
that contain ordinary Raven syntax without publishing their private DSL parser
representation:

```csharp
context.CreateFragmentRegion(MacroFragmentKind.Expression, expressionSpan)
```

* [x] use body-relative spans as the macro-authoring primitive
* [x] map each region to its absolute authored source span in the compiler
* [x] distinguish expression, statement, type, pattern, and member categories
* [x] permit zero-width expected regions for incomplete-code completion
* [x] resolve the optional provider through `SemanticModel` and `Compilation`
* [x] isolate optional tooling-provider failures from other semantic queries
* [x] project fragment-region contributions from `macro` syntax
* [x] route ordinary Raven completion inside reported fragment regions using
  the invocation's caller scope
* [x] describe explicitly typed fragment locals, including query-introduced
  sequence-element locals visible inside selected fragments
* [x] resolve ordinary symbol/type hover in expression and statement fragments
  through a compiler-owned span query, including caller and fragment locals
* [x] let an expression span carry an optional target type so ordinary lambda
  inference, hover, and completion can use the DSL's real contextual type
* [x] isolate provider failures and detached recovered fragment syntax so a bad
  optional tooling request cannot invalidate later semantic queries

This is deliberately a token-and-span API. It does not require or expose a
secondary DSL syntax tree. Completion reparses only the selected ordinary Raven
fragment and delegates to the existing compiler completion provider. Semantic
scope bridging beyond immutable name/type locals remains a later independently
justified slice.

The target-typed form is intentionally expression-specific:

```csharp
context.CreateExpressionFragmentRegion(expressionSpan, expectedDelegateType)
```

This avoids adding HTML, SQL, or other DSL structure to Raven's syntax and
bound trees. A macro supplies only a span and an ordinary compiler type symbol;
the existing binder provides normal target-typed semantics.

The Raven-authored `query!` sample reports its source, predicate, and projection
through this same API. Caller-scope completion works in those spans, and a
compiler-owned sequence-element local makes its range variable available only
inside the predicate and projection. Broader scope shapes remain deferred until
another DSL demonstrates their value.

## Active slice: classified macro tokens

Status: **implemented**

`SemanticModel.GetMacroTokens` and its `Compilation` counterpart now surface
the token stream selected by a token-tree macro. Each `MacroTokenInfo` carries
the original `SyntaxToken`, provider-owned `RawKind`, text, body-relative span,
absolute authored span, and an optional `MacroTokenClassification`.

* [x] preserve provider-owned raw kinds without extending Raven `SyntaxKind`
* [x] map token spans into authored source coordinates
* [x] classify declared macro keyword overlays automatically
* [x] allow a lightweight optional `IMacroTokenClassifier`
* [x] expose standard and provider-defined token kind names
* [x] isolate optional tokenization/classification failures from semantic tools
* [x] normalize optional metadata failures per token without losing the stream
* [x] cache token and fragment-region results in the owning semantic model
* [x] consume available macro token classifications in semantic highlighting
* [x] project token kind and classification metadata from `macro` syntax

The classifier sees the same stream tokens used by expansion. It labels tokens;
it does not publish the macro's parser nodes or alter ordinary Raven lexing.

`GetMacroInputSnapshot` is the normal combined query for editor consumers. It
groups the cached tokens and fragment regions for one authored body while
retaining the narrower queries for callers that need only one side.
Fragment regions are source ordered, and `FindFragmentRegion(position)` returns
the narrowest matching region. Zero-width expected slots match their exact
authored position.

## Validation case: compile-time file embedding

Status: **test macro implemented; tracked resource API deferred**

A test-only `embedText!(path)` procedural macro reads a file during expansion,
returns its contents as an ordinary string literal, and maps read failures to
the authored path argument. This demonstrates both compile-time execution and
early diagnostics.

Production file-reading macros still need compiler-owned path resolution,
dependency recording, incremental invalidation, determinism, cancellation, and
file-access policy. Direct file I/O is not promoted as the public resource
contract.

## API ergonomics: expansion-result factories

The invocable macro MVP now provides
`InvocableMacroExpansionResult.FromExpression`, `FromDiagnostic`, and
`FromDiagnostics`. These cover plain expression success, success with
forwarded parser diagnostics, macro-authored diagnostic-only results, and
combined native/macro diagnostics while normalizing default immutable arrays.
The built-in quote implementation and representative query and Playground
macros use these factories. Mutable properties remain for compatibility.

Attached macros now have the matching `MacroExpansionResult.FromReplacement`,
`FromIntroducedMembers`, `FromPeerDeclarations`, `FromDiagnostic`, and
`FromDiagnostics` factories. `FromReplacement` overloads cover replacement
alone, replacement plus introduced members, and replacement plus introduced
members and peer declarations. This captures every currently supported
attached output category without replacing the compatibility property surface.

## MVP hardening: stable replacement-property accessors

Status: **implemented and validated**

Attached property macros reuse the target property's effective symbol. Their
replacement accessors must have the same stability: the accessor reached
through `IPropertySymbol.GetMethod` or `SetMethod` must be the exact symbol
returned from the containing type's member table.

The declaration-signature pass now marks explicit property accessors as
signature skeletons. The first complete property bind replaces those skeletons
with fully bound accessors, and later binds reuse the registered completed
symbols for the same effective declaration. This avoids equivalent-but-distinct
accessor objects when semantic queries, diagnostics, and emit request the
replacement property in different orders.

Validation record for this slice:

* focused attached replacement-property emit and identity test: passed
* focused macro, property-binding, and async-property suites: 99 passed
* `scripts/test-feature-suite.sh macros`: 52 passed
* `scripts/test-feature-suite.sh macros --runtime`: 15 passed

## Authoring hardening: actionable expansion failures

Status: **implemented**

Typed macro interfaces currently dispatch through reflection after their
parameter objects are bound. Expansion exceptions from that path must report
the macro author's actual failure, not `TargetInvocationException`'s generic
wrapper message.

The expansion service now unwraps reflection invocation failures for both
attached and invocable typed macros and reports the underlying message
through `RAVM020` at the authored macro name. Expected input validation remains
on the source-located `MacroExpansionDiagnostic` path; this diagnostic is for
unexpected macro implementation failures.

This is the first explicitly authoring-oriented hardening slice. The next
authoring work should follow the minimum loop documented in
[Macro and DSL developer experience](developer-experience.md): local macros,
typed parameters, `#quote`, result factories, source-located diagnostics, and
expanded-source inspection before retained DSL structure.

Validation record for this slice:

* focused typed attached and invocable failure tests: 2 passed
* focused attached and invocable semantic suites: 43 passed
* `scripts/test-feature-suite.sh macros`: 54 passed

## Authoring hardening: cancellable expansion

Status: **implemented**

Macro contexts already carry a caller-provided cancellation token, but the
expansion service previously caught `OperationCanceledException` and converted
it into `RAVM020`. Typed macro cancellation was additionally wrapped in
`TargetInvocationException`.

The expansion service now:

* checks cancellation before resolving and invoking each macro;
* unwraps reflection invocation failures before classifying cancellation;
* propagates direct and reflection-wrapped `OperationCanceledException`;
* avoids reporting cancellation as an implementation failure; and
* leaves the expansion cache empty so a later uncanceled request can retry.

Validation record for this slice:

* focused direct and typed attached/invocable cancellation tests: 4 passed
* focused attached and invocable semantic suites: 47 passed
* `scripts/test-feature-suite.sh macros`: 58 passed

## SDK integration: provider-declared compiler plugins

Status: **implemented and compatibility item retired**

Replace consumer-authored `RavenMacro` items with provider-declared
compiler-plugin assets:

1. [x] a Raven macro project marks its output with
   `[assembly: RavenCompilerPlugin]`;
2. [x] consumers use a normal Raven project dependency;
3. [x] the SDK resolves the marked asset and passes it to the compilation;
4. [x] the compiler loads its manifest and exported macro contracts; a
   repeatable assembly-level marker may explicitly name macro definitions,
   while a bare marker authorizes fallback discovery within that marked
   assembly; and
5. normal duplicate-name and load diagnostics continue to
   apply.

Do not scan every ordinary runtime reference for macro implementations.
Provider metadata is the explicit execution boundary; consumer source imports
are unnecessary because macro names are registered with the compilation.
Prefer explicit macro types in the manifest and restrict reflection discovery
to assemblies carrying the compiler-plugin marker.

The MVP recognizes the assembly marker syntactically in evaluated
Raven and C# project sources, builds the provider through the appropriate
language build path, and adds its output as a `MacroReference` rather than a
runtime `ProjectReference`. Unmarked project references retain their ordinary
behavior. The representative `macro-invocable` and `macro-add-equatable`
samples cover Raven-authored invocable and attached providers respectively.
The compiler continues to support .NET-authored providers through the same
object-oriented contracts, but runnable language samples use Raven.

The transitional consumer-authored `RavenMacro` project item has been retired.
Project loaders report a migration error directing existing projects to an
ordinary `ProjectReference` and the provider marker.

Validation for project-item retirement:

* focused MSBuild and legacy-project migration suite: 16 passed
* focused language-server macro-definition tests: 2 passed
* focused project-backed workspace tests: 4 passed
* complete macro feature suite: 61 passed
* migrated `macro-observable` runtime output: `Title`, `Hello from Raven`
* migrated `macro-reactive` runtime output: `1`, `2`

Package resolution now keeps ordinary compile assets and compiler-plugin
implementations separate. A package may expose `ref/<tfm>/Provider.dll` to
consumer binding while placing its marked implementation in
`lib/<tfm>/Provider.dll`; only the reference assembly enters the consumer
metadata graph, while the implementation becomes a `MacroReference`. The macro
load context also probes beside the implementation for package-local helper
assemblies when no application `.deps.json` is available. When NuGet restore
produces `project.assets.json`, every runtime assembly in the selected target
graph—including assets from transitive packages—is available as an
identity-checked private dependency probe for the macro load context. Those
runtime-only probes do not become consumer metadata references.

Validation for split-package activation:

* focused macro-reference and package activation tests: 15 passed
* focused package resolution tests, including a transitive runtime-only
  dependency: 4 passed
* complete MSBuild project-system service suite: 14 passed
* `scripts/test-feature-suite.sh macros`: 58 passed

Explicit manifests use
`[assembly: RavenCompilerPlugin(typeof(QueryMacro))]`, repeated for each
exported macro. The loader validates that every declared type belongs to the
marked assembly, is a concrete definition implementing exactly one macro role,
and has a public parameterless constructor. Invalid or mixed explicit/fallback
manifests are
reported through the existing `RAVM001` compiler diagnostic. Assembly, image,
and file activation share this behavior.

Dedicated macro declaration syntax is deliberately outside the infrastructure
MVP. First stabilize token streams, ordinary OO authoring, expansion,
diagnostics, and project activation. A later shorthand may remove class
boilerplate only by lowering to or interoperating with these same contracts.
It may also synthesize the assembly export registration currently expressed
through `RavenCompilerPlugin`.

The function-like declaration uses parameter roles, an invocable return-type
target or an attached `on` parameter, and body contributions to describe the
macro:

```raven
macro Foo(argument: ExpressionSyntax) -> ExpressionSyntax {
    // ...
}

macro Query(body: IMacroTokenStream) -> ExpressionSyntax {
    // ...
}

macro AddEquatable(on target: BaseTypeDeclarationSyntax) {
    introduce CreateEqualityMembers(target)
}
```

`macro` is intended to be contextual before the existing `func` declaration,
keeping the new keyword surface narrow. This is defined as source-level
lowering over the shared dynamic and typed macro infrastructure. The compiler
currently synthesizes the corresponding parameter-object class,
category-specific adapter, and `Expand` method, but these generated types are
not the semantic identity of the macro declaration and are not a compatibility
constraint. Local declarations enter the compile-time partition without
assembly export metadata. Only an explicitly exported declaration in a
provider assembly synthesizes provider manifest metadata.

The model separates user inputs, compiler-supplied roles, invocation targets,
and `expand`/`replace`/`introduce` behavior. `expand` is the semantic return for
an invocable expansion path; attached `replace` and `introduce` statements
accumulate result state until body fall-through. Diagnostics accumulate through
the context API rather than another language statement. It
must cover attached, argument-style, and token-tree macros without
reintroducing a separate `MacroKind` annotation. The detailed lowering matrix lives in
[Macro and DSL developer experience](developer-experience.md).

Macro binding classifies parameters by explicit `on` syntax and recognized
compiler API types into `MacroParameterRole`. Value parameters populate the
generated typed parameter object normally. `ExpressionSyntax` parameters retain
that real compiler API type and receive the caller's authored node. An `on`
parameter receives the attached source node and never consumes a call-site
argument. A single
`body: IMacroTokenStream` parameter is
instead supplied through `TokenTreeMacroContext.CreateTokenStream()` and
selects token-tree invocation syntax.

Context parameters are optional capabilities. Simple macros bind only their
declared caller inputs and output target; generated adapters must not require
authors to declare a context they do not use.

The future strongly typed layer also includes symbolic generic arguments.
Explicit macro type arguments bind to `ITypeSymbol` values, participate in
constraint validation before expansion, and remain distinct from CLR generic
parameters on the provider implementation. A macro declaration may eventually
declare a call-site semantic result type, while its implementation supplies
syntax that the compiler binds and verifies against that result. Generic
inference and overload resolution remain later layers. For invocable macros,
the return type declares grammar targets rather than a separate call-site
semantic result type; the expanded ordinary syntax is subsequently bound and
type-checked normally.

Validation record for this slice:

* focused manifest, referenced-assembly, package, and unmarked local macro
  activation tests: 14 passed
* complete MSBuild project-system service suite: 14 passed
* `scripts/test-feature-suite.sh macros`: 58 passed
* compiler-driver `macro-invocable` project validation: passed
* `macro-invocable` runtime output: `42`, `False`, `correct`, `70`,
  `answer + 1`
* dedicated `macro-quote` runtime output: `42`

Attached compiler-plugin project validation:

* `macro-add-equatable` Raven provider runtime output: `True`, `False`
* `scripts/test-feature-suite.sh macros`: 58 passed
* compiler-driver `macro-add-equatable` project validation: passed
* `macro-add-equatable` runtime output: `True`, `False`

Referenced-assembly discovery validation:

* marked direct metadata-reference activation: passed
* marked package assembly activation from the global package cache: passed
* unmarked reference behavior remains unchanged

Explicit entry-point manifest validation:

* focused macro-reference tests: 10 passed
* `scripts/test-feature-suite.sh macros`: 58 passed
* compiler-driver `macro-invocable` project validation: passed
* `macro-invocable` runtime output: `42`, `False`, `correct`, `70`,
  `answer + 1`

## Historical slice: default environment and in-memory activation

Status: **superseded by the Raven.Macros companion assembly**

* [x] centralize compiler-provided macro registration in a default environment
* [x] use the same default environment for binding and completion
* [x] register compiler-provided macros without project items while retaining
  ordinary namespace/import lookup
* [x] allow a macro assembly image to be activated without writing it to disk
* [x] prove the image path with a Raven-authored macro that imports
  `Raven.Macros.*` and uses `quote!`

The original compiler-owned default environment has been removed. Standard
`quote` and `compile` declarations now come from the Raven-authored
`Raven.Macros` plugin, while in-memory image activation remains the execution
boundary used by the Playground and local macro partitions.

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
* [x] automatically classify direct macro declarations in compiler and
  workspace construction
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
* [x] run workspace analyzers over macro and consumer projections with their
  owning semantic models

The automatic MVP uses a syntax-only rule. A top-level declaration whose base
list names one of the category-specific macro interfaces is moved into the
compile-time partition. A file containing only such declarations becomes a
dedicated macro tree automatically. Supporting types can be moved
declaration-by-declaration with `[LocalMacro]`; that attribute is distinct from
both `#[...]` macro invocations and the assembly-level `RavenCompilerPlugin`
marker for reusable compiler-plugin dependencies.

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

The Workspace analyzer driver likewise resolves every compiler-owned projection
for an authored document. A mixed `[LocalMacro]` document is traversed once in
the consumer semantic model and once in the macro semantic model. Registered
syntax-tree, syntax-node, symbol, and operation actions therefore see ordinary
Raven macro implementation code without analyzers becoming part of macro
activation or direct `Compilation` use. The document remains one analyzer cache
unit, and diagnostics retain authored positions because both projections are
position-preserving. This does not yet expose Raven fragments parsed inside a
DSL body; that remains gated on an explicit retained-structure contract.

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
* focused mixed-document analyzer projection and semantic-model test: passed
* browser Playground smoke test, including every example: passed

Next planned slice: continue hardening the minimum direct-expansion macro
experience. Retained DSL structure and embedded-fragment analyzer routing stay
documented as future work until the basic authoring, expansion, diagnostics,
and Playground workflows are stable.

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
typed positions. `InvocableMacroExpressionSyntax` is the expression carrier;
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

Expression, statement, type, pattern, compilation-unit, and exact-one member
helpers now provide diagnostic-bearing parse results with authored locations.
Future fragment categories should preserve the same source mapping and result
shape rather than adding an untyped syntax-node parser.

### Fresh generated binding names

Status: **textual collision avoidance implemented**

All macro contexts expose `CreateUniqueName(hint)`. Allocation is deterministic
within an invocation and avoids identifiers authored anywhere in its document
plus names returned by earlier calls on the same context. This is the Raven
counterpart to the immediately useful part of Nim's `genSym`: macro authors no
longer need to invent temporary-name suffixes themselves.

`CreateUniqueIdentifier(hint)` is the concise syntax-construction form for the
same allocation policy. Neither helper is presented as semantic hygiene.

The helper does not yet encode a hygienic symbol identity or choose
definition-site versus call-site lookup. Those semantic facilities remain a
later slice and must be designed before declaration and statement quotation is
stabilized.

### Diagnostic-first syntax category requirements

Status: **implemented**

All macro contexts expose `RequireSyntax<TSyntax>`. It returns the original
typed node on success; on mismatch it accumulates a macro error and returns
`null` instead of throwing. Source-backed fragments use their authored origin
for the diagnostic, while detached generated nodes fall back to the invocation.

The nullable result is transitional while `Raven.CodeAnalysis` cannot depend
on Raven's bootstrapped core unions. The intended Raven-facing projection is
`Option<TSyntax>` for this two-state helper. Richer helpers should use
`Result<T, TError>` or purpose-built unions when distinct failure/recovery
cases add value, rather than accumulating ad hoc nullable fields.

### Invocation-scoped semantic inspection

Status: **expression type and symbol inspection implemented**

`TokenTreeMacroContext.GetTypeInfo` and `GetSymbolInfo` inspect parsed Raven
expressions in the invocation's caller scope. The helpers resolve caller
locals, parameters, members, and imports through compiler-owned semantic
state, without requiring macro authors to construct a detached semantic model.

### Cursor-based Raven fragment parsing

Status: **implemented for expressions, statements, types, patterns, and members**

`TokenTreeMacroContext.CreateTokenStream()` now returns a context-bound
`MacroTokenStream` that retains ordinary token cursor operations and adds
`ParseExpression`, `ParseStatement`, `ParseType`, `ParsePattern`, and
`ParseMemberDeclaration`. Each method parses at the current token, advances through the construct, and returns
`MacroSyntaxParseResult<TSyntax>` with recovered syntax, diagnostics, and its
`BodyRelativeSpan`. Stream synchronization uses the parser's consumed position
internally, so error recovery can advance safely without misrepresenting the
recovered node's own span.

The existing complete-body and explicit-`TextSpan` APIs remain available.
Explicit spans are required when an outer DSL delimiter could also be consumed
as Raven syntax. Compilation units inherently consume their selected input.
Cursor member parsing uses a dedicated parser entry point and therefore does
not select the first declaration from a complete compilation-unit parse.

### Additional expansion positions

Design status: **expression, raw-body statement, and member-list expansion implemented**

The [macro application model](application-model.md) defines expression,
statement, member, type, pattern, and attached scenarios; return-type targets;
actual-position context; carrier selection; typed simple APIs; the validated
multi-position ABI; and implementation order. The MVP result carries zero or
one `SyntaxNode` and validates expression versus statement placement before
binding. A bare raw-body `Name! { ... }` invocation selects statement placement;
parentheses retain expression placement. A union annotation supplies an exact
closed position set, while `-> SyntaxNode` explicitly opts into all single-node
invocable positions. Neither form includes attached or list-valued
contributions. Member expansion uses `SyntaxList<TMember>` for a list-valued
namespace- or type-member result and normalizes it to an immutable member
array. Compact declarations project that return type to namespace-member and
type-member positions, and generated providers preserve those targets through
the class ABI. Empty output removes the carrier, ordering is preserved, and
validation is atomic. This reuses Raven's existing syntax collection rather
than adding macro-only multiplicity syntax. Type, pattern, and typed-wrapper
carriers remain post-MVP work.

Here “untyped” means category-erased to the `SyntaxNode` base type, not dynamic
or text-based expansion.

For an invocable macro, the return type is normative target metadata: it
decides where resolution and completion offer that macro before expansion runs.

The accepted attached syntax moves `on` into the parameter list:
`on target: TargetSyntax`. The parameter type or union decides attachment
targets, while `on target: SyntaxNode` is category-untyped. The parameter binder
must normalize value, syntax-input, context, token-body, and attached-target
roles before mapping positional or named arguments; compiler-supplied roles do
not consume call-site arguments.

### Expansion driver and isolation

Move expansion scheduling, caching, recursion detection, source mapping, and
resource limits into a compilation-owned macro driver. Consider an
out-of-process host only after the in-process contracts and performance model
are stable.

### Compiler-owned `#quote`

Status: **expression quote and expression-hole MVPs implemented**

`Raven.Macros.Quote` is now a compiler-registered token-tree macro and needs no
plugin reference. Its `quote` alias requires `import Raven.Macros.*`; the
canonical qualified name remains available without that import. The first
slice:

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

Later quote slices add contextual category selection; statement, member, type,
pattern, and declaration quote categories; category-checked holes;
token/identifier/list/repetition splices; and compiler-owned source provenance
for generated syntax. Bind/equivalence verification should ensure quote and
factory construction produce the same valid nodes. Hygiene and definition-site
versus call-site name lookup must be settled before declaration quotes are
stabilized. These facilities reuse normal fragment semantics; `quote!` does
not need outer-DSL token-symbol providers because its body is Raven syntax.
