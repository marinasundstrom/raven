# Raven Changelog

Behavior-focused timeline covering **2025-09-12** to **2026-05-09**.

## Unreleased

- Nullability-flow wrappers now lower through their underlying expression
  instead of disappearing during bound-tree rewriting. Narrowed values remain
  valid as invocation arguments and match scrutinees through emission, while
  disabled debug tracing no longer formats entire bound expressions eagerly.
- Raven-emitted generic interfaces now preserve `in` and `out` variance in
  their CLR generic-parameter metadata. Reloaded PE symbols consequently apply
  the same covariant and contravariant interface conversions as source symbols.
- Constructed generic properties, indexers, and events now project accessors
  back to the constructed member, and substituted parameters are owned by their
  constructed method rather than directly by the containing type. Raven source
  and emitted PE symbols now agree across the complete member graph.
- Metadata `[MaybeNull]` output contracts can now place a declared non-nullable
  reference in the maybe-null flow state. The declared Raven type remains
  unchanged, null patterns can refine it again, and branch joins preserve the
  possible-null diagnostic in either semantic-query order.
- Null-flow state captured by `break` now passes through an enclosing
  `finally`: possible assignments invalidate pre-finally non-null facts, and an
  abrupt `finally` replaces pending protected-region breaks. Loop exits no
  longer publish stale nullability after cleanup code runs.
- Generic inference and constructed signatures now preserve type parameters
  nested through nullable arrays, tuples, and constructed generic arguments.
  Raven tuple projections compare consistently with emitted
  `System.ValueTuple` signatures, and incomplete available-state inference
  falls back to authoritative binding instead of dropping a generic overload.
  Mixed suffixes such as `T?[]`, `T[]?`, and `T?[]?` also retain their written
  nesting order in the syntax tree.
- Raven-emitted generic type and method parameters now preserve `notnull` when
  reloaded from metadata. Constraint round-trip coverage also locks `class`,
  `struct`, `new()`, base-class, and interface constraints across source and PE
  symbols.
- Nested constructed generic types now substitute outer type arguments inside
  their own parameter constraints for both source and metadata symbols.
  Constructor binding validates the final constructed type, so dependent
  constraints cannot be bypassed by using a generic nested type as a call
  target.
- Nullable match exhaustiveness now preserves the complete non-null pattern
  domain instead of reducing it to one opaque underlying-type case. Booleans,
  enums, and sealed hierarchies can be covered case by case alongside `null`;
  open hierarchies still require a base-type or `_` fallback. Missing-case code
  fixes now generate typed sealed-hierarchy bindings from nullable scrutinees.
- Nullability documentation now defines one coherent Raven policy: unified
  nullable symbols for reference and value types, strict flow-based proof of
  safe access, pattern-first handling, `Option<T>` for domain absence, and
  direct null checks as supported compatibility forms. Compiler and analyzer
  configuration are documented as policy controls rather than alternate type
  systems.
- Generic inference for `ref`, `out`, and `in` arguments now consumes the
  referenced element type instead of leaking compiler-only address/ref wrapper
  symbols into constructed method type arguments. Exact by-reference kind and
  addressability checks still run during overload applicability.
- Semantic queries after an event- or property-accessor body edit now recover
  the accessor's method binder directly, preserving accessor parameters and
  edited local types without forcing complete source declaration binding.
- Generic calls rejected by type-parameter constraints now retain the inferred,
  constructed method as their semantic candidate. Nullable generic parameters
  are projected consistently through Raven's unified nullable symbol model for
  both source and metadata methods.
- Control-flow analysis now lets an abrupt `finally` replace pending loop
  transfers before publishing exits. A `continue` in `finally` therefore
  suppresses an enclosed `break` instead of making a non-completing loop appear
  to have a reachable endpoint.
- An abrupt `finally` now suppresses enclosed loop transfers during null-flow
  back-edge analysis. Mutations before a `continue` that is replaced by a
  `return` or `throw` in `finally` no longer weaken an unreachable next
  iteration.
- Lazy source member declaration now preserves every constructed containing-type
  layer when rebuilding nested receivers. Rejected generic overload candidates
  consequently expose the same projected constraints and containing types as
  their metadata equivalents.
- Public type information for constructor invocations now reports the
  constructed containing type instead of the constructor's `unit` return type,
  including every construction layer of nested generic types.
- Constructing an error type through the public named-type API now preserves the
  same recovery symbol instead of throwing `NotSupportedException`, keeping
  semantic tooling total while source is incomplete. Error types also identify
  themselves as closed type definitions through the ordinary symbol contract.
- Local functions declared in nested statement and expression blocks no longer
  leak into enclosing scopes through their emitted container. Lexical function
  overload sets are restored on block exit, and semantic lookup admits local
  functions only through an enclosing block.
- An incomplete generic constraint clause such as `where T:` now reports a
  localized missing-constraint diagnostic while retaining the declaration and
  later sibling declarations for semantic queries during edits.
- Constant-false loops now have an explicit null-flow contract: mutations in
  their unreachable bodies do not weaken the state at the following reachable
  statement, independent of semantic-query order.
- Constant-false catch filters likewise exclude their unreachable catch-body
  mutations from the null-state joined after a `try` statement.
- Cold public symbol queries now apply a method group's contextual delegate
  conversion instead of returning an unresolved candidate set. Imported
  generic and non-generic namespace-function overloads therefore select the
  same method before and after diagnostics are collected.
- Lexical function scopes now retain complete overload sets instead of replacing
  an earlier overload with the last declaration of the same name. Higher-order
  generic calls therefore see generic and non-generic namespace-function
  overloads consistently, independent of declaration and semantic-query order.
- Catch filters now carry true-path null facts into their catch bodies without
  leaking those facts to sibling catches. Diagnostics-first semantic queries
  also retain the expression's declared nullable type while reporting its
  narrowed flow state.
- Null-state flow now preserves incoming facts across `while false` and ignores
  mutations in such unreachable nested loops when computing an enclosing loop's
  back-edge.
- Semantic queries over object-initializer syntax no longer throw during edit
  recovery. Attached initializers report their containing construction type,
  while temporarily detached recovery nodes produce an error result.
- Null-flow analysis can be disabled with
  `CompilationOptions.WithEnableNullFlowAnalysis(false)` or the MSBuild
  property `EnableNullFlowAnalysis=false`. Declared nullability,
  boundary checks, nullable metadata, pattern refinement, and flow-sensitive
  semantic information remain active. The former "extended null flow" naming
  has been removed from compiler and workspace APIs.
- Conditional access now marks its synthesized non-null receiver conversion as
  a refinement, preventing false nullable-input diagnostics when invoking
  source extension methods through `?.`.
- Reuse unchanged local macro partitions even when they contain authored
  diagnostics, remapping those diagnostics after consumer-only workspace edits
  instead of recompiling the macro partition.

- Nullable flow now consumes metadata `MemberNotNull` and
  `MemberNotNullWhen` contracts. Facts are keyed by receiver and member, so a
  call narrows only that instance and conditional contracts apply only on their
  declared Boolean branch.
- Overload resolution now prefers an exact non-generic method over a generic
  candidate whose inferred construction produces the same parameter sequence,
  independent of declaration order. The same rule selects methods when a method
  group is converted to a delegate.
- Raven.Core's union JSON converters now preserve nullable payloads through
  reflection by carrying the declared case/member type separately from the
  possibly null value. Their Raven signatures also match the nullable .NET
  contracts used by `JsonSerializer` and reflection invocation.
- By-reference overload applicability now follows CLR signature identity for
  nullability: nullable reference annotations do not make an otherwise matching
  `ref`, `out`, or `in` argument inapplicable, while nullable value types remain
  distinct. This also lets generic metadata `MaybeNull` output contracts update
  Raven flow state after a successfully resolved call.
- VS Code hover now presents the compiler's position-specific nullable flow
  state for nullable locals and parameters. The declared signature remains
  unchanged while the hover distinguishes `maybe null` from a value narrowed
  to `not null` at the selected reference.
- Generic method inference now widens repeated type-parameter bounds for a
  base/derived argument pair instead of depending on argument order. Partial
  explicit type arguments on namespace functions also remain open through
  invocation binding, so trailing fixed arguments and leading inferred
  arguments are combined by the shared overload resolver.
- Labeled loop transfers now participate in the same null-flow joins as their
  unlabeled forms. A labeled `break` contributes its state to the target loop's
  exit, and labeled `break`/`continue` paths are classified correctly when
  determining whether a mutation can reach that loop's back-edge.
- Null-flow joins after `try` now exclude `try` and `catch` branches that cannot
  complete normally. An abrupt branch no longer weakens facts established by
  the reachable continuation solely because it was bound before the join.
- Metadata `MaybeNullWhen` contracts now invalidate a narrowed nullable
  `ref` argument on the indicated Boolean branch while preserving the opposite
  branch. By-reference arguments bind their writable declared storage shape,
  so a prior flow conversion no longer hides the symbol from post-call flow.
- Invocation and property-assignment inputs now honor .NET nullability
  contracts. Non-nullable and `DisallowNull` inputs reject null or maybe-null
  values, `AllowNull` accepts them without changing the declared/read type, and
  PE property attributes are available through the public symbol model.
- Nullable local and mutable-parameter flow now follows the assigned value.
  Definitely non-null initializers and assignments establish a fact, assignments
  from narrowed values copy it, and null or maybe-null assignments remove it;
  cached declarations replay the same initialization state.
- Source named types now expose the same local CLI `MetadataName` as PE named
  types, including generic arity but excluding namespace and containing-type
  paths. `ToFullyQualifiedMetadataName` owns complete identities, and emission
  now uses it explicitly for top-level types while retaining nested builders.
- PE by-reference parameters now retain their nullable element annotation,
  using write-state nullability for `out` parameters even when reflection
  exposes the annotation on the root by-ref node. Invocation flow applies
  `NotNull` and `MaybeNull` postconditions to `ref`/`out` arguments, including
  constructed generic methods, and replays them when the invocation is cached.
- Metadata `NotNullIfNotNull` return contracts now affect invocation flow.
  Raven resolves the named parameter from the imported attribute and projects
  a non-null result only when the corresponding argument is non-null in the
  current flow state, without changing the method's declared return annotation.
- Null-flow state now joins `try` and each `catch` from independent entry and
  exit states, then applies `finally` to the joined normal continuation. An
  assignment that may occur before an exception conservatively invalidates the
  corresponding fact at catch entry, and one mutating catch no longer leaks
  its state into the binding of a sibling catch.
- Null-flow narrowing now accounts for assignments on loop back-edges,
  including paths reached through `continue`, and the states carried by
  `break`. Values mutated by a `loop` or `while` body are no longer assumed
  non-null on later iterations or after a reachable mutated exit, while a
  `while` condition can reestablish its narrowing for each body entry.
  Assignment targets also retain their writable declared shape instead of
  being replaced by a non-null flow conversion.
- Constrained generic overloads now participate in fast semantic-query ranking
  only when their constraints are satisfied. A generic identity conversion can
  beat an `object` fallback, while a rejected generic candidate leaves the
  fallback selectable, independent of declaration and diagnostic query order.
- Fast semantic-model queries now apply generic method constraints before
  publishing an invocation target. Dependent constraints such as
  `TDerived: TBase` work for explicit and inferred calls through emitted Raven
  libraries; invalid calls expose a failed candidate instead of a selected
  method.
- Name-based PE member lookup now resolves nested generic types by their
  Raven-facing name (for example, `Inner` as well as the CLI name `Inner`1`).
  Constructed nested generic types and their methods retain equal source/PE
  identity across emit and reload, including inherited containing-type
  substitutions.
- `ConstructedMethodSymbol` object equality and hashing now use its constructed
  signature identity instead of proxying to the open method definition.
  Reflexive and independently reconstructed methods behave consistently in
  ordinary sets as well as with `SymbolEqualityComparer`.
- Source and PE symbols now retain stable identity across an emit/reload generic
  boundary. PE modules expose their scope name and containing assembly when
  reflection omits a module file name, and source generic method
  `MetadataName` follows the CLI name rather than documentation-ID
  double-backtick notation.
- Null-flow analysis now recognizes guard blocks whose final statement exits,
  as well as nested `if`/`else` guards whose branches all exit. Subsequent code
  receives the same non-null narrowing as it does after a single-statement
  guard.
- Definite-assignment analysis for `out` parameters now joins the actual exits
  from `loop` statements. Assigning before every reachable `break` satisfies
  the contract, while any unassigned break path still reports `RAV0269`.
- `MaybeNull` return contracts now affect invocation flow without rewriting the
  declared return annotation. Direct dereferences, inferred locals, and
  explicitly non-nullable local assignments observe maybe-null reference and
  constructed-generic results, while non-nullable value-type results remain
  definitely non-null.
- Conditional nullable metadata contracts now participate in Raven flow
  analysis. `NotNullWhen` narrows every annotated invocation argument on the
  matching Boolean branch without making an unsound claim on the opposite
  branch.
- Removed the unused placeholder local table and throwing declaration path from
  `LocalScopeBinder`. Lexical locals remain owned by `BlockBinder`, leaving the
  local-scope binder as the forwarding semantic boundary it actually provides.
- `IForLoopOperation` now exposes source `for let` patterns and no longer
  reports the binder's synthetic iteration temporary as the loop local.
  `while let` and pattern-based `for` loops are covered as first-class public
  operation shapes.
- PE method and parameter symbols now project nullable flow attributes from
  referenced assemblies, including return-level `MaybeNull` and parameter-level
  `NotNullWhen` constructor values. The metadata decoder is shared across
  method, return, and parameter attributes.
- Higher-order generic calls now preserve constraint failures discovered while
  constructing a method-group argument. Passing a constrained generic function
  with incompatible inferred type arguments reports the constraint violation
  instead of silently accepting the invocation.
- Symbol queries for method-group arguments now use the enclosing invocation's
  contextual binding before falling back to the visible open declaration. This
  keeps inferred method symbols stable across cold queries, diagnostic queries,
  and workspace edits.
- Constraint clauses that name undeclared type parameters now report `RAV0360`
  for functions, methods, function expressions, and macro functions instead of
  being silently ignored.
- Duplicate lexical bindings in the same scope now report `RAV0167` as binding
  errors. `RAV0168` remains a warning for intentional or accidental shadowing
  across nested scopes.
- Unsupported or programmatically constructed literal syntax now produces an
  invalid-expression diagnostic and bound error expression instead of throwing
  from semantic binding.
- Incomplete interpolated strings remain bindable as strings during edits, and
  unknown constructed content recovers through an invalid-expression
  diagnostic instead of an exception.
- Removed the obsolete `TypeUnionSymbol`, `ITypeUnionSymbol`, and
  `TypeKind.TypeUnion` compiler API and all associated conversion, inference,
  exhaustiveness, emission, analyzer, and language-service behavior. Raven's
  union feature remains independent; union syntax no longer falls back to a
  synthesized or common-base type when its required union definition is absent.
- `GetTypeInfo` now reports contextual conversions consistently for return,
  assignment, and argument expressions. In particular, explicit return values
  converted to a standard-union carrier retain their natural type while
  exposing the carrier as `ConvertedType`, independent of expression shape,
  available-state path, and query order.
- Bound-tree walkers now use generated expression and statement dispatch and
  accept every bound-node family without throwing on non-block statements.
  This prevents analysis and lowering walkers from silently skipping newly
  introduced bound expressions.
- Pattern binding now recovers from unsupported synthetic pattern kinds with a
  normal invalid-term diagnostic and error pattern instead of throwing. This
  keeps semantic APIs total for edited or programmatically constructed trees.
- Constant evaluation now folds logical negation of Boolean constants, so
  control-flow and missing-return analysis recognize loops such as
  `while !false` as non-terminating unless a reachable break exists. Boolean
  conjunction, disjunction, and same-type constant equality are folded as well.
- Control-flow analysis now recognizes parenthesized, converted, and
  all-abrupt `if` and `match` expressions as non-completing, including when
  such an expression is nested in a local initializer.
- Nullable type symbols now project the underlying type's declared members for
  both reference and value types. Public `GetMembers` therefore agrees with
  `LookupType` and `IsMemberDefined` instead of exposing only base-type members.
- Nullable symbol APIs now distinguish structural inspection from total
  normalization: `TryGetNullableUnderlyingType` exposes nullable wrapping,
  `GetNonNullableType` replaces the ambiguous `GetPlainType`/`StripNullable`
  helpers, and `WithNullableAnnotation` provides an immutable, idempotent
  declared-annotation transform. `TypeInfo` continues to report contextual flow
  separately from the declared type symbol, using the same semantic shape for
  nullable reference and value types.
- Metadata loading now preserves non-null owner and type contracts for PE field,
  property, event, and method symbols, using the compiler error type for
  unreadable referenced signatures. Non-constant PE fields now return no
  constant instead of invoking an invalid reflection operation, and projected
  tuple fields no longer inherit from PE symbols with absent reflection state.
  Syntax trees always expose source text; detached syntax nodes and tokens
  explicitly expose nullable parents and source behavior, while default tokens,
  node-or-token values, syntax lists, and separated lists are safe to inspect.
  Child-list and reflected-property projections now materialize stable cached
  views, detached nodes are rejected as declaration-table keys, and nested PE
  types preserve their declaring type, namespace, and module ownership.
  Separated-list tokens retain their actual parent and source position, while
  token replacement can safely descend into structured recovery trivia.
  Constructed generic methods preserve substituted array metadata, and
  synthesized entry points use the compilation's canonical `string[]` type.
  Detached symbols now expose nullable assembly and module containment in line
  with the public compiler API while retaining stable names for presentation.
  PE array loading preserves pointer and by-reference element symbol kinds
  instead of projecting every wrapper element as a named type.
- Public compiler-model queries no longer throw for array, tuple,
  and module namespace symbols. `SourceText` now provides cached line
  collections with line-break spans, validated copy operations, and cancellable
  full or span-based writes without creating substring values.
- Tuple symbols now report tuple identity consistently across source,
  metadata, constructed, and aliased symbols. `UnderlyingTupleType` is present
  only for tuple projections and is explicitly nullable for ordinary named
  types.
- Semantic-model queries now reject foreign and detached syntax consistently,
  including symbol, type, operation, capture, flow-analysis, function-parameter,
  and macro-expansion queries. Semantic-model acquisition distinguishes a null
  tree from a non-null tree that is not part of the compilation, and reversed
  statement regions no longer produce a successful control-flow analysis.
- Source fields model an absent or null constant value without violating their
  symbol contract. Failed type-resolution results without a detailed issue now
  produce the ordinary fallback diagnostic instead of throwing, and parser
  recovery coverage now checks every incomplete prefix of a macro function with
  a match body.
- Async lambda return-target and iterator signature recognition no longer
  assumes that every named type exposes an original definition or a complete
  namespace-parent chain, preventing malformed or custom symbols from causing
  binding failures.
- Nullable metadata now uses the .NET transform-flag convention for nested
  generic arguments, arrays, generic value types, and by-reference positions.
  Raven also imports both uniform and positional nullable annotations without
  changing overload applicability based only on reference-type annotations.
  Emitted source types now carry the conventional non-null nullable context, so
  unannotated reference signatures round-trip as non-null through both .NET
  reflection and Raven metadata import without redundant position attributes.
  Synthesized nullable context and constraint attributes are emitted from raw
  metadata blobs, avoiding unsupported constructor introspection when compiling
  in the WebAssembly Playground.
- Expression blocks now project `return` and `throw` items as abrupt expression
  statements; bare expression-form `return` carries implicit `unit`. `break`
  and `continue` remain statement-only and consistently report `RAV1902` or
  `RAV1903` from expression blocks, including macro bodies and blocks nested in
  loops.
- Public `TypeInfo` nullability now keeps the declared nullable annotation while
  reporting the bound expression's narrowed flow state. Strict null-check
  branches and early null guards return the same result in cold and
  diagnostics-first semantic query orders.
- Control-flow analysis now models unconditional `loop` statements, reachable
  `break` exits, literal-true `while` loops, `unsafe` blocks, and `finally`
  execution consistently. Exhaustive match statements now make their endpoint
  unreachable when every arm returns or throws; missing coverage, guards, and
  completing arms remain reachable.
- `out`-parameter definite assignment now joins exhaustive match arms and
  respects proven non-terminating loops. Raven locals continue to require an
  initializer at declaration; the unused `RAV0165` use-before-assignment
  descriptor has been removed instead of advertising an inactive rule.
  Missing-return, unreachable-code, and `let ... else` diagnostics no longer
  disappear behind blanket exception suppression when a body contains an
  independent binding error.
- Function declarations now have explicit isolation coverage proving that body
  errors retain the declared signature, stay confined to the broken
  declaration, and do not prevent valid sibling resolution after workspace
  edits. `if` expression binding no longer changes scope by silently falling
  back to the enclosing binder when branch-binder construction fails.
- Target-typed expression binding now keys semantic cache entries by target type
  for every expression form, including wrappers such as parenthesized
  expressions, instead of relying on a syntax-kind allowlist. Semantic results
  are stable regardless of which target-type context binds the syntax first.
- Open generic method groups now infer and construct candidates from target
  delegate parameter types. Their constructed signatures participate in outer
  generic inference, delegate conversion, and semantic symbol publication, so
  calls such as `Apply(21, Identity)` resolve both methods consistently.
- Nullable parameter types now retain their underlying type during namespace
  function signature declaration. Distinct overloads such as `string?` and
  `object?` are no longer misdiagnosed as duplicates, while null-literal calls
  consistently select the more specific reference overload.
- Ambiguous invocations now publish `CandidateReason.Ambiguous` and the complete
  candidate set through `SemanticModel.GetSymbolInfo`. Opportunistic invocation
  lookup falls back to authoritative binding when it cannot select a method.
- Failed invocation binding now retains every considered method candidate, so
  `GetSymbolInfo` exposes useful candidates with
  `CandidateReason.OverloadResolutionFailure`.
- Incremental match exhaustiveness now has add-and-restore coverage for source
  enums, Raven union cases, and sealed-hierarchy permitted subtypes, including
  agreement between diagnostics and `GetMatchExhaustiveness` across snapshots.
- Ordinary `while` bodies now inherit nullability facts established by their
  condition, and cold semantic queries bind the enclosing loop so public flow
  information agrees with diagnostics-first binding.
- Normal exits from `while` loops now project the condition's false-state
  nullability facts when no `break` or outward `goto` can bypass the condition.
- Nested-loop `break` statements no longer suppress nullability facts inferred
  from normal exit of an enclosing `while`; only exits targeting that loop (or
  conservatively unresolved labeled/goto exits) block the inference.
- Workspace edits that change a `while` null check now invalidate and restore
  body flow state together with possible-null diagnostics.
- Successful non-null typed patterns now narrow their nullable scrutinee inside
  both `if ... is` and `while let` bodies, including cold semantic queries.
- Negated typed patterns now invert the same nullability fact, so a guard that
  exits on `is not T` narrows the scrutinee on the continuing path without
  regressing ordinary `is not null` flow.
- Successful property patterns, including the empty `{ }` non-null pattern,
  now narrow nullable scrutinees in public flow information and diagnostics.
- Successful nominal deconstruction patterns now likewise publish a non-null
  scrutinee within their true branch.
- Pattern mismatch now preserves a previously established non-null fact unless
  the pattern proves the scrutinee null, keeping cold and diagnostics-first
  semantic queries consistent across nested guards.
- Conjunctive patterns now combine operand nullability guarantees, so a
  successful `not null and ...` pattern narrows its scrutinee.
- Disjunctive patterns now narrow their success path only when every alternative
  requires non-null, and combine failure-path guarantees conservatively.
- Sequence patterns now accept nullable arrays and strings, fail normally for
  `null`, and publish a non-null scrutinee on successful matches.
- Dictionary patterns now accept nullable dictionary inputs with the same
  null-fail and successful non-null flow semantics.
- Incomplete constructor declarations now recover with a missing block and a
  targeted `RAV1028` diagnostic instead of throwing or silently accepting a
  bodyless `init`. Recovery preserves following type members, and parser
  mutation coverage now includes contemporary constructor and macro syntax.
  Type-only declaration patterns now use their optional designation directly
  instead of manufacturing a designation containing `None` tokens.
- Incremental syntax updates now retain fragment parser diagnostics, discard
  stale diagnostics from replaced syntax, and shift unaffected diagnostic spans
  after edits. Green-node replacement also preserves unchanged sibling
  identities instead of rebuilding the entire tree. Incremental parser tests
  compare exact syntax shape and diagnostics with an authoritative full parse,
  including incomplete macro functions and repair edits.
- Raven's existing nullability and control-flow actions are usable through the
  language server again: structured diagnostic arguments now survive the LSP
  round trip, strict-null guidance is registered by default, nullable-to-Option
  rewrites generate canonical `let` bindings, and if/else-to-match refactorings
  preserve exact fallback semantics with `_` instead of guessing complementary
  cases from names. The VS Code lifecycle log now records code-action requests
  and returned action counts for future editor-side diagnosis.
- Match exhaustiveness quick fixes now add all missing arms in one
  deduplicated action for match statements and expressions, including matches
  authored inside macro functions. Generated patterns follow the scrutinee:
  typed bindings for sealed classes and parenthesized unions, positional
  bindings for sealed records, target-typed cases for case-declared unions and
  enums, and literal patterns for finite literal cases.
- Macro-function bodies now participate in primary semantic diagnostics like
  ordinary function bodies. Invalid local macros report against authored source
  immediately instead of waiting for the projected macro assembly, and project
  diagnostics no longer replace those errors with generated source positions.
  A broken macro function no longer prevents valid sibling macros from compiling
  and expanding, and attached macro targets are modeled as implicit parameters
  for normal lookup and semantic tooling. Incremental recovery now preserves
  incomplete macro bodies, while language-service analysis retains the complete
  authored macro compilation even when emission filters out a broken macro.
  Document-scoped compiler diagnostics now unify the consumer and macro
  projections of an authored file, so VS Code publishes macro-body diagnostics
  during edits. Invocations still resolve to a recognized local macro
  declaration when its implementation is broken, avoiding misleading
  unresolved-macro cascades, while hover remains available throughout macro
  parameters and bodies.
- RavenDoc now omits redundant `public` modifiers, hides compiler-emitted
  extension grouping types and implementation-only accessors, preserves
  protected accessor contracts, and renders operator signatures without a
  duplicated `func` keyword.
- RavenDoc now groups case-declared union cases under their declaring union
  using logical Raven names and signatures, while keeping parenthesized
  member-type unions distinct and suppressing separate emitted case-type pages.
- Published Raven.Core API pages are now generated from the Raven project
  rather than its metadata assembly, restoring GitHub source links with line
  anchors. Workspace project loading also honors `RavenEmitCoreTypesOnly` by
  disabling framework projections while Raven.Core itself is analyzed.
- Abstract syntax API families now carry generator-owned closed-hierarchy
  metadata. Raven imports permitted subtype lists from referenced assemblies,
  so matches over the `SyntaxNode` root, structured trivia, expressions,
  statements, patterns, names, types, members, and other intermediate syntax
  categories receive ordinary exhaustiveness diagnostics and missing-case
  feedback, including recovery-only syntax nodes.

- Unit-returning callables now report `RAV9034` when their final expression
  produces a non-unit value, including effectful invocations. This prevents a
  discarded value from looking like a valid tail result; `_ = expression`
  remains the explicit intentional-discard form. The analyzer diagnostic can
  be disabled through standard `.editorconfig` severity configuration.
  Consumed block-expression and value-returning lambda tails remain valid
  implicit results and are not reported.
- Consolidated full returned-value handling into `RAV9034`, removing the
  overlapping `RAV9029` diagnostic. Value-forming outer expressions such as
  `2 + Compute()` are now reported even when a nested call may have effects,
  while full mode extends the same diagnostic to bare calls and member access.
- Added a user-facing built-in analyzer reference with default severities and
  `.editorconfig` override guidance to the main documentation navigation.
- Added a Playground sample demonstrating Raven's support for functional
  programming patterns within its pragmatic, general-purpose model: immutable
  transformations, value-producing `match` and `if` expressions, block
  expressions, final-expression returns, and an effectful output shell around
  a pure calculation core.
- RavenDoc now accepts repeatable `--value name=value` inputs and substitutes
  explicit `{{name}}` placeholders in Markdown, enabling publishing workflows
  to inject paths, versions, commit identifiers, and version stamps while
  leaving unresolved placeholders visible.
- RavenDoc now separates its reusable page template and static assets from
  symbol extraction, renders compact API headings, editor-like signatures with
  generic constraints, distinct namespace/member icons, structured
  documentation sections, and a responsive page outline.
- Updated tests, samples, and language-facing documentation to use canonical
  `let` lexical bindings while retaining `val` for immutable properties and
  compatibility coverage. Compiler API, analyzer, source-generator, and macro
  examples now prefer Raven where the API is being consumed from Raven, and
  contributor guidance defines a gradual Raven-first infrastructure and
  bootstrap boundary.
- Added `Raven.Macros.Sha256Digest!` with the imported `sha256Digest!` alias.
  It hashes literal values during compilation and expands to a lowercase
  hexadecimal string, avoiding runtime hashing and naming collisions with
  .NET's `SHA256` type.
- Added `Raven.Macros.EmbedFileContent!` with the imported
  `embedFileContent!` alias. It resolves paths relative to the invoking source
  file, embeds UTF-8 text as a string literal, diagnoses missing files, and
  invalidates cached expansions when observed files change or disappear.
- Macro function declarations now appear in VS Code's document outline with a
  distinct operator symbol, including local functions nested in their bodies.
- The documentation build now publishes independent RavenDoc API sites for
  `Raven.Core` and `Raven.Macros` alongside the DocFX language site and browser
  Playground. Library documentation is prominent in the main navigation,
  compiler APIs are supporting tooling reference, and macro pages link to the
  compiler syntax-tree guide.
- RavenDoc now renders namespace functions and `macro func` declarations on
  namespace pages. Namespace-function pages preserve the Raven-facing shape
  while identifying the emitted CLR container for consumers in other .NET
  languages.
- Moved the standard `quote` and `compile` declarations into the Raven-authored
  `Raven.Macros` compiler-plugin assembly. Its aliases require
  `import Raven.Macros.*`, while canonical qualified names remain available.
- Raven compiler-plugin projects marked with `RavenCompilerPlugin` can now emit
  reusable `macro func` declarations, and compiler/MSBuild runtime dependency
  propagation follows actual emitted assembly references instead of scanning
  source for specific macro names.
- Fixed a language-server hover regression that could recursively materialize
  metadata symbols while resolving qualified names. Metadata types now use a
  cached simple-name index, qualified namespace segments resolve from available
  semantic state, and cold consumer-local hovers recover correctly in files
  partitioned for local macro functions.
- VS Code and the language server now treat a source file outside evaluated
  `.rvnproj` items as an isolated file-based application. Loose files no longer
  leak declarations into one another or nearby projects, standalone snapshots
  receive the standard prelude, and Run Active File/Project invokes `rvn run`
  in an interactive terminal.
- Added first-class file-based applications through `rvn run <file.rvn>` and
  the `rvn <file.rvn>` shorthand. Arguments after `--` reach `Main`, process
  exit codes are preserved, and one-shot compilation artifacts are isolated
  from the source tree and cleaned after execution. A first-line
  `#!/usr/bin/env rvn` shebang is preserved as trivia, enabling executable
  Raven files on Unix-like systems.
- Macro completion items now carry `IMacroSymbol` and map to VS Code's distinct
  snippet icon instead of appearing as ordinary classes or untyped text.
- Added compiler symbols for loaded and intrinsic macros. `nameof` now accepts
  macro names and preserves the resolved alias or canonical spelling, while
  `typeof` reports a dedicated diagnostic when its operand resolves to a macro.
- Added namespace-qualified macro invocation and import-scoped macro aliases.
  Compiler-provided `Raven.Macros.Quote` and `Raven.Macros.Compile` expose the
  `quote` and `compile` aliases through `import Raven.Macros.*`; local names can
  shadow imported aliases, while qualified invocation remains an escape hatch.
  Argument-style bang invocations such as `twice!(21)` no longer require an
  empty token-tree body, and freestanding macro samples now use this preferred
  form.
- Fixed expanded-document commands for projects containing authored
  `macro func` declarations. `rvn dev syntax --syntax-view expanded` and
  `rvn dev macros` now resolve the workspace document's projected compilation
  tree, expand every sibling macro invocation, and preserve line breaks after
  multiline token-tree invocations.
- Fixed language-server hover inside authored `macro func` declarations.
  Their signature semantic model now provides method-like body scopes, so
  parameter declarations, locals, references, member access, and invocations
  resolve without consulting the lowered macro implementation tree.
- Made authored `macro func` declarations first-class incremental executable
  owners. Signature and body edits now invalidate their semantic state without
  discarding unrelated state, and language-service queries recover through
  malformed intermediate edits instead of reusing stale parameters or locals.
- Prevented authored macro-function parameters and attached-target names from
  colliding with compiler-generated adapter locals during local macro lowering.
- Updated the Playground metaprogramming sample to use native macro functions
  with real `ExpressionSyntax` and `IMacroTokenStream` parameters, and made
  local macro partition emission work entirely in memory under WebAssembly.
- Added type-directed `ExpressionSyntax` parameters to macro functions. They project
  authored invocation arguments as `ExpressionSyntax` and can be mixed with
  ordinary typed values, while semantic symbols and runtime parameter
  descriptors expose the shared `MacroParameterRole`.
- Added native token-stream inputs to macro functions. An
  `body: IMacroTokenStream` parameter selects token-tree invocation syntax and
  binds the raw body, while the remaining parameters continue
  to use the typed caller-supplied argument model.
- Made `macro func` declarations executable as same-compilation argument-style
  and attached macros. Attached declarations use contextual `on Property` or
  `on property: Property` target clauses, and ordinary synchronous bodies can
  conditionally combine `expand`, `replace`, and `introduce` contribution
  statements. The compiler lowers them to isolated provider adapters and typed
  parameter objects while preserving `IMacroFunctionSymbol` as their semantic
  identity.
- Made the Playground own and serve its theme stylesheet directly so local
  development and standalone subpath deployments use the same asset URL.
- Reduced the Playground header to a compact Raven mark beside its title so the
  complete desktop workspace fits without page-level scrolling on typical
  laptop viewports.
- Replaced the Playground's native example selector with a themed, searchable
  picker that organizes samples into a Basics section and feature-focused
  groups.
- Added expression-form pattern binding with
  `if let pattern = value { ... } else { ... }`. It uses the same pattern and
  capture semantics as the existing statement form, scopes captures to the
  successful branch, and computes its result from the two branch values.
- Added compiler-integrated conditional compilation with `#if`, `#elif`,
  `#else`, and `#endif`. Conditions support defined symbols, `true`/`false`,
  parentheses, and Raven `not`/`and`/`or` operators (with
  `!`/`&&`/`||` aliases). Symbols flow from MSBuild `DefineConstants` or the
  `rvnc --define` option, inactive source remains lossless disabled trivia, and
  the VS Code extension highlights directives and dims inactive code.
- Added a Roslyn-inspired syntax tree visualizer to the VS Code extension. The
  Explorer view presents compiler-produced nodes, tokens, trivia, property
  roles, spans, raw kinds, missing elements, and diagnostics; supports authored
  and fully macro-expanded trees; and opens the complete expanded source when
  switching to the expanded tree.
  `rvn dev syntax` now exposes the structured JSON and source-override contract
  used by the view.
- Namespace-level types, delegates, unions, and extension declarations now
  default to `internal`; `public` explicitly exports them from the assembly and
  is no longer diagnosed as redundant in that position. Type members,
  including nested types, default to `public`, while Raven.Core now marks its
  exported declarations explicitly. The former `MembersPublicByDefault`
  compilation/project option and its CLI switches have been removed.
- `use` bindings no longer report `RAV9027` merely because their bound value is
  not read; establishing the disposal lifetime counts as the declaration's
  intended use.
- Added raw-body token-tree expression macros with `#name { ... }` syntax,
  lossless DSL body capture, body-relative diagnostics, and helpers for parsing
  the complete body or selected embedded spans as Raven expressions. Macro
  bodies bypass ordinary Raven tokenization, while expansion continues through
  normal semantic binding and emit.
- Added the alternate `name! { ... }` spelling for token-tree expression
  macros. It has a dedicated syntax node while sharing macro binding,
  expansion, completion, and language-service behavior with `#name { ... }`.
- Added `SyntaxToken.RawKind`, macro-local token reclassification through
  `WithRawKind`, and detached custom-token construction without changing
  ordinary Raven `SyntaxKind` classification or lexing.
- Added replaceable macro token streams that emit `SyntaxToken`, including a
  default Raven-lexer-backed stream, macro-local keyword/reserved-word overlays,
  and compiler-discovered custom stream providers for DSL-specific lexers.
- Added typed token-tree macro inputs through
  `ITokenTreeExpressionMacro<TParameters>`, allowing validated positional or
  named arguments before an unrestricted raw DSL body.
- Added compiler-owned typed macro parameter descriptors and named-argument
  completion for attached, argument-style, and token-tree macros.
- Added context-aware macro-name completion for incomplete invocations.
  Typing `#` in an expression offers only freestanding and token-tree macros;
  typing it in a declaration offers only attached macros and inserts the
  complete `#[Macro]` attribute form.
- Added a Raven-authored `#guard { unless <expression> }` sample as the
  token-tree macro MVP, demonstrating macro-local keywords, embedded Raven
  expression parsing, direct lowering, and end-to-end execution.
- Extended the token-tree macro sample with
  `#choose { test ... then ... otherwise ... }`, demonstrating multiple
  macro-local clauses, independently parsed Raven fragments, body-mapped
  missing-clause diagnostics, and direct lowering to an `if` expression.
- Added a minimal LINQ-like `#query` macro sample with one `from`, optional
  `where`, and one `select` clause, directly lowering caller-scoped Raven
  fragments to ordinary `Where`/`Select` calls and authored range-variable
  lambdas.
- Added diagnostic-bearing embedded Raven expression parsing for token-tree
  macros. `ParseExpressionResult` returns recovered syntax plus immutable
  native parser diagnostics mapped to the authored invocation, while the
  existing `ParseExpression` convenience API remains available.
- Added complete-body and selected-span Raven statement parsing for token-tree
  macros. `ParseStatement` returns recovered `StatementSyntax`, while
  `ParseStatementResult` also retains native authored-source diagnostics and
  rejects trailing input.
- Added compiler-owned macro signature help for typed attached, freestanding,
  and token-tree invocations. The semantic model now exposes normalized macro
  parameters and the active argument, and the language server presents that
  result including token-tree body shape.
- Added the initial `macro func` declaration boundary at compilation-unit and
  namespace-member scope. It uses a dedicated
  `MacroFunctionDeclarationSyntax`, treats `macro` contextually, and exposes a
  distinct `IMacroFunctionSymbol` with macro-owned parameters, generic
  parameters, constraints, and call-site return type. Macro functions do not
  implement `IMethodSymbol`, enter ordinary runtime method binding, or support
  `async`/`await`; semantic activation and lowering remain future work.
- Added the compiler-owned expression-only `#quote { ... }` intrinsic. It
  preserves tokens and trivia, rejects malformed or trailing input at authored
  locations, expands to fully qualified `SyntaxFactory` construction, and
  participates in macro-name completion without a plugin reference.
- Added `#(expression)` holes inside expression quotes. Holes are discovered
  through the macro token stream without changing Raven lexing, accept ordinary
  Raven expressions that bind as `ExpressionSyntax`, preserve surrounding
  quote trivia, and retain native diagnostics at authored locations.
- Added `compile<TDelegate>! { expression }`, which applies the `quote!` syntax
  and hole model, compiles the resulting Raven expression at runtime, and
  returns a strongly typed delegate. Compiler and SDK builds now add
  `Raven.CodeAnalysis` and runtime compiler dependencies on demand for the
  intrinsic while respecting an explicit project reference.
- Migrated the Raven-authored sample `#add` procedural macro to construct its
  expansion with `#quote` and argument-expression holes, validating quote while
  compiling a macro plugin and loading that plugin in a consuming project.
- Centralized compiler-provided macro registration in the compiler's default
  macro environment and added `MacroReference.CreateFromImage`, allowing emitted
  Raven macro plugins to be activated directly from memory as a foundation for
  same-project macros and the Playground.
- Added an explicit compile-time-only macro source partition to `Compilation`.
  `AddMacroSyntaxTrees` compiles Raven macro declarations in memory before
  consumer binding, reports partition diagnostics through the consumer
  compilation, includes local macros in completion, and excludes plugin
  implementation types from runtime emit.
- Added automatic direct-declaration discovery for same-project macros. Macro
  interface implementations move into the compile-time partition through
  compiler, Workspace, and SDK compilation paths, while retaining
  semantic-model access and requiring neither a `RavenMacro` item nor an
  explicit compiler-contract project reference.
- Retired the transitional consumer-authored `RavenMacro` project item.
  Reusable compiler-plugin providers now use ordinary marked project,
  assembly, or package references; project loaders report migration guidance
  when the removed item is encountered.
- Removed the transitional `IRavenMacroPlugin` aggregation contract and
  `[LocalMacroPlugin]` source marker. Macros are now registered directly as
  `IMacroDefinition` implementations.
- Made macro category classification compiler-owned. Definitions implement
  exactly one category-specific macro interface, and `MacroFacts` derives
  `MacroKind` without a macro-overridable discriminator property.
- Moved macro target applicability to `IAttachedDeclarationMacro`, removing
  redundant `MacroTarget.None` implementations from freestanding and
  token-tree macros while retaining normalized queries through `MacroFacts`.
- Added focused sample projects for custom macro token streams and quote-based
  macro expansion.
- Applied nominal-type macro replacements to base/interface binding, allowing
  an attached macro to add a real interface contract alongside generated
  members.
- Migrated the remaining C# macro sample providers to Raven-authored macro
  projects.
- Renamed project sample files around their entry point, program, primary type,
  or related type group instead of using `main.rvn` universally.
- Changed `MacroReference` to expose a cached immutable `Macros` snapshot so
  compiler and tooling queries reuse the same definition instances.
- Kept collectible macro assembly contexts alive for the lifetime of their
  cached macro snapshots, preventing referenced helper assemblies from failing
  to load when collection occurs before expansion.
- Made VS Code language-server builds on extension activation opt-in. The
  extension now starts an existing workspace or packaged server immediately by
  default instead of blocking activation on a full compiler dependency build.
- Added declaration-granular same-project macros through `[LocalMacro]`.
  Marked top-level declarations are compiled and activated separately while
  ordinary declarations in the same source remain runtime code, enabling macros
  to be declared and consumed in one Playground buffer.
- Reused emitted same-project macro partition artifacts across consumer-only
  incremental edits, while invalidating them for macro or reference changes and
  remapping cached partition diagnostics to the current syntax-tree projection.
- Added `RAVM003` for local macro implementations that depend on consumer
  declarations, identifying the compile-time activation cycle at the authored
  reference in both dedicated and mixed-source macro layouts.
- Added position-aware semantic-model lookup for mixed local-macro documents.
  `Compilation.GetSemanticModel(tree, position)` and
  `Document.GetSemanticModelAsync(position)` now route macro declaration
  positions to the current macro projection while preserving the consumer model
  for ordinary source positions and existing positionless calls.
- Routed language-server hover and completion through the position-aware
  semantic projection, enabling ordinary Raven symbol information and member
  completion inside same-buffer local macro implementations.
- Routed language-server definition, references, and rename through the
  position-aware semantic projection. Reference search now scans both
  compiler-owned projections of a mixed document while returning edits and
  locations against the original authored source.
- Made workspace analyzer execution projection-aware for mixed local-macro
  documents. Syntax-node, symbol, operation, and syntax-tree actions now see
  both ordinary consumer code and Raven macro implementation code with the
  semantic model that owns each projection.
- Added `FreestandingMacroExpansionResult` factory methods for expression
  results, forwarded parser diagnostics, macro-authored diagnostics, and
  combined diagnostic results. The built-in `#quote` macro and Playground
  local-macro example now use the factory path.
- Added matching `MacroExpansionResult` factories for attached declaration
  replacement, introduced members, peer declarations, and diagnostic-only
  results. Mutable result properties remain available for compatibility.
- Stabilized attached property replacement binding so declaration-pass
  accessor skeletons are completed once and later binds reuse the registered
  accessor symbols. Replacement properties now expose the same getter and
  setter identities through both the property and containing type.
- Improved typed macro failure diagnostics by unwrapping reflection invocation
  failures. Attached and freestanding macro authors now see their underlying
  exception message at the authored macro name instead of a generic reflection
  wrapper message.
- Made attached and freestanding macro expansion cancellation-aware. Direct
  and reflection-wrapped cancellation now propagates to the compiler caller,
  does not produce `RAVM020`, and does not cache a failed expansion, allowing a
  later uncanceled request to retry normally.
- Added the provider-owned `[assembly: RavenCompilerPlugin]` marker for
  reusable Raven macro projects. Consumers can now use an ordinary
  `ProjectReference`; the workspace builds and activates marked providers as
  compiler plugins without adding them as runtime project references or
  scanning unmarked dependencies.
- Added deterministic macro export manifests through repeatable
  `[assembly: RavenCompilerPlugin(typeof(MacroType))]` markers. File, assembly,
  and in-memory macro references now select direct macro definitions, retain
  bare-marker fallback discovery, and report invalid manifests as `RAVM001`.
  Same-project macro partitions discover direct definitions without an
  assembly export marker.
- Added provider-marked C# compiler-plugin project references. Raven projects
  can now consume a C# macro provider through an ordinary `ProjectReference`;
  the project system builds and activates marked providers without adding them
  to the consumer's runtime reference graph or scanning unmarked dependencies.
- Added compiler-owned discovery of marked portable assembly references.
  Direct DLL and resolved package references now join the same active macro
  registry as explicit and same-project macros after a metadata-only marker
  check; unmarked assemblies are never activated or searched for macro types.
- Added split NuGet package support for compiler plugins. Consumer binding now
  retains a package's `ref/<tfm>` assembly while a marked `lib/<tfm>`
  implementation is activated separately as a macro reference. Macro helper
  assemblies shipped beside the implementation are resolved without requiring
  an application `.deps.json`; runtime assets supplied by transitive NuGet
  packages are carried as private identity-checked macro dependency probes
  rather than consumer metadata references.
- Deferred assembly custom-attribute emission until source type builders and
  members exist, allowing assembly attributes such as macro manifests to carry
  `typeof` values that refer to types declared in the same Raven assembly.
- Added runnable Playground examples for constructing syntax with `#quote` and
  for defining local attached, argument-style expression, and token-tree
  expression macros.
- Prevented incomplete recovered source symbols from aborting external
  documentation emission when no stable documentation member ID can be built.
- Changed `Compilation.AddReferences` to append metadata references, matching
  its Roslyn-style additive contract instead of replacing existing references.
- Clarified that Raven-authored enums are closed by default for declared-member
  match exhaustiveness, with no source modifier, and locked complete enum
  matches with focused semantic coverage.
- Documented the typestate pattern with phantom marker types, a state-erased
  base class, and state-specific extensions, and added a runnable Playground
  connection-lifecycle example.
- Changed `RavenQuoter` to emit Raven `SyntaxFactory` construction code by
  default, with explicit C# output available through `RavenQuoterOptions`.
- Added a Raven-themed DocFX site with a dedicated carousel of language samples,
  compact documentation navigation, VS Code-style Raven syntax highlighting,
  and shared light/dark design tokens used by RavenDoc and the Playground.
- Added compiler-owned Markdown classification for Raven documentation
  comments and dedicated language-server semantic tokens for tags, headings,
  links, inline code, and fenced code. Tag-like text in code remains literal.
- Refreshed the `markdown-docs` library/consumer sample to demonstrate default
  dual output, Raven-native role aliases, links, headings, inline code, and
  fenced Raven examples without redundant project configuration.
- Added the format-neutral `RavenDocumentation` compiler API. Markdown and XML
  inputs now normalize into Raven-owned section and association roles before
  XML projection, with a small compatibility alias set including `@parameter`,
  `@result`, and `@throws`.
- Raven library projects now emit both Raven Markdown documentation sidecars
  and compatible .NET XML documentation by default. Raven consumes Markdown
  first and falls back to XML for libraries without Raven documentation;
  Markdown remains the default source comment format and XML authoring remains
  explicit.
- `Raven.Core` now emits and ships `Raven.Core.xml`, with documentation for its
  public types, carrier members, LINQ helpers, JSON converters, and framework
  projection adapters. Projected framework methods forward their adapter
  documentation so hover can present the Raven-facing `Option`/`Result`
  behavior.
- XML documentation emission now handles source-field metadata names safely,
  and MSBuild-relative default documentation paths resolve without duplicating
  the intermediate output directory.

- Updated the ASP.NET Core samples for .NET 11 Preview 6 union request,
  response, streaming, JSON persistence, and OpenAPI `anyOf` support.

- Restrict .NET 11 runtime-async method metadata to Task-like methods so async
  iterators remain valid CLR types.

- Allow multiline fluent expressions in typed `let ... else` declarations,
  including unparenthesized awaited invocations.

- Added Roslyn-shaped source generator APIs, dedicated workspace generator
  references, and `.rvnproj` `<Analyzer>` / `<SourceGenerator>` assembly items
  that run extensions in normal project builds.

- Reorganized spans, stack allocation, ref structs, ref safety, and unsafe
  interop into a dedicated systems-programming documentation section so these
  specialized features no longer dominate the core language path.

- Added first-class `Span<T>` and `ReadOnlySpan<T>` support across stack
  allocation, covariant conversions, generic inference, overload resolution,
  indexing, mutation, slicing, iteration, and span-targeted collection
  expressions, with `Memory<T>` and `ReadOnlyMemory<T>` interoperability.
- Added unsafe pointer-producing stack allocation with
  `stackalloc T[count]`, including runtime-sized allocations, unmanaged element
  validation, integer-count diagnostics, direct `localloc` emission, and safe
  natural `Span<T>` or explicit `ReadOnlySpan<T>` targets.
- Rejected returning `stackalloc` storage through direct expressions, locals,
  and simple pointer or span aliases while preserving returns of spans backed
  by parameters or managed arrays.
- Added `ref struct` declaration syntax and source-symbol classification,
  including modifier validation and consistency checks across partial
  declarations, and emitted the standard `IsByRefLikeAttribute` metadata for
  both generic and non-generic ref structs.
- Added `readonly ref struct` classification and `IsReadOnlyAttribute`
  emission, with diagnostics for mutable instance storage and inconsistent
  partial declarations.
- Added ref fields with `&T` field types inside ref structs, including semantic
  restrictions, symbol API classification, and standard CLR `BYREF` field
  signatures.
- Rejected returning ref structs that contain references to method locals or
  `stackalloc`-backed ref-like fields, including through simple local aliases,
  while allowing caller-owned references and spans supplied by parameters.
- Added the `allows ref struct` generic anti-constraint, including source
  semantic classification and the standard CLI `AllowByRefLike` metadata flag.
- Applied ref-like storage, capture, async, and iterator safety rules to type
  parameters declared with `allows ref struct`, not only to concrete ref-like
  named types.
- Allowed managed-reference dereferences in safe code while retaining unsafe
  diagnostics for raw pointer dereferences.
- Rejected ref fields whose referent is itself ref-like or is a generic type
  parameter that allows ref structs.
- Diagnosed misplaced, duplicated, and `class`-conflicting
  `allows ref struct` anti-constraints in both inline and `where` constraint
  lists.
- Recognized `ScopedRefAttribute` on consumed .NET parameters and exposed the
  result through the Roslyn-like `IParameterSymbol.ScopedKind` API, including
  constructed generic symbols.
- Added `scoped` parameter syntax and source-symbol classification for both
  scoped ref-like values and by-reference parameters.
- Emitted `ScopedRefAttribute` for explicitly scoped parameters when required
  by the C# metadata contract, including generic-safe metadata round trips.
- Rejected returning scoped ref-like parameters through direct expressions or
  local aliases.
- Applied C#-compatible implicit scoped defaults to `out` parameters and `ref`
  parameters of ref-like type, including metadata classification and emission.
- Highlighted contextual `scoped` parameter modifiers in both semantic tokens
  and the VS Code TextMate grammar without reserving identifier uses.
- Added Raven-native `scoped val`/`var`/`let`/`const` local syntax, scoped-value
  versus scoped-reference symbol classification, and editor highlighting.
- Diagnosed `scoped` local declarations whose resulting type is neither
  ref-like nor by-reference.
- Diagnosed by-value `scoped` parameters of non-ref-like type while permitting
  ordinary types behind `scoped ref`, `in`, and `out`.
- Rejected returning scoped ref-like locals directly or through ordinary local
  aliases.
- Propagated scoped-local escape provenance through ref-like field containment.
- Propagated scoped provenance into ref-like call results through receivers and
  unscoped parameters, while excluding arguments to scoped parameters.
- Rejected capturing scoped parameters and locals in lambdas or local
  functions, including scoped references to ordinary value types.
- Rejected scoped parameters and locals that would remain live across `await`
  or `yield` suspension points.
- Prevented overrides and explicit interface implementations from weakening a
  scoped parameter contract while allowing implementations to strengthen it.
- Required partial method declarations and implementations to agree on each
  parameter's scoped contract.
- Preserved scoped parameter attributes on emitted delegate `Invoke` methods,
  including generic delegates that allow ref-like type arguments.
- Rejected assignments that expose scoped values through by-reference
  parameters or fields of `self` and by-reference receivers.
- Enforced scoped indexer parameter contracts across overrides and explicit
  interface implementations.
- Correctly materialized value-type `self` when Raven methods request its value
  while preserving the managed receiver for address-based access, keeping
  generic `Option` and `Result` instance behavior portable across runtimes.
- Materialized empty union cases through their enclosing carrier during `?`
  propagation, preventing `None` from being returned with the incompatible
  case-only runtime layout.
- Computed async resume dispatch from the fully lowered protected-region tree
  and entered nested guards at their boundaries, preventing branches into
  `try` regions for `try? await` and Result propagation.
- Lowered non-`use` async bodies before suspension rewriting so a `try await`
  expression consumed by a match cannot acquire an unguarded protected region.
- Emitted sequential storage for unions containing managed references while
  retaining compact explicit storage for unmanaged-only unions, preventing
  invalid overlapping object/value fields across runtimes.
- Projected `DateTimeOffset`, `DateOnly`, `TimeOnly`, and `TimeSpan`
  `TryParse(string)` methods as `Option<T>` values.
- Added `lock expression { ... }` statements, lowering to exception-safe
  `System.Threading.Monitor` acquisition and release.
- Added playground samples showing guarded deconstruction in `for` iteration
  and pattern-bound `while let` consumption of a domain event stream.
- Added a contextual playground sample for mixed-era shipment references using
  a type union with target-typed construction.
- Prevented async lowering from redirecting synthesized state-machine receivers
  through their own hoisted receiver field, producing portable async-iterator IL.
- Added a cold-chain monitoring playground sample using `yield` and `await for`
  to consume an asynchronous stream.
- Added a webhook-routing playground sample that jointly matches dictionary
  metadata and sequence-shaped request paths.
- Recovered statically bound sequence types when matching nested tuple elements,
  so sequence patterns compose with dictionary and other structural patterns.
- Made cold metadata member lookup include explicit-interface properties and
  enabled middle-rest sequence deconstruction over `ImmutableArray<T>`.
- Kept parenthesized tuple patterns on tuple code generation when their runtime
  types also expose indexable interface members, preserving composed structural
  patterns across desktop and WebAssembly runtimes.
- Added a playground sample that scopes `HttpClient` with `use`, loads the
  deployed example catalog, deserializes its JSON, and models outcomes as a
  union.
- Preserved collection element types while resolving competing overloads so
  generic enumerable overloads such as `Task.WhenAll([task1, task2])` infer
  their type arguments instead of prematurely widening the elements.
- Made the WebAssembly playground await synthesized async top-level entry
  points directly instead of invoking their synchronously blocking console
  bridge.
- Added a checkout playground sample that starts independent warehouse stock
  lookups together and awaits their results before presenting availability.
- Added an order-boundary playground sample showing `Result` conditional
  access and implicit error conversion during propagation.
- Added a price-import playground sample that captures exceptions from a
  throwing .NET API as typed results with `try?`.
- Made `Result` propagation extract its error union case structurally instead
  of depending on `UnwrapError`, preserving failure behavior across runtimes.
- Added a dispatch-planning playground sample built with immutable collection
  comprehensions, filtering, and collection spreads.
- Added a contextual playground sample that models fulfillment routes as a
  sealed class hierarchy with shared behavior, property patterns, and
  exhaustive matching.
- Emitted closed-hierarchy metadata without runtime constructor inspection, so
  sealed class hierarchies compile in WebAssembly.
- Prevented Monaco's automatic layout from repeatedly increasing the playground
  workspace height by giving the desktop workspace a bounded viewport-relative
  height and sizing the editor through flex layout.
- Made the playground load Hello World deterministically on startup and added
  shareable source URLs through a base64url `source` query parameter and Share
  command.
- Expanded the playground catalog with contextual examples of `Option` and
  `let ... else`, framework parsing projected into typed flow, higher-order
  functions, and `Result` propagation.
- Resolved the playground's embedded .NET reference assemblies from MSBuild's
  selected targeting pack instead of assuming its patch version matched the
  browser runtime pack. Static builds now fail early if `System.Runtime` is
  absent, and browser coverage executes every registered example.
- Made PE attribute discovery tolerate missing transitive metadata dependencies,
  preventing wildcard namespace imports from crashing browser compilation when
  a nonessential attribute assembly cannot be resolved.
- Emitted nullable metadata through raw custom-attribute blobs instead of
  runtime constructor inspection, allowing user-defined unions and other
  nullable shapes to compile under browser WebAssembly.
- Made repository-local Raven MSBuild targets honor the active build
  configuration when locating the compiler host and Raven.Core, so clean
  Release builds no longer incorrectly require Debug artifacts.
- Published the browser playground beneath `/playground/` in the same GitHub
  Pages artifact as the documentation site, with top-level documentation links
  and a relocatable static base path.
- Added a playground-owned example catalog loaded from static files, allowing
  curated Raven programs to be registered and updated independently of the
  browser-hosted compiler code. Browser coverage compiles every registered
  example.
- Made playground completion visible and responsive by debouncing member-prefix
  requests, explicitly opening Monaco suggestions after a short pause, and
  avoiding uncancelable WebAssembly completion work on ordinary keystrokes.
  Global completion remains available through `Ctrl+Space`.
- Prevented equivalent synthesized methods from producing duplicate CLR method
  definitions during emission. Raven.Core union carriers such as `Result<T, E>`
  now emit a single executable `ToString` body instead of a bodyless method
  that could fail at runtime with `BadImageFormatException`.
- Aligned the WebAssembly playground's compilation environment with `rvnc` by
  sharing the standard generated prelude, referencing Raven.Core, and compiling
  against the .NET reference assemblies. Record equality synthesis now accepts
  equivalent nullable and non-nullable metadata representations of
  `object.Equals`, preventing platform-specific missing-member diagnostics.
- Reused the WebAssembly playground's emitted assembly when Compile and Run
  target the same immutable compilation snapshot, avoiding redundant browser
  emission while preserving incremental recompilation after edits.
- Reduced one-file compiler latency by indexing extension-conversion containers
  directly from referenced assembly metadata instead of loading conversion
  members across every referenced type.
- Added a static-hostable Blazor WebAssembly playground starter with a Monaco
  editor, Raven TextMate highlighting, separate Compile and Run commands, and
  in-browser compilation and execution of emitted Raven assemblies. A
  repository-owned browser smoke test covers the release-published static site,
  compiler-backed Monaco completion, diagnostics, and emitted-program output.
  The browser host advances an ordinary Raven workspace and reuses its current
  compilation across editor requests and explicit compilation.
- Removed two browser-WebAssembly blockers from compiler metadata loading:
  assembly identities are read from portable executable metadata where runtime
  assembly-loading APIs are unavailable, and unavailable runtime nullability
  reflection falls back to explicit nullable metadata.
- Added dotted property paths in property patterns. For example,
  `Foo { Item.Size: 2 }` is shorthand for
  `Foo { Item: { Size: 2 } }`. Completion and hover resolve each property-path
  segment against its receiver type, including while a dotted path is being
  typed.
- Removed the experimental trailing-block call syntax and its builder/receiver
  DSL infrastructure from the main language. Function values use ordinary
  function-expression syntax.
- Restored brace object initializers as a distinct construct: `Foo { Name =
  "Foo" }` selects a parameterless constructor, while `Bar("Foo") { Age = 42 }`
  initializes an object after an explicit constructor call. `value with { ... }`
  remains the separate non-destructive copying form.
- Made incremental document diagnostics independent of prior semantic queries
  by declaring same-document member signatures before binding executable code.
- Added the initial distribution contract: platform SDK archive builders,
  relocatable compiler/MSBuild assets, `rvn sdk path`, installed-SDK discovery
  in VS Code, a universal VSIX builder with a bundled language server,
  checksum-verifying installers, and automated multi-platform release builds.
  Packaged `rvn`, `rvnc`, and VSIX artifacts now share the release version;
  both command-line tools expose it through `--version`.
- Added `rvn doctor` to diagnose the .NET SDK and required Raven SDK files.
- The VS Code extension now offers SDK installation instructions when build,
  run, and debug tooling is unavailable, while retaining bundled editor support.
- Locked the built-in union C# surface and serialization contract with direct
  C# construction/extraction coverage: payload-first JSON remains the standard
  behavior, while tagged Raven serialization requires explicit opt-in.
- Highlighted constructor-form `init(...)` declarations and primary-constructor
  access modifiers in the VS Code TextMate grammar.
- Preserved keyword highlighting for parenthesized patterns such as `if let
  (...)`, `while let (...)`, `for let (...)`, and `value is (...)` in both the
  VS Code TextMate grammar and the DocFX site highlighter.
- Removed `trait` as an alias for extension declarations; use `extension`.
- Added the first generic instance framework projection:
  `Dictionary<TKey, TValue>.TryGetValue(key) -> Option<TValue>`. Missing keys
  become `None`, while constructed value-type nullability is preserved.
- Projected `Guid.Parse(string)` as `Result<Guid, FormatException>` and
  `int.Parse(string)` as `Result<int, FormatException | OverflowException>`.
  Null-argument exceptions that require forcing null through the non-null Raven
  signatures now propagate as faults rather than ordinary result errors; the
  legacy lowercase `int.parse` Raven.Core helpers remain removed.
- Added default-on framework API projections for the simplest `TryParse`
  overloads on `int`, `long`, `double`, `decimal`, `Guid`, and `DateTime`.
  Raven presents these as `Option<T>`-returning methods; projects can set
  `RavenFrameworkProjections` to `None` to restore the ordinary CLR surface.
  The exact mappings and failure recipes live in a versioned compiler catalog;
  stable projection IDs bind each catalog entry to its attributed Raven.Core
  bridge without relying on extension-method precedence.
- Added projection-specific diagnostics for missing, duplicate, and
  structurally incompatible framework projection bridges.
- Validated built-in projection source and bridge methods against their full
  reflected CLR signatures, including generic arguments and ref-kinds.
- Presented receiver-specific projection overloads in signature help and
  receiver-owned Raven signatures in hover, without exposing CLR `out`
  overloads; loose-file language-server projects now preserve the workspace's
  configured target framework when resolving framework and Raven.Core metadata.
- Added the first same-signature `Parse -> Result` projection for
  `Int32.Parse(string)`, with explicit null, format, and overflow mappings.
- Added Rust-style `let pattern = expression else { ... }` declarations. The
  `else` branch must exit, and successful pattern bindings remain available in
  the surrounding scope. Documentation now promotes `if let` and `let ... else`
  for binding-oriented control flow while retaining `is` for boolean pattern
  expressions.
- Preserved target typing for ordinary typed `let` declarations after their
  unification with pattern-declaration syntax, including shorthand union cases
  in `if` expression branches.
- Recognized interface implementations inherited from metadata base classes,
  avoiding spurious missing-member diagnostics on derived Raven classes.
- Ordered language-server diagnostic presentation per document and rejected
  older editor versions, preventing recovery diagnostics from reappearing
  after a newer compiler pass has cleared them.
- Invalidated reused metadata load contexts when a portable reference is
  rebuilt at the same path, preventing editor semantic requests from repeatedly
  failing after project outputs such as `Raven.Core.dll` change.
- Added `RAV1026`, a warning for lists that inconsistently mix comma and newline
  separators. Union case and enum member lists now diagnose the mixed style
  while continuing to parse both forms.
- Finite union payload products now understand `not`, `and`, and `or`
  combinators when proving collective case coverage. Removed the superseded
  binder-owned exhaustiveness implementation so diagnostics and semantic
  queries cannot drift between separate checking paths.
- Missing-case diagnostics now identify uncovered alternatives inside wholly
  or partially unmatched finite union payloads, such as
  `Error(OverflowException)` and `Error(.ServiceUnavailable)`, instead of
  collapsing the payload coverage to `Error`.
- Exhaustiveness analysis now proves complete positional tuple matches when
  tuple elements form a bounded finite product of booleans, enums, nested
  tuples, or discriminated unions, including nullable tuple carriers and
  pattern combinators.
- Top-level `not` and `and` patterns now participate in discriminated-union
  and enum exhaustiveness, including complements of payload cases whose
  payload domains are not themselves finite.
- Closed type unions and sealed hierarchies now apply conservative
  none/some/all coverage algebra to `not` and `and` patterns. Nullable domains
  likewise recognize `null`/`not null` as complementary coverage.
- Constant-true nested guarded patterns now contribute their underlying
  coverage, while dynamic or false guards remain conservative. A rest-only
  sequence pattern is recognized as total for a compatible sequence input,
  and reachability diagnostics use the same shared catch-all classification.
- Compile-time-true match-arm guards now contribute consistently in every
  domain, including `bool` and catch-all reporting. The singleton `unit` and
  null-only domains are analyzed explicitly.
- Match diagnostics and `SemanticModel.GetMatchExhaustiveness` now use one
  authoritative evaluator across boolean, nullable, enum, union, sealed
  hierarchy, structural, and numeric pattern domains. Diagnostics report every
  missing semantic case returned by the API, while flow-sensitive struct-union
  default-state handling remains limited to catch-all reachability warnings.
- Match diagnostics and the semantic exhaustiveness API now use the same
  interval analysis for integral comparison, range, `not`, `and`, and `or`
  patterns. Complementary numeric arms can prove a match exhaustive, guarded
  arms remain conservative, and a redundant catch-all is reported after full
  explicit coverage.
- Match exhaustiveness now combines nested discriminated-union case patterns,
  so arms such as `.Error(.WrongCredentials)` and
  `.Error(.ServiceUnavailable)` can collectively cover the complete `Error`
  payload without requiring a discard arm. Finite `bool` payloads and bounded
  Cartesian combinations of multiple finite payloads are analyzed likewise.
- Adopted `let`/`var` as the standard spelling for lexical bindings while
  retaining `val`/`var` for properties and signature-like declarations. A
  `let` local remains semantically read-only and is displayed as `val` by hover
  and symbol presentation. The former optional `PreferValInsteadOfLetAnalyzer`
  was replaced by the optional `PreferLetInsteadOfValAnalyzer` (`RAV9035`) and
  its code fix. `RAV9004` and its code fix are now provided by
  `VarCanBeLetAnalyzer` and recommend `let` when a lexical `var` is never
  reassigned.
- Async-iterator method declarations now suspend incomplete awaits in
  `MoveNextAsync` and return a
  pending `ValueTask<bool>` instead of synchronously blocking in
  `TaskAwaiter.GetResult()`. Their kickoff methods now carry
  `AsyncIteratorStateMachineAttribute` metadata, so async streams such as the
  greenhouse telemetry sample no longer occupy the caller thread while
  awaiting delays or I/O.
- Added primary-constructor accessibility modifiers after the type name and any
  type parameters, e.g. `record struct Year private (Value: int)`. Constructor
  accessibility is independent of accessibility on promoted parameters, so
  records and other primary-constructor types can expose data while restricting
  construction to factories or the containing assembly.
- Improved language-server recovery after rapid edits by keeping analyzer
  diagnostics on the active compiler snapshot and forwarding reusable
  incremental semantic state across intermediate snapshots.
- Reduced analyzer latency by enumerating narrowly registered expression-statement
  operation actions without constructing unrelated operation graphs, and by
  filtering unused-method invocation candidates before semantic lookup.
- Reused metadata load contexts across incremental compilations when portable
  metadata references are unchanged.
- Kept document diagnostics demand-driven for source declarations instead of
  eagerly declaring every project syntax tree after each edit.
- Added struct-like discriminated union cases with named payload fields, e.g.
  `case Closed { Reason: string? = null }`. Defaulted fields are optional in
  named case construction, and `.Closed { ... }` lowers through the synthesized
  case constructor rather than mutable object initialization.
- Added statement-form `loop { ... }` for unconditional loops. `break` exits the
  loop and `continue` jumps to the next iteration using the same structured
  loop rules as `while` and `for`.
- Added labeled `break label` and `continue label` for targeting enclosing
  labeled loops. Unlabeled `break` and `continue` still target the closest
  enclosing loop, and labels on ordinary statements remain `goto` targets.
- Added keyword-first `match scrutinee { ... }` as the normal match expression
  form, aligning match expressions with match statements. The older postfix
  expression form remains supported for composition cases such as
  `try expr match { ... }`.
- Added support for `[method: ...]` attributes on class, struct, and record
  declarations with primary constructors, applying them to the synthesized
  constructor metadata.
- Added unsafe block expressions, allowing `unsafe { ... }` in value-producing
  expression positions while reusing the existing scoped unsafe context rules.
- Added `RAV0404` so conditional access reports an error when `?.` is used on
  a statically non-null receiver while preserving member binding for tooling.
- Fixed interface contract diagnostics so concrete classes report missing
  required interface members such as `IDisposable.Dispose`.
- Fixed interface contract diagnostics so source explicit interface method
  implementations satisfy the required interface member even when the emitted
  method name is interface-qualified, and default interface members are not
  treated as missing required implementations.
- Fixed union case binding so bare case constructor calls require an explicit
  target type, while pattern hovers report case symbols projected from the
  matched union type arguments.
- Fixed cold language-server hover resolution for pattern locals nested in
  executable scopes such as `await for`, including both declarations and uses.
- Fixed union declaration attribute validation so source unions accept
  type-level attributes whose usage targets either class or struct carriers.
- Changed `RAV9016` member-can-be-private and `RAV9017`
  member-can-be-static analyzer diagnostics to default to informational
  suggestions. `RAV9017` no longer suggests making methods static when they
  satisfy inherited interface contracts such as `IDisposable.Dispose`.
- Added code fixes for compiler-owned match exhaustiveness diagnostics: `RAV2100`
  can insert a missing match arm, and `RAV2103` can remove a redundant catch-all arm.
- Aligned union content nullability with C# unions: Raven now tracks nullable
  parenthesized union contents from constructor/member case types, treats
  `TryGetValue(out T)` as an extraction helper instead of an extra case source
  when constructors exist, and imports nullable C# union contents from .NET 11
  metadata.
- Aligned nullable union contents with the C# access pattern: `HasValue` now
  follows `Value != null`, `null` patterns over class unions check both the
  carrier reference and active `Value`, and nullable-content parenthesized
  unions no longer expose `null` as a pseudo member type. Bare `null` no longer
  implicitly converts to nominal or Raven.Core union carriers just because one
  payload type is nullable.
- Changed plain Raven `union` declarations to synthesize struct carriers by
  default, matching the C# generated-union direction. Raven.Core `Union<...>`,
  `Option<T>`, and `Result<T, E>` now use that default struct carrier shape.
  Struct-union match exhaustiveness now follows the C# contract: declared cases
  are source-exhaustive, and the inactive `default` carrier is not treated as a
  semantic case that must be written in source. Defensive catch-all arms on
  struct unions are still allowed when local flow says the inactive carrier
  state is physically possible, but active local values report redundant
  catch-all arms. Passing a struct-union value that may still be the inactive
  `default` carrier to a struct-union parameter now reports `RAV0405` at the
  call site, so callee parameters can keep their active-value contract. Omitted
  optional struct-union arguments whose default is the inactive carrier now
  report the same diagnostic. Lowering and emit keep responsibility for
  defensive runtime fallbacks when metadata consumers or forced default carriers
  bypass Raven's source checks.
- Returning a struct-union value that may still be the inactive `default`
  carrier now reports `RAV0406` at the return boundary, preserving the same
  active-value contract for callers.
- Fixed matching over nullable union carriers (`U?`) so union case patterns are
  checked against the underlying union while `null` is treated as a separate
  nullable-wrapper case for exhaustiveness. This applies to both `union struct`
  and `union class` carriers and does not make `null` a union pseudo-case.
- Added .NET 11 C# interop coverage for Raven-produced union carriers and made
  metadata nullability loading tolerate preview reflection types that do not
  support `NullabilityInfoContext`.
- Added `SemanticModel.GetMatchExhaustiveness(MatchStatementSyntax)` so tooling
  can query the same exhaustiveness information for match statements that it
  already can for keyword-first and postfix match expressions.
- Struct-union parameters and `self` are now treated as active inside the
  callee, relying on call-site diagnostics to reject possibly inactive carriers
  before entry. Raven.Core `Option<T>` and `Result<T, E>` helpers no longer need
  source-level defensive default arms, and lowered source-exhaustive matches now
  throw when no arm matches instead of falling through with a default result.
- Raven.Core `Option<T>` and `Result<T, E>` JSON converters now serialize the
  inactive default carrier as JSON `null` instead of emitting no token or an
  empty object.
- Fixed expanded `params` argument target typing so extra positional arguments
  are bound against the params element type, including target-typed union cases.
- Fixed extension member completion after partially typed member names so
  imported metadata extension methods are offered for prefixes such as
  `widget.Dou`.
- Fixed editor compiler diagnostics after hover/inlay-style semantic queries so
  presentation-only cache entries do not cause false missing local or missing
  member errors in the same document snapshot.
- Fixed member completion for interface-typed receivers so members inherited
  through implemented interfaces are offered on values such as `IQueryable<T>`.
- Added `scripts/build-project-samples.sh` to build all source sample projects
  under `samples/projects` separately from the standalone sample compiler script.
- Converted `Raven.Core` to a normal Raven MSBuild project so it builds through
  the shared Raven language targets and participates in project references.
- Added receiver-aware pipe target completion after `|>` and in the following
  identifier, including applicable in-scope functions/static methods and
  extension methods. The language server now registers `>` as a completion
  trigger so typing `value |> ` opens the suggestion list.
- Fixed editor diagnostics after text edits so syntax diagnostics are refreshed
  from the pending document text immediately, clearing stale parser errors
  while semantic diagnostics remain deferred.
- Fixed editor diagnostic flicker while typing by translating the last computed
  snapshot diagnostics across pending edits until fresh diagnostics are ready.
- Changed `RAV0403` to report on the full `<expr>!` nullable suppression
  expression and describe that the operand is treated as non-null.
- Fixed member completion after nullable suppression expressions such as `x!.`
  and target-typed `default!.`.
- Fixed inlay hint flicker while editing by keeping visible providers stable
  until a debounced refresh can request translated cached hints for pending
  document text or fresh hints from the loaded workspace snapshot.
- Fixed semantic queries for top-level global statements so editor features bind
  through the compiler-owned top-level statement binder instead of throwing.
- Added `RAV9034` for standalone value-producing expressions whose result is
  known to be unused, such as literal/variable unary and binary expressions in
  `unit`-returning bodies. Calls remain exempt.
- Fixed `RAV9033` disposable-object diagnostics to use generic disposable-value
  wording instead of guessing an object name from locals or producer members.
- Fixed hover on `default` expressions so it shows a `default(T)` constant
  expression preview instead of being suppressed as a keyword.
- Fixed `use` declarations so nullable disposable targets such as
  `IDisposable?` are rejected and invalid resources are not registered for
  disposal.
- Fixed inlay hint refreshes for top-level invocations with function arguments
  so the refreshed request does not fail while rebinding global statements.
- Fixed semantic symbol info for callable instance members so hovering or
  analyzing the invoked name in `callback()` returns the member symbol instead
  of the delegate `Invoke` method.
- Fixed document compiler diagnostics so attributes on union declarations are
  validated against the union type instead of synthesized helper methods after
  editor semantic warm-up.
- Fixed local symbol queries so inferred generic constructor initializers such as
  `val values = List<JsonValue>()` return the constructed type instead of an
  incomplete `List<>` symbol.
- Fixed `self.` completion inside instance members and instance extension
  members, restored partial property/event definition-implementation merging,
  and re-enabled fast semantic coverage for positional pattern assignments.
- Fixed attribute diagnostics so `GetDiagnostics()` reports invalid attribute
  targets, duplicate attributes, and non-constant attribute arguments during the
  diagnostic pass instead of depending on prior `GetAttributes()` queries.
- Fixed semantic diagnostics so method-like members, primary constructor
  parameters, indexer parameters, indexer async getters, and constructor
  initializers are reported during `GetDiagnostics()` even when symbol
  declarations were already cached.
- Fixed diagnostic reuse for type declarations so partial-method, sealed
  hierarchy, and static-type storage diagnostics remain available after
  executable binding reuses cached declaration state.
- Fixed duplicate diagnostics when rebinding finalizer declarations and partial
  method definition/implementation counterparts.
- Fixed complete semantic diagnostics so `GetDiagnostics()` collects
  declaration-binder diagnostics instead of taking the document-scoped
  incremental diagnostics path.
- Fixed top-level `Main` entry-point discovery so invalid file-scoped statements
  report `RAV1021` without also synthesizing or selecting a competing
  top-level-program `Main`.
- Fixed completion on cold semantic models so earlier top-level declarations
  initialized from invocations or function expressions contribute their inferred
  types to member lists and completion descriptions.
- Fixed full diagnostics for top-level function attributes and extern
  top-level functions with bodies.
- Fixed qualified generic type lookup in member-access-shaped type expressions
  such as `System.Func<int, string>`.
- Fixed macro-expanded local declarations so documentation-comment lookup uses
  the declarator syntax node instead of a token-only span, avoiding crashes when
  inspecting expanded documents.
- Fixed member completion after `nameof(...)` so the receiver is treated as
  `string` instead of using the named symbol's type.
- Fixed target-typed `default` for reference types so it is treated as a
  nullable null value. Returning or assigning it to a non-nullable reference now
  requires `default!` and reports the existing null-assignment diagnostic when
  omitted.
- Added first-class MSBuild language targets for `.rvnproj` builds. Raven projects
  now build through `dotnet build`, produce SDK-style outputs, and can be consumed
  from C# projects through normal `ProjectReference` when wired to
  `build/Raven.Language.targets`.
- Deprecated legacy `.ravenproj` project files in favor of MSBuild-backed `.rvnproj`
  projects. The CLI now warns when compiling a legacy project file.
- Added `[Receiver]` and `[Receiver<T>]` trailing-block parameters. An
  unparameterized trailing block passed to a one-argument function parameter
  marked with `[Receiver]` can access receiver members directly inside the block;
  `[Receiver<T>]` narrows member lookup to an explicit compatible receiver type.
- Added combined builder/receiver trailing blocks for DSLs such as
  `[Builder<UiBuilder>, Receiver<WindowBuilder>] content: () -> UiNode`. The
  result builder handles block lowering, while the receiver builder exposes the
  component-specific member scope and produces the sub-result through
  `BuildFinalResult(component, receiver)`.
- Added class-only `base` expressions for instance members, enabling explicit
  base-member access and non-virtual base method invocation such as
  `base.OnFrameworkInitializationCompleted()`.
- Added `_` discard parameters for function expressions and parameterized
  trailing blocks. They consume the delegate parameter slot without introducing
  a body-visible name or unused-parameter warning.
- Trailing blocks now bind to the final visible function-typed parameter even
  when earlier optional parameters are omitted with default values, enabling DSL
  APIs such as `StackPanel(spacing: 8.0) { ... }` with
  `content: (() -> UiNode)? = null`.
- Added opt-in diagnostic `RAV9029` for bare member invocations and member accesses whose
  returned value is ignored. Assign the returned value to a target, assign it to `_`, return
  it, or pass it on. The analyzer is disabled by default while it uses whole-analyzer mode.
- Added `--returned-value-handling <default|full|none|info|warning|error>` and
  `--force-returned-value-handling` to configure `RAV9029` from the compiler CLI.
- Added project-file mode configuration for `RAV9029` through `ReturnedValueHandlingMode` /
  `RavenReturnedValueHandlingMode` and `EnableReturnedValueAnalyzer` /
  `RavenEnableReturnedValueAnalyzer`.
- Extended unused-variable analysis to report unused callable parameters as warning
  `RAV9030`, covering methods, `func` statements, constructors, operators, and function
  expressions.
- Added hidden analyzer diagnostic `RAV9031` for unused wildcard namespace imports within
  the lexical scope that declares them, with cleanup support through the redundant-import
  code fix.
- Added analyzer diagnostic `RAV9033` for disposable objects returned from calls or object
  creation that are assigned to ordinary locals or discarded without a `use` declaration or
  direct `Dispose()` call before scope exit.
- Added source-applicable invocation parameter-name inlay hints. Positional arguments now
  display their resolved parameter names, such as `StackPanel(spacing: 8.0)`, while already
  named arguments are left alone. Positional and nominal deconstruction patterns now also
  display inferred element names when the tuple or `Deconstruct` shape provides them.
  Raven inlay hints now have a master VS Code setting plus separate per-kind settings for
  inferred types and name hints.
- Fixed editor diagnostic scheduling so open, edit, and save follow-up passes include
  analyzer diagnostics such as unused locals and parameters, while typing uses a throttled
  document-scoped analyzer pass instead of running full-project analyzers on every edit.
- Fixed Raven.Core metadata union case imports so `import System.Result.*` and
  `import System.Option.*` bring `Ok`, `Error`, `Some`, and `None` into
  unqualified scope even though the PE case types are emitted as standalone
  types.
- Fixed `RAV9012` so inferred target declarations such as `val x = ...` are not
  reported just because the initializer has a nullable type.
- Fixed `RAV9019` so async `Main(args: string[]) -> Task` methods identified as
  application entry points are not reported as unused when a synthesized entry-point
  bridge is used.
- Fixed pattern matching, propagation, and carrier conditional access over
  Raven.Core metadata unions by matching logical case wrappers and constructed
  PE case types by stable metadata identity.
- Unused parameter, method, and property analyzer diagnostics now skip members
  that are required by virtual/override or interface implementation contracts.
- Fixed full-document and focused-range inlay hints for small real-world files
  so target-typed constructor shorthand arguments such as
  `.(1, "Ana", 29, true)` still show source-applicable parameter names.
- Fixed VS Code inlay refresh behavior so Raven edits re-request visible hints
  after the existing debounce, and locally superseded inlay requests no longer
  publish an empty hint set that can make hints flicker off.
- Fixed VS Code project build, run, and debug commands so `.rvnproj` targets use
  the `rvn build` frontend instead of invoking the `rvnc` compiler driver with
  publish-only arguments.
- Split unused local and unused parameter analysis into distinct built-in analyzers while
  keeping `UnusedVariableAnalyzer` as a compatibility disable name, and tightened analyzer
  symbol matching so equivalent lazy-bound symbols are compared with Raven symbol equality
  instead of object identity.
- Renamed the property initialization diagnostic analyzer to
  `UninitializedPropertyAnalyzer`, generalized its wording from auto-properties to
  stored properties, and added `UninitializedFieldAnalyzer` for explicit private fields.
- Constructor declarations now participate in lightweight member signature declaration,
  making symbol-based analyzers see constructor parameters deterministically after edits
  without requiring a prior full body bind.
- Workspace analyzer diagnostics now log cache hits, misses, stores, cancellations,
  failures, and per-analyzer execution failures so editor diagnostic latency can be
  traced without conflating it with foreground semantic requests.
- Analyzer infrastructure now supports Roslyn-style operation actions through
  `RegisterOperationAction`, and the document analyzer driver dispatches them from one
  shared operation traversal. Returned-value and immutable-collection result analyzers
  now use operation actions instead of syntax callbacks that each queried operations.
- Language-server semantic tokens now focus on semantic symbol classifications and
  regex string specialization, leaving keywords, literals, comments, and operators to
  the VS Code TextMate grammar. The grammar now covers ordinary attributes,
  documentation comments, character literals, labels, constructor-like calls, dot
  punctuation, and missing Raven keywords such as `goto`, `yield`, `fixed`, and `new`.

### Changed
- Unused local value diagnostics now say `Value '<name>' is never used.` while unused
  parameters continue to say `Parameter '<name>' is never used.`.
- `TopLevelAttribute` is now generated in the `System.Runtime.CompilerServices`
  namespace, so namespace-member containers are marked with
  `System.Runtime.CompilerServices.TopLevelAttribute`.
- Namespace-level `func` and `const` declarations now bind as namespace-level members emitted into a synthesized `[TopLevel]` `NamespaceMembers` container, and static types marked with `[TopLevel]` promote their static members through namespace lookup/completion. `AllowNamespaceMembers` controls declarations independently from top-level statements, while `AllowNamespaceMemberImports` controls namespace promotion from namespace-member containers.
- Project and single-file compilations now generate a prelude of global imports by default, including common `System` namespaces plus `System.Result.*` and `System.Option.*`; ordinary union cases are no longer introduced unqualified unless imported or referenced with target-typed `.Case` syntax.
- Attached declaration macros targeting types are now valid on union case declarations, matching the compiler's representation of cases as generated case types.
- Records now use the full primary-constructor parameter list as their canonical value shape, including non-public promoted parameters, and record bodies now reject extra instance storage and secondary instance constructors.
- The language server now provides source-applicable inlay hints for inferred local type annotations and inferred function return type annotations, and the VS Code extension can toggle those hints with `raven.inlayHints.inferredTypes.enabled` or `Raven: Toggle Inferred Type Inlay Hints`.
- Language-server document edits now preserve `SourceText` change ranges through incremental sync, fall back to full parsing for whole-document or large paste edits, debounce macro-consumer refreshes, and keep normal typing diagnostics syntax-only so expensive semantic diagnostics wait for open/save.
- Match expression arms now accept direct `return` expressions, aligning them with other expression-oriented value positions while preserving diagnostics for statement `return` inside block-expression arms.
- `for` loop identifier targets now support explicit type annotations such as `for item: int in items`, and inferred type inlay hints are offered for unannotated identifier targets.
- Outer pattern-binding contexts now allow implicit deconstruction captures to carry type annotations without repeating the binding keyword, so forms such as `val (key: string, value: int) = entry` and `val [head: string, ..tail: string[]] = values` parse as typed captures.
- Equality operands now target-type member-binding shorthand such as `value == .Case`, matching pattern shorthand while still allowing `value is .Case` when pattern syntax better communicates intent.
- Raven unions now align their emitted interop surface with the .NET 11 union
  direction by implementing `IUnion`; body-declared Raven case types are recorded
  on the carrier with Raven-owned metadata instead of non-standard system case
  marker attributes.

### Fixed
- String `==` and `!=` now use `System.String` value equality instead of
  reference equality.
- Function expressions inside instance methods now capture unqualified instance
  property receivers correctly.
- Interface-typed receivers now resolve `System.Object` instance members such
  as `GetType`, `ToString`, `Equals`, and `GetHashCode`.
- Function expressions now capture variables assigned through the left side of an
  assignment, including function expressions passed as call arguments, and nested
  function expressions now reuse the owning method closure instead of snapshotting
  stale values.
- Diagnostic binding now follows macro replacement declarations, preventing
  attached property macros from reporting the original property as a duplicate
  member after the replacement property has already been registered.
- Target-typed enum member defaults on external enum parameters and `double`
  default parameter constants now bind and emit without compiler crashes.
- Emitting direct signatures over NuGet `ref/` assembly types now prefers the
  corresponding `lib/` runtime assembly and guards type probing failures,
  preventing external packages such as Avalonia from crashing emit during
  runtime type resolution.
- Metadata base-type resolution now falls back to compilation-level package
  references when a module-local reference walk misses, fixing inherited
  member lookup and reference conversions across package sibling assemblies
  such as Avalonia `Button` to `Interactive`.
- Runtime MethodInfo resolution now handles methods from constructed generic
  package types, fixing emit for calls such as Avalonia `StackPanel.Children.Add`.
- The compiler CLI now copies native NuGet runtime assets for the current
  platform when running or publishing, so packages such as Avalonia can load
  native dependencies like SkiaSharp from the output directory.
- Unused-parameter analysis now treats constructor parameters passed to
  constructor initializers such as `base(value)` as used.
- `MemberCanBeStatic` now recognizes instance callable members invoked through
  bare identifier syntax, avoiding false positives for callback wrapper methods.
- Unused-property analysis now respects interface property implementations.
- Removed builder-method-name exemptions from unused-method analysis so generic
  unused-member diagnostics are not coupled to DSL lowering conventions.
- Incremental executable-owner analysis now treats top-level `func` statements as
  function owners instead of generic global-statement owners, improving editor
  recovery after wrapping top-level statements in `func Main`.
- Semantic invocation queries can now use already-available argument types to
  construct simple generic metadata candidates, avoiding unnecessary body
  rebinding for language-service hovers such as `JsonSerializer.Serialize` /
  `Deserialize<T>` chains.
- Large full-document inlay hint requests now avoid cold expensive binding
  fallbacks, reducing editor request pile-ups while small documents and precise
  range requests can still bind to fill missing hints.
- Full-document inlay hint responses now skip eager tooltip markdown generation,
  keeping initial annotation payloads lighter while focused range requests still
  include richer tooltip content.
- Pattern inlay hints now skip assignment patterns that deconstruct into
  existing variables, while still annotating `val`/`var`/`let` pattern
  declarations and inline pattern bindings.
- The redundant-import quick fix now offers a document-level action to remove all imports already covered by global imports.
- Optional enum parameter defaults now accept target-typed member binding syntax such as `value: ServiceLifetime = .Scoped`.
- Qualified constant-member patterns such as `value is Math.PI` and enum-member patterns such as `value is JsonValueKind.True` now bind and emit as value comparisons instead of type tests.
- Enum conversions now follow C#/CLR rules for explicit enum-to-integral, integral-to-enum, and enum-to-enum conversions, and emitted casts preserve CLR-open enum values that are not declared members.
- Attribute arguments now accept enum constants in qualified and target-typed forms, including enum flag compositions such as `.Class | .Delegate`.
- Type wildcard imports now expose enum members alongside normal static members and constants, and individual enum members can be imported as specific constant imports.
- Delegate declaration attributes now bind to the delegate type, validate against the CLR `delegate` attribute target, and emit to delegate metadata.
- Generated display-class closure frame types now consistently carry `CompilerGeneratedAttribute` metadata.
- Metadata type symbols now preserve declared visibility from referenced assemblies, and delegate declarations now emit CLR delegate metadata with nested placement, `abstract sealed` flags, by-ref parameter shapes, and `unit` `Invoke` returns as `void`.
- Conditional element access such as `values?[index]` now emits correctly for array receivers.
- Line-leading pointer dereference assignments such as `*ptr = value` now parse as new statements after expression statements instead of being treated as multiplication continuations.
- Semantic symbol queries for user-defined unary and binary operator expressions now return the selected operator method instead of rebinding the expression out of scope.
- Mixed nullable equality checks with user-defined equality operators no longer recurse through target-type lookup.
- `GetDeclaredSymbol` on field declarators now returns the declared field instead of routing through local-variable binding.
- `GetDeclaredSymbol` on function-statement parameters now returns the method parameter symbol, and member-access completion can resolve parameter receivers without a prior bind.
- `GetDeclaredSymbol` on async methods with annotated `Task<T>` return types now completes the method signature before returning the symbol, avoiding stale provisional `Task` skeletons.
- Early method signature symbols now mark `ref` and `out` parameters as mutable, matching fully bound parameter symbols.
- Field-targeted attributes on auto-properties are now attributed to the synthesized backing field rather than validated against the property symbol.
- Expression-backed value patterns such as `person is { Name: name }` now compare against the runtime value of `name` even when `name` is a parameter or local rather than a compile-time constant.

## 2026-05-09

### Changed
- `RavenUnionJsonConverter` has been renamed to `RavenTaggedUnionJsonConverter` to make its tagged JSON shape explicit.
- `RavenTaggedUnionJsonConverter<TUnion>` now writes direct parenthesized union members such as scalar values and arrays under a tagged `value` payload, preserving existing flattened output for body-form cases while allowing `JsonValue[]` members to serialize.
- Added a dedicated `json-modeling-playground` sample that models JSON structure with records and unions, using both built-in Raven.Core and custom JSON converters.
- Collection literals target-typed as a union now use the single collection-shaped union member when one exists, so nested values such as `JsonValue[]` can be inferred inside `JsonValue` dictionaries.
- Language-server open and save events now schedule a deferred full diagnostic pass after the immediate syntax pass, so analyzer diagnostics appear without requiring a document edit and stale analyzer diagnostics can be cleared after saving.
- Language-server semantic tokens now skip unmapped classifications instead of emitting default keyword tokens, and classify local declaration/designation identifiers from syntax so open-document tuple deconstruction edits keep correct spans and token types.
- Semantic declared-symbol lookup for pattern declaration assignments now binds the owning statement before fallback synthesis, so hovering a tuple-deconstruction declaration such as `val (no, _) = Get()` reports the same element type as later references.
- Language-server tuple type hovers now present the underlying `ValueTuple<...>` shape with its implemented interfaces, while tuple element type hovers resolve to the individual element types for both named and unnamed tuple syntax.

## 2026-05-08

### Changed
- Attached property macros that replace their target declaration with syntax derived from the original property now reuse the effective declaration symbol, preventing false duplicate-member diagnostics while preserving generated accessors.
- The language server now ignores IDE build/debug artifact folders such as `.raven-build` and `.debug` when deciding whether watched file changes should reload the workspace, so compiling from the editor does not disturb open-document semantic state.

## 2026-05-07

### Changed
- Target-typed constructor binding now supports `.(...)`, allowing assignments, arguments, and collection elements with a known target type to construct that type without repeating its name.

## 2026-05-04

### Changed
- Runtime-async entry-point bridges targeting .NET 11 now call `System.Runtime.CompilerServices.AsyncHelpers.HandleAsyncEntryPoint(...)` for `Task` and `Task<int>` `Main` methods instead of hand-emitting awaiter blocking, while Raven-specific `Result<..., ...>` entry points keep their result-mapping bridge.
- `Raven.Core` now treats each target-specific `bin/<Configuration>/<TargetFramework>/Raven.Core.dll` as an incremental build output, so it is regenerated only when the Raven source list or sources change, or when the target DLL is missing.
- Standard union type syntax is back: `T1 | T2` now parses as a type annotation
  and binds to `System.Union<T1, T2>` from `Raven.Core`, with arities two
  through five.
- `$identifier` string interpolation shorthand now preserves the identifier width even for keyword-shaped names, preventing subsequent syntax and editor spans from drifting while binding can still diagnose unresolved names.
- `Option<T>` JSON serialization now maps `.Some(value)` directly to the payload JSON and `.None` to `null`, matching JSON's native nullable-property shape. `Result<T, E>` keeps its tagged converter shape.
- Constant field emission now supports narrow and unsigned primitive constants, fixing metadata enum members with byte-backed values such as `JsonValueKind.Null`.

## 2026-05-03

### Changed
- Trailing blocks now support an optional parameter clause before the body, such as `GET("/{id:int}") { id => ... }` or `Combine { (left, right) => ... }`, and use the declared arity during overload resolution.
- Target-typed union case construction now works in constructor arguments even when overloads have same-arity parameters, so nested calls such as `Theme(None)` and `Theme(.None)` bind against an `Option<T>` parameter without requiring `Option<T>.None`.
- The language server now ignores generated, package/cache, build output, and temporary probe directories when discovering projects or reacting to watched-file changes, reducing full workspace reload storms in VS Code.
- Semantic queries over brace trailing blocks now bind the trailing block expression instead of throwing, so hover and related editor features remain stable when the cursor lands inside that syntax.
- VS Code syntax highlighting and language-server semantic tokens now classify call targets inside trailing-block DSLs consistently, including uppercase extension-style calls and constructor-like calls such as `GET("/") { ... }`.
- Generic type construction can now infer type arguments from explicit function-expression parameters even when a same-named non-generic type exists, enabling DSL shapes such as `GET("/{id:int}", func (id: int) => ...)` to select `GET<int>` when the non-generic constructor is not applicable.
- Repeated trailing-block calls in the same scope now emit distinct lambda bodies, including builder-rewritten trailing blocks used by lightweight DSLs.

## 2026-05-02

### Changed
- `Raven.Core` now includes the generic `RavenTaggedUnionJsonConverterFactory`/`RavenTaggedUnionJsonConverter<TUnion>` implementation for opt-in JSON serialization of ordinary Raven unions, while `Option<T>` and `Result<T, E>` keep their specialized JSON converters.
- Generic Raven union JSON serialization now supports a configurable case discriminator property. `"$case"` remains the default, and `[RavenTaggedUnionJsonConverter("kind")]` can be used when a domain-specific property name fits better.

## 2026-05-01

### Changed
- Overload resolution now target-types collection literal arguments against array-shaped parameters when overload candidates disagree on the parameter type, so calls such as `Activator.CreateInstance(type, [value])` bind to the `object?[]` overload instead of falling back to an inferred immutable list.
- Trailing blocks can now receive implicit closure parameters from the selected final function parameter. Parameters are available as Swift-style `$0`, `$1`, etc., and `it` aliases the first lambda parameter.

## 2026-04-30

### Changed
- Brace trailers now bind as Swift-like trailing closure call syntax. `callee(args) { ... }` appends a zero-argument closure as the final argument, and `callee { ... }` is accepted when overload resolution can bind that trailing closure.
- Trailing closure parameters annotated with `[Builder<T>]` now activate builder-block binding for expression components and `if`/`else` composition through Swift-like builder methods such as `BuildExpression`, `BuildBlock`, `BuildOptional`, `BuildEither`, and `BuildFinalResult`.
- `TrailingBlockExpressionSyntax` now wraps a normal block body, so statements inside trailing blocks are ordinary Raven statements instead of initializer-style entries.

Impact:
- `Type { ... }` is no longer an initializer-like DSL placeholder. It is valid only when a function, method, delegate invocation, or constructor accepts the final closure argument; object initialization remains `Type with { ... }`.

## 2026-04-24

### Changed
- `while` statements now support the same outer pattern-binding form as `if`, allowing loops such as `while val pattern = expr { ... }` where captured pattern locals are available inside the loop body.
- Object initialization can now use the `Type with { ... }` form. The compiler binds this through the existing object-initializer path, so `init`, `required`, compound assignment, and event subscription semantics are preserved while brace trailers remain available for future DSL work.
- Brace trailers are now represented in the syntax tree as `TrailingBlockExpression` nodes with `TrailingBlockEntry` children instead of object-initializer syntax nodes, matching their role as the future DSL block surface.
- Brace trailers no longer bind as object initializers. They now report a dedicated trailing-DSL diagnostic until DSL binding support is introduced.
- The language docs and reference spec now describe the current union-body model more directly: body-form unions use `case` declarations inside an ordinary member body, may contain authored members beside cases, may be declared `partial`, reserve `Value`/`HasValue`, and follow record-like `ToString()` override behavior while still rejecting authored union equality/hash special members.
- The compiler diagnostics reference now includes the union-specific reserved-name and unsupported-special-member diagnostics `RAV2111` and `RAV2112`.
- Language-server semantic tokens now recognize string arguments passed to parameters annotated with `System.Diagnostics.CodeAnalysis.StringSyntaxAttribute.Regex` and classify those literals as `regexp`.
- Attribute binding and emission now route `[module: ...]` attributes to the module symbol and `[field: ...]` attributes on auto-properties to the synthesized backing field.

Impact:
- The written language reference now matches the compiler and `Raven.Core` surface more closely for modern unions such as `Option<T>` and `Result<T, E>`.
- Editors can apply regex-aware highlighting to Raven string literals when APIs use the standard .NET string-syntax annotation.
- Module-level metadata and backing-field-specific property annotations now round-trip through symbols and emitted assemblies.

## 2026-04-19

### Changed
- Union body declarations now require the `case` keyword for each declared case, and the parser preserves that keyword in the syntax tree for nested union case clauses.
- Samples, language docs, and compiler-facing symbol displays now reflect the prefixed declaration form, including member-keyword formatting for nested union case types.

Impact:
- Union declarations now read as `union Result<T> { case Ok(value: T) case Error(error: E) }`, which gives nested case declarations an explicit syntactic marker ahead of larger union-surface changes.
- Tools and diagnostics that request member keywords now identify nested union case types as `case` members instead of displaying them like ordinary nested types.

## 2026-04-18

### Changed
- Top-level binder creation no longer eagerly binds global statements during root-binder setup. Raven now finishes source declaration/member registration across the compilation before top-level statements are bound, which removes file-order sensitivity for top-level code that touches members declared in other source files.
- Match-expression arms now tolerate the arm expression starting on the line after `=>`, which fixes recovery/binding failures for multiline union matches such as JSON serialization helpers.

Impact:
- Multi-file Raven projects no longer spuriously report missing members like `RAV0103` / `RAV0117` just because a top-level statement bound before another file had registered its members.
- Newline-styled `match` arms bind the same way as single-line arms, which makes editor diagnostics and sample projects much less brittle around multiline union handling code.

## 2026-04-08

### Changed
- Deconstruction patterns now support named elements for `Deconstruct`-backed shapes in both matching and declaration/assignment forms. Raven accepts forms such as `Person(Items: val items, Name: val name, Age: 42)` and `val (Items: items, Name: name, Age: age) = person`, binds named elements by `Deconstruct` parameter name in any order, and now reports `RAV1602` when a supplied deconstruction name does not exist on the target shape.
- Union carriers now expose a conventional union-root `Value` property, and `union struct` carriers reserve discriminator `0` as an uninitialized/default state so `default(U).Value` is `null` until a real case is assigned.
- `Raven.Core` now declares `Option<T>` and `Result<T, E>` as `union class` carriers instead of `union struct`, removing the implicit default/uninitialized state from the standard library’s primary algebraic carriers.
- Synthesized union `Value` now follows the carrier nullability contract more closely: `union struct` exposes `Value: object?`, ordinary class carriers expose `Value: object`, and class carriers with nullable member payloads expose `Value: object?`.
- Synthesized union carriers now also expose `HasValue`, allowing callers to distinguish default/uninitialized `union struct` values from active cases even when nullable annotations are not observed by consuming C# code.
- Statement-form `if`, `if val`, `while`, and `for` bodies can now be written without braces when the body statement starts on the next line. Raven now rejects same-line non-block forms such as `if flag return`, while still allowing block bodies and `else if` chaining on one line.
- Parenthesized union declarations now use `|` between member types instead of `,`, and compiler-facing displays such as symbol formatting, hover text, signature help, samples, and spec examples now reflect the bar-separated form consistently.
- Async iterators now support C#-style enumeration cancellation. `CancellationToken` parameters marked with `[EnumeratorCancellation]` receive the token passed to `GetAsyncEnumerator(...)`, Raven warns when async iterators declare `CancellationToken` parameters without marking one, and parenthesized async lambdas accept the inline parameter-attribute form `async ([EnumeratorCancellation] token: CancellationToken) => ...`.
- Iterator statements now accept the shorthand `yield expression` in addition to `yield return expression`. Both spellings lower identically, while `yield break` remains the early-termination form.

Impact:
- Raven unions now align more closely with the emerging .NET/C# union contract: tooling and runtime consumers can inspect the active carrier payload through `Value`, while defaulted struct unions no longer masquerade as the first declared case.
- `Option` and `Result` now model only their authored case sets in ordinary use, instead of also carrying a silent struct-default state that callers had to treat as an extra runtime possibility.
- Control-flow statements read more naturally in Raven’s newline-sensitive style without reopening the same-line ambiguity that previously made single-statement bodies look like adjacent tokens instead of a structured body.
- Parenthesized unions now align their declaration syntax with Raven’s broader union-type notation, so authored code and tooling output present the same shape for unions like `union Payment(Cash | Card)`.
- Async streaming code now follows the same cancellation model as C# async iterators, including Minimal API-style handlers that expose the request cancellation token through an attributed lambda parameter.
- Iterator code can now use the shorter `yield value` spelling without changing semantics, which better matches the fact that iterator elements are produced rather than returned from the method.

## 2026-04-05

### Changed
- Collection expressions now continue to honor builder-backed target types such as `ImmutableArray<T>` in ordinary assignments, expression-bodied returns, and object-initializer property assignments. Non-empty `[...]` still reject non-collection targets, but they no longer fall back to `ImmutableList<T>` when the target is a supported builder-backed collection.

Impact:
- Samples and APIs that expose `ImmutableArray<T>` regain target-typed `[]` behavior, including macro expansion results and object-initializer assignments, while the earlier overload-resolution fix still preserves the intended `Cannot convert from 'ImmutableList<T>' to 'T'` diagnostic for non-collection targets.

## 2026-04-04

### Changed
- The Raven VS Code extension now supports `raven.sdkPath`, an SDK-root override that lets one extension installation target different Raven toolset builds by resolving `Raven.LanguageServer.dll`, `rvn.dll`, and related assemblies from a chosen SDK directory.
- Workspace-built language servers launched by the VS Code extension are now staged into an isolated extension-owned directory but keep the repository root as their working directory, so repo-relative assets such as `Raven.Core.dll` still resolve without leaving the live workspace binaries locked.
- The VS Code extension now restarts the Raven language client when workspace folders change in the same session, so diagnostics and project loading re-root correctly after switching between sample folders.

Impact:
- Developers can validate multiple Raven SDK builds or packaged toolsets against the same VS Code extension without rebuilding or retargeting the extension itself.
- Using the editor no longer competes as aggressively with local Raven builds, while project-backed diagnostics like `HelloWorld` still resolve `Raven.Core` and other repo-relative assets correctly.
- Opening a different Raven workspace in the same VS Code session no longer leaves the language server pinned to stale project roots.

## 2026-04-02

### Changed
- Overload resolution now respects `System.Runtime.CompilerServices.OverloadResolutionPriorityAttribute` on applicable methods, including methods imported from referenced assemblies. Higher-priority candidates are kept before Raven runs its usual specificity comparison.
- Function and block bodies can now declare local `class`, `struct`, `record`, and `enum` helper types. These declarations are block-scoped in source and emitted as compiler-mangled nested types under the enclosing containing type.
- Imported extension members are now classified per member instead of per container. Classic extension methods continue to use `IsExtensionMethod`, while Raven/C#-style static extension members bind through extension-receiver metadata even when they live in mixed extension containers. Generic metadata extension methods now recover method type parameters correctly during PE import.
- Source classic extension methods declared with `static` members plus `ExtensionAttribute` now remain discoverable during same-compilation binding and through `CompilationReference` imports. Raven no longer caches `IsExtensionMethod = false` just because the method symbol was observed before its parameter symbols were assigned.
- `Raven.Core` parse errors now use a stricter `IError` contract: `Message` is required, `Cause` remains optional, and `IError.WithMessage(...)` now preserves the original error as the wrapped cause instead of constructing an invalid `ContextError`.
- `Raven.Core` now exposes generic `ContextError<TError>` and a typed `WithMessage(...)` wrapper, so Raven code can retain the concrete wrapped error type while still surfacing the shared `IError` contract. When callers only have `IError`, the erased wrapper shape is `ContextError<IError>`.
- `Result<T, E>` now also supports `WithMessage(...)` when `E : IError`, projecting only the error channel to `ContextError<E>` instead of wrapping the entire result carrier.
- Imported metadata types now compute `AllInterfaces` transitively from declared interfaces and base types instead of relying on reflection’s flattened view. This restores generic constraint checks like `E : IError` for metadata-backed types such as `ParseIntError` implementing `IParseError : IError`.
- Source explicit interface implementations now bind correctly for methods and properties because source interface members are registered before classes that implement them, including nested interface declarations. This also unlocks explicit interface property implementations such as `val IError.Cause`.
- Generic methods that lower captured lambdas now emit generic display classes when they need the enclosing method's type parameters. This fixes runtime `BadImageFormatException` failures in patterns such as `Result<T, E>.WithMessage(...)` implemented via `MapError(error => error.WithMessage(message))`.

Impact:
- Raven now matches C#’s overload-priority behavior for APIs that intentionally hide more specific overloads behind `OverloadResolutionPriorityAttribute`, which improves interop with modern .NET libraries and C#-authored metadata.
- Helper types can now live next to the code that uses them without being promoted to outer type scope, while keeping runtime metadata isolated behind compiler-generated nesting names.
- Mixed extension containers in referenced assemblies now interoperate more like .NET/C#: `int.parse(...)` binds again as a static extension member, while classic generic extension methods like `OptionExtensions.UnwrapOr<T>` continue to import as extension methods instead of degrading to unreadable metadata signatures.
- Classic C#-style source extension methods are stable again across both direct source binding and referenced-compilation imports, which restores samples and semantic tests that rely on `static class` + `[ExtensionAttribute]` interop semantics.
- Parse-oriented Raven APIs now expose a more coherent error surface to both Raven code and .NET consumers: every `IError` has a meaningful message, wrapping keeps provenance through `Cause`, and `Parse.rav` no longer relies on an invalid constructor call during core emission.
- Error-wrapping code no longer has to choose between provenance and static type information: callers can use `ContextError<TError>.Cause` when they want the concrete wrapped error, or treat the wrapper as plain `IError` through the explicit interface `Cause`.
- Result pipelines can now add context at the right abstraction level: `int.parse(text).WithMessage("...")` keeps the carrier as `Result<T, ...>` and only enriches the error payload.
- Metadata-backed generic constraints now see transitive interface implementations consistently, so extension members like `Result<T, E>.WithMessage(...) where E : IError` bind correctly from `Raven.Core.dll` and other referenced assemblies.
- Raven can now express .NET-style explicit interface members in source without spurious `RAV0315` failures, which makes contracts like `IError.Cause` compose cleanly with typed overload properties on the same type.
- Captured-lambda codegen is now stable for generic helper methods that flow constrained type parameters through higher-order functions, so error-channel projection helpers like `WithMessage` no longer compile successfully and then fail at runtime with invalid IL.

## 2026-04-01

### Added
- `Raven.Core` now defines `System.IParseError`, `System.ParseIntError`, `System.IntErrorKind`, and lowercase `int.parse(...)` static extension helpers that return `Result<int, ParseIntError>` instead of throwing for null, empty, format, and overflow failures.

Impact:
- Raven code can now use `int.parse("42")` and propagate parse failures through `Result` pipelines with `?`, avoiding direct dependency on CLR exceptions at the call site.

## 2026-03-28

### Changed
- `catch` clauses now reuse Raven’s pattern syntax instead of a bespoke `catch(Type name)` declaration form. Raven accepts preferred forms like `catch FormatException ex` and still parses parenthesized patterns such as `catch (FormatException ex)` for grouping and forward compatibility.
- Sealed hierarchies now include interfaces: Raven accepts `sealed interface` declarations, allows optional `permits` clauses on interfaces, and enforces the closed set across direct implementors and subinterfaces.
- Nested type declarations inside interfaces now participate in sealed-hierarchy modeling, so interface-scoped case-like records/classes can be used as direct sealed-interface members.
- Nested direct cases inside generic sealed hierarchies no longer capture outer type parameters at runtime. They now behave like algebraic-data-type cases, which fixes invalid CLR generic nesting and runtime failures such as `BadImageFormatException` when constructing generic sealed-interface cases.
- Sealed hierarchy signatures and hover now print `sealed` for sealed classes and interfaces, bare generic sealed roots are diagnosed consistently in storage-type positions, and target-typed `.Case(...)` patterns now bind for nested sealed-hierarchy direct cases when the scrutinee already determines the sealed root.
- Method generic `where` clauses are now initialized consistently for member methods as well as local functions, which fixes constrained generic math scenarios such as `where T : INumber<T>` inside sealed-hierarchy evaluators and other generic member bodies.
- Built-in binary operator binding now follows a fuller predefined numeric-promotion model, so `float`, `uint`, `ulong`, `short`, `ushort`, and `sbyte` participate consistently instead of only `int`/`long`/`double`/`decimal` plus a few ad hoc promoted cases.
- Unused-variable analysis now treats interpolated-string identifier reads as real local usage and falls back to binder-based local lookup when symbol lookup does not report the local directly, which fixes false positives such as `val content = ...; return "submitted: $content"`.
- `typeof` over open generic source types now emits the generic type definition token instead of an invalid placeholder-instantiated runtime type, which fixes runtime failures in scenarios like `typeof(Result<,>)` inside JSON converter factories.
- The file-local type modifier is now spelled `fileprivate` instead of `filescope`, aligning the surface syntax with its accessibility semantics and Swift-style precedent.

Impact:
- Exception handling syntax now aligns more closely with the rest of Raven’s pattern surface, reducing one-off grammar and leaving room for future richer catch-pattern work.
- Raven can model Java/Kotlin-style sealed interface families directly, including patterns where the direct cases live inside the interface declaration.
- Exhaustiveness and hierarchy validation now treat sealed interfaces consistently with sealed classes and record classes.
- Generic sealed hierarchies can now use nested case declarations without forcing CLR-style outer generic qualification such as `Expr<float>.Case`, which better supports ADT and future GADT-style modeling.
- Sealed-hierarchy direct cases are now documented and implemented as full named types whose nesting is optional source organization, while the nested form still supports `Expr.Case(...)` construction and target-typed `.Case(...)` patterns.
- Generic member methods now honor their declared `where` constraints during body binding, so generic math interfaces like `INumber<T>` can drive operator binding in normal member methods and generic sealed-hierarchy evaluators.
- Numeric expressions across Raven’s predefined types now behave much more uniformly, including float arithmetic/order comparisons and the unsigned/small-integral families.
- Interpolated strings no longer trigger bogus `RAV9027` warnings for locals that are only read inside `$name` / `${expr}` segments, including project-based app samples and async handler code.
- Project-based apps and Raven.Core JSON converters no longer fail with `BadImageFormatException` just from inspecting `Result<...>` / other open generic source types via `typeof`.
- Source code, tests, specs, and editor grammar should now use `fileprivate` for file-local type-like declarations and extensions.

## 2026-03-26

### Changed
- Synthesized union `ToString()` bodies now quote generic string and char payloads on the bound-body path, so parenthesized generic unions print values like `Either<Int32, String>("invoice")` and `Either<Char, String>('x')` instead of emitting raw unquoted payload text.
- Hover text for extension members now identifies them as extension methods/properties and shows the qualified declaring extension container instead of collapsing them into an ordinary containing type display.

Impact:
- Generic union `ToString()` output is now consistent with other quoted literal-style displays for string and char payloads, especially on synthesized carrier formatting paths.
- Extension APIs are easier to distinguish from ordinary instance members during hover in VS Code, especially when users need to see which extension declaration contributes a member.
- Hover now makes it clear which extension declaration contributes a member when multiple similarly named members are in scope.

## 2026-03-25

### Changed
- Raven unions now explicitly use one runtime model: a carrier plus independent case types. Body-form unions continue to synthesize case types, but those case types no longer form an inheritance hierarchy with the union root.
- Union construction, matching, propagation, and conditional-access lowering now consistently target carrier semantics, with `TryGetValue(out CaseType)` and pattern matching as the extraction surface.
- Compiler naming and docs continue the move away from the old “discriminated union” terminology toward the simpler `union` / `union case` vocabulary where possible.

Impact:
- `union` now has a clearer contract: it describes a closed carrier type rather than an inheritance-oriented object model.
- Users who want OOP subtype semantics should prefer Raven sealed hierarchies, while unions remain the right tool for closed carrier-style data modeling and Result/Option-style APIs.

## 2026-03-24

### Changed
- Declaration-oriented separated lists now accept newline-delimited separators in more places, including enum member lists, parameter lists, type-parameter lists, and type-argument lists. The syntax tree preserves explicit separator tokens when present, uses `SyntaxKind.None` for valid newline-delimited boundaries, and recovers same-line omissions with missing expected separator tokens.
- Enum member lists now also accept `;` as an explicit separator alongside `,`, while keeping comma as the canonical recovery separator when an explicit same-line separator is missing.
- Enum member lists now also diagnose mixed explicit separator kinds within the same declaration, so `,`/`;` style stays internally consistent while newline-delimited implicit boundaries remain neutral.
- Added warning `RAV9028` for unnecessary trailing separators in ordinary comma-delimited separated lists with closing delimiters. The warning only applies to real explicit trailing separator tokens and does not fire for newline-delimited implicit boundaries or enum member lists.
- Newlines are now modeled strictly as trivia in the syntax tree. Implicit statement and declaration termination uses surrounding end-of-line trivia together with `SyntaxKind.None` terminator/separator slots instead of any dedicated newline token.
- Imported .NET nullability now preserves ordinary nullable reference annotations such as `object.Equals(object?)`, `object.ToString() -> string?`, and `Console.ReadLine() -> string?`, while generic type-parameter positions only become nullable when metadata carries explicit `NullableAttribute` flags. This restores metadata-backed conversions like `string? -> Option<string>` without regressing LINQ and collection APIs that use plain `T`.
- Record value-member synthesis and record `with` expressions now work again under the imported-nullability model, because synthesized record support can once more see the expected nullable `object` members from metadata.
- Property patterns and nominal deconstruction patterns now treat nullable scrutinees such as `object?` as valid runtime-test inputs when the underlying non-nullable type can participate in the pattern. This fixes cases like `if candidate is Shipment { ... }` and `if x is Foo(...)` where the input was nullable only because of flow/state, not because the pattern itself was invalid.
- Patterns that introduce bindings now support nested `when` guards inside the pattern itself. This works in statement-form conditional binding, `for` pattern targets, and collection-comprehension pattern targets, so forms like `for val (id, amount when > 100) in orders` and `if val (id, name when name.Length > 5) = customer { ... }` bind the value and then apply either a secondary pattern guard or a boolean guard expression in the bound-local scope.
- Match exhaustiveness now treats pure deconstruction inside discriminated-union case payload patterns the same as direct payload binding when the deconstruction is total. In particular, extension-based `Deconstruct` patterns such as `.Error((val message))` no longer force a redundant `_` arm just to satisfy exhaustiveness.
- Syntax highlighting now treats `default`, type-parameter variance keywords (`in`/`out` in generic parameter lists), and conversion-operator keywords (`implicit`/`explicit`) as first-class keyword/modifier scopes in the editor grammar, and focused semantic/highlighter tests lock that coverage in.
- Static framework and user-defined types now follow normal .NET storage rules during binding. Raven reports `RAV2810` when a static type is used for a local, field, property, indexer, or parameter type instead of silently accepting declarations such as `val file: File`.

Impact:
- Deconstruction code can now keep the matched value in scope while still filtering on that same value, instead of forcing users to choose between pattern-only matching (`> 100`) and a named binding (`amount`).
- Result-style matches can now use payload deconstruction directly inside a case arm without losing redundant-catch-all warnings or adding placeholder fallback arms.
- Editor coloring for generic variance, conversion operators, and `default` literals is now more consistent across themes that did not visibly style the generic operator-word scope.
- Static types now behave more like they do in C# at declaration sites, so invalid storage declarations fail early with a targeted diagnostic instead of surfacing later binder or emit noise.

## 2026-03-20

### Added
- Raven now supports F#-style scoped pinning through `use ptr = fixed &expr` in unsafe contexts. The `fixed` initializer yields a native pointer, requires explicit address-taking with `&`, and releases the pin automatically when the `use` scope exits.
- `use` declarations now also support an explicit nested-scope form, `use value = expr in { ... }`, which is equivalent to a nested block starting with the `use` declaration and avoids ambiguity with object initializer braces.
- The macro spec and focused tests now explicitly define how attached declaration macros compose when multiple macros target the same declaration and when both a parent declaration and its members use macros.
- Collection comprehensions now accept pattern targets, including deconstruction patterns, so forms like `[for val (key, value) in pairs => ...]` and `[for val (2, name) in people => name]` behave consistently with `for` statements.

### Changed
- Attached declaration macros are now documented as a source-ordered same-target pipeline: each macro sees both the original authored declaration and the current pre-application declaration, replacement results feed later macros on that declaration, introduced members are integrated first, the last replacement wins for the declaration itself, and peer declarations are integrated afterward.
- Result propagation lowering and block-expression codegen are now more robust in composed expression contexts. Propagated expressions used inside invocation/object-creation arguments are lowered through temporaries before emission, nested propagate nodes are rewritten consistently, exception-to-error rewriting only synthesizes a catch path when an actual `Exception` can convert into the enclosing error payload, and discard-context block expressions no longer leak `Unit` values onto the evaluation stack.

Impact:
- Managed storage can now be pinned without introducing a separate C#-style `fixed (...) { ... }` statement, so pinning composes with Raven’s existing `use` lifetime model and keeps address selection explicit.
- Resource lifetimes can now be narrowed inline without relying on extra surrounding braces, while object-initializer forms such as `use obj = Foo { Value = 2 } in { ... }` remain syntactically clear.
- Macro authors now have a stable, documented composition model to target, including explicit access to both authored syntax and composed same-target syntax, while IDE expansion views still show the full declaration result after all attached macros have run.
- Comprehensions can now reuse Raven’s existing pattern/deconstruction surface directly in collection-building code instead of forcing tuple/item access inside the selector.
- Raven code that combines `?` propagation with method arguments, generic calls, and lowered block expressions now emits valid IL and runs correctly instead of failing with `InvalidProgramException` or stack-shape bugs in mixed lowering/codegen paths.

## 2026-03-19

### Changed
- Nullable conditional member access now supports statement-form assignment. Raven accepts `x?.Name = value` and compound forms like `x?.Name += delta`, evaluates the receiver once, and skips the write when the receiver is `null`.
- Collection literals now have a clear split between general collection expressions and explicit arrays. Plain `[...]` remains the general collection form, defaulting to `ImmutableList<T>` in untyped contexts and `List<T>` when prefixed with `!`, while explicit arrays now use `[| ... |]`.
- Target typing still governs how `[...]` binds in typed contexts, so existing assignments such as `int[] = [1, 2, 3]`, `ImmutableArray<int> = [1, 2, 3]`, and `List<int> = [1, 2, 3]` continue to work without extra syntax.
- Collection expressions now also support dictionary-shaped literals. In addition to `key: value` entries, dictionary literals can now spread other dictionary-compatible sources with `...expr`, use single-entry spread syntax like `...key: value`, and build entries through dictionary comprehensions such as `[for item in items => item.Name: item.Value]`. Targetless forms follow the same immutable-by-default rule as list literals: bare forms infer `ImmutableDictionary<TKey, TValue>` and `!` forms infer `Dictionary<TKey, TValue>`.
- Pattern matching and deconstruction now support keyed dictionary forms. Raven can match dictionary-compatible values with patterns like `["a": val first, "b": 2]`, and declaration/assignment deconstruction now supports keyed extraction such as `val ["a": first, "b": second] = values`.
- Sequence-pattern slice captures now preserve concrete collection families when the scrutinee has one. Rest and fixed-segment captures over `List<T>`, `ImmutableList<T>`, and `ImmutableArray<T>` now bind back to those same collection types instead of degrading to `T[]`, while strings and arrays keep their existing slice behavior.
- Array support is now more stable across jagged and multidimensional CLR shapes. Jagged arrays continue to work through nested one-dimensional arrays, multidimensional array indexing/assignment now binds and emits correctly, and internal CLR type normalization no longer collapses multidimensional array metadata to `T[]`. Collection/array literal syntax remains intentionally single-dimensional, so explicit multidimensional array construction still goes through runtime APIs such as `System.Array.CreateInstance(...)`.
- Statement-form conditional pattern binding is now explicitly documented and test-covered for property patterns, so forms like `if val Person { Name: "Ada", Age: age } = value { ... }` are treated as part of the normal general-pattern surface rather than as an undocumented side effect of the shared binder path.

Impact:
- Raven code can now express common null-guarded property/field updates without spelling an explicit `if receiver != null` block, while compound assignments preserve the usual single-evaluation guarantee for the left-hand side.
- Raven local code now reads more consistently: `[...]` stays list-oriented unless target-typed otherwise, while `[| ... |]` carries explicit array intent through spreads and other composed expressions.
- Raven collection literals can now describe both list-like and dictionary-like construction without introducing a separate keyword or constructor-style syntax.
- Destructuring and pattern matching over immutable collections are now more predictable because captured slices keep the same collection semantics as the source value instead of silently changing APIs and mutability characteristics.
- Keyed lookup scenarios can now stay in Raven’s existing pattern/deconstruction syntax instead of dropping to manual `ContainsKey` / indexer code for dictionaries.
- Existing array code is more predictable: nested array literals keep working for jagged arrays, multidimensional interop no longer loses rank information in emitted metadata, and unsupported multidimensional literals now fail at analysis time instead of reaching broken codegen.

## 2026-03-18

### Added
- `SemanticModel.GetExpandedRoot()` and `Document.GetExpandedSyntaxRootAsync()` now expose an incremental expanded-document view that rewrites attached declaration macros and freestanding expression macros into a single syntax root for tooling and debugging.
- Raven now supports a `fileprivate` modifier on type-like declarations. File-scoped declarations bind only within the declaring source file, file-scoped partial types must stay in one file, and emitted type/container metadata names are mangled so file-local helpers do not publish a stable CLR-facing name.

### Changed
- `rvn` now supports `--dump-macros [original|expanded|both][:plain|pretty[:no-diagnostics]]` so a single-file compile can show the pre-expansion source beside the currently expanded macro view, either as raw text or highlighted output.
- `.debug` compiler captures now also include per-document macro original/expanded source snapshots, including a plain text highlighted dump for the expanded view.
- Macro language-service support now treats macro names as first-class completion sites: `#[...]` offers attached macro names, `#name(...)` offers freestanding macro names before the call is complete, and macro hovers include kind/target/argument hints alongside the existing expansion preview.

Impact:
- Macro debugging from the CLI no longer requires manually inspecting per-node expansion results just to compare authored source with the compiler’s current expansion output.
- Tooling and tests can request one expanded syntax root directly instead of reconstructing document-level macro output ad hoc.
- The ReactiveMacros-style editing loop is more discoverable because authors now get completion at the macro invocation site and immediate hover guidance about what a macro applies to before expanding it.

## 2026-03-17

### Changed
- Partial nominal types now behave consistently across classes, structs, records, and interfaces. Matching partial declarations merge into one type symbol, interface parts can contribute members across files, and conflicting accessibility/type-parameter shapes now report dedicated diagnostics instead of silently taking whichever declaration bound first.
- Partial methods, partial properties, and partial events are now supported inside partial nominal types. Raven accepts declaration/implementation pairs, merges them into a single symbol, and reports dedicated diagnostics when either side of the pair is missing or when a property/event implementation is left as auto/field-like syntax.

Impact:
- Multi-file type organization is now more predictable because partial-type compatibility is checked explicitly instead of depending on declaration order.
- Library authors can now split method/property/event contracts from their implementations in the same way they already split types, while still getting clear compiler feedback when a partial-member pair is incomplete.

## 2026-03-16

### Changed
- Collection expressions now reserve `...` for general spread segments and treat bare range elements such as `[1..3]`, `[1, 3..4, 9]`, and `[1..<4]` as inline sequence expansion. Constant-bounds range elements also participate in fixed-length array inference for targetless literals, including constant endpoints like `const MAX_VALUE = 10; [3..MAX_VALUE]`. This also fixes exclusive upper-bound handling for range-backed collection comprehensions so `..<` stops before the upper endpoint consistently.
- Raven now supports single-dimensional fixed-length array types written as `T[N]`. The compiler tracks the declared length on array symbols, preserves it through emitted `System.Runtime.CompilerServices.FixedLengthArrayAttribute` metadata, allows implicit conversion from `T[N]` to open `T[]`, and uses the fixed length during sequence-pattern/deconstruction analysis.
- Plain local collection literals now infer fixed-length arrays when the total length is statically known. That includes fixed-length array spreads, so expressions like `[...a, 3]` infer a fixed-length result when `a` is `T[N]`, while spreads from open arrays and comprehensions still infer open arrays.
- Fixed-length-array assignment/conversion failures now report size-aware diagnostics for open-array-to-fixed-array and mismatched fixed-length assignments instead of falling back to generic conversion errors.
- Sequence patterns now accept bare `...` as a non-capturing rest segment, so forms like `[first, ...]` and `[first, ..., last]` ignore the unmatched slice without introducing a binding. Captured rest segments like `...rest` may likewise appear in the middle or at the end of the pattern.
- Sequence-pattern captures over fixed-length arrays now preserve inferred segment sizes when the width is statically known. For example, deconstructing `int[4]` with `[a, b, ...rest]` binds `rest` as `int[2]`, and `[..2 head, tail]` over `int[3]` binds `head` as `int[2]`.

Impact:
- Raven now preserves obvious fixed array lengths without forcing annotations in local collection-expression code, while still keeping inference conservative in cases such as comprehensions and open-array spreads where the compiler does not yet model a statically known length.
- Raven now supports postfix nullable suppression via `expr!` as a narrow interop-oriented escape hatch. The parser models it as `SuppressNullableWarningExpression`, nullable references narrow to their underlying non-nullable type without changing runtime codegen, and nullable value types reuse the existing unwrap path. Using `!` now reports warning `RAV0403`, and this also fixes false `RAV0162` unreachable-code warnings on forms like `return value!`.
- Added statement-form conditional pattern binding via `if val pattern = expr { ... }` / `if var pattern = expr { ... }`. The compiler lowers this through the existing pattern-matching machinery, and the dedicated syntax node for the form is now `IfPatternStatement`.
- Statement-form conditional pattern binding now supports typed implicit captures under the outer binding keyword, so forms like `if val x: int = input { ... }` narrow nullable values and bind `x` without requiring an inner `val x: int`.
- Nominal `Type(...)` patterns now work for deconstructable primary-constructor classes and structs in addition to records. Public promoted `val` / `var` parameters synthesize a `Deconstruct` method in declaration order, so class patterns like `if val Person(1, name, _) = person { ... }` bind and type-check the same way as record patterns.
- `for` loop headers now accept an optional outer binding keyword before the iteration target. Forms like `for val item in items { ... }` and `for val Person(1, name, _) in persons { ... }` are supported, and for pattern targets the outer binding keyword supplies the binding mode for otherwise bare captures using the same shorthand rule as deconstruction assignment.
- `match` arms now accept an optional outer binding keyword before the arm pattern. Forms like `val [first, second, ...rest] => ...` and `val Some((x, y)) => ...` are supported, and the outer keyword supplies the binding mode for otherwise bare captures in the arm pattern.
- Structural patterns now support trailing whole-pattern designations consistently across `if val pattern = expr`, `for val pattern in values`, and match arms. Forms like `if val (2, > 0.5) point = input`, `for val Person(1, name, _) person in persons`, and `val Some((x, y)) pair => ...` bind the full matched value when the pattern succeeds.
- Explicit pattern comparisons now use a single comparison-pattern family across `==`, `!=`, `<`, `<=`, `>`, and `>=`. The parser no longer produces a separate explicit-value-pattern syntax node for `== expr`; compiler APIs now expose `ComparisonPatternSyntax` / `BoundComparisonPattern` / `IComparisonPatternOperation` consistently for all operator-led pattern comparisons.
- Comparison and range patterns now require the operand/bound type to match the scrutinee type after plain-type unwrapping. Raven no longer applies ordinary implicit numeric widening inside patterns, so forms like matching an `int` against `> 0.5` now report `RAV1606` instead of silently converting the operand.
- Record-pattern diagnostics now describe the real requirement: the nominal type must support deconstruction, not merely carry the `record` modifier.
- `RAV2704` now suggests the concrete `Task<...>` wrapper Raven expects when an `async` method, property getter, or function expression is annotated with a non-task return type, and async lambdas with that error now suppress the confusing follow-on body conversion diagnostic that previously obscured the root cause.

Impact:
- Swift-style conditional binding can now be written directly in statement form without introducing a separate `is` condition by hand, while still reusing Raven’s existing pattern scoping, shadowing, and flow analysis rules.
- Primary-constructor nominal types participate more naturally in positional matching and deconstruction-based APIs because the compiler now supplies a consistent `Deconstruct` surface for their promoted public state.

## 2026-03-15

### Changed
- `for` loop headers now accept pattern targets in addition to simple identifiers, so forms like `for (val x, 0) in points { ... }` and `for [val head, ..val tail] in values { ... }` lower to per-element pattern guards instead of requiring a manual `if value is ...` inside the loop body.
- Removed the legacy `for each` / `await for each` syntax. Raven now uses `for` and `await for` exclusively, with `_` or an omitted target for element-discarding loops.
- Macro plugins can now report macro-specific validation diagnostics with custom messages and optional argument locations through `MacroExpansionDiagnostic` plus helper methods on macro contexts, without having to manufacture raw compiler `DiagnosticDescriptor` instances.
- The existing `RAV9012` nullable-type guidance now offers a scoped `"Rewrite nullable flow to Option pattern matching"` code fix for simple local flows, rewriting a nullable local plus its immediately following `if x != null` / `if x is not null` branch into an `Option<T>` local and `Some(...)` pattern check when all uses stay inside that guarded flow.
- Style-only source-shape rewrites now use the new context-driven refactoring pipeline instead of built-in analyzer diagnostics. Target-typed union-case rewrites, expression-body/block-body conversions, redundant accessor removal, and string-concatenation rewrites now surface as on-demand editor suggestions without occupying the diagnostics list.
- Added a separate `"Convert if/else to match"` refactoring for pattern-based `if` statements, so control-flow shape changes are independent from the nullable-to-`Option` migration.
- `"Convert if/else to match"` now preserves common complementary union cases when rewriting pattern checks, so `Some(...)` rewrites pair with `None` and `Ok(...)` rewrites pair with `Error` instead of falling back to `_`.
- Raven code actions now expose preview entries that open a before/after diff for both diagnostic-backed fixes and context-driven refactorings, using the same general preview model instead of feature-specific expansion viewers.
- Signature help now behaves more like C#: partial invocations no longer crash extension-method pre-inference, and the language server gathers overloads from the underlying method group so `Foo(` can continue showing the full overload list instead of collapsing to only the currently selected candidate.

Impact:
- Collection iteration can now express filtering and deconstruction directly in the loop header, and the published grammar/editor tooling no longer advertises the retired `each` keyword.
- Macro authors can surface input-validation errors at the macro or argument site using a stable compiler-owned diagnostic path (`RAVM021`) while still keeping existing raw diagnostic emission available for advanced cases.
- Nullable-to-option guidance can now upgrade straightforward user-authored null-guarded locals into idiomatic `Option<T>` flow without crossing broader API boundaries or forcing a separate control-flow shape rewrite.
- Built-in diagnostics are now more focused on policy and correctness guidance, while purely optional shape rewrites come from refactoring providers and no longer require suggestion-mode analyzers.
- Users can inspect the effect of a Raven fix/refactoring before applying it, which makes the new suggestion-only actions usable without having to trust the edit blindly.
- Overload help is now more stable while typing incomplete calls and more useful for overloaded APIs, because the editor keeps showing the full callable surface even after one overload becomes the current best match.

## 2026-03-13

### Changed
- Inline and freestanding positional/list/record/member patterns now require an explicit binding keyword (`val`, `var`, or `let`) to capture variables; bare identifiers in those pattern positions are interpreted as existing-value matches instead. Assignment/declaration deconstruction shorthand such as `(a, b) = expr`, `val (a, b) = expr`, `[a, b] = expr`, and `val [a, b] = expr` is unchanged, and inline collection rest captures now use forms like `..val rest`.
- Collection patterns and collection deconstruction now support fixed-size sequence segments with operator-first syntax such as `[..2 val start, val end]`, alongside `..val rest` / `...val rest`. Strings participate in the same model: single-element captures bind `char`, while fixed/rest segment captures bind `string`.

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
- `SyntaxFactory` token convenience members now return fresh token instances instead of reusing shared singleton tokens, so Raven-authored macros can safely use helpers like `CommaToken` and `SetKeyword` multiple times while building detached syntax trees.
- Statement factory convenience overloads now default `TerminatorToken` to `SyntaxKind.None`, matching the parser’s newline-as-trivia model so raw `SyntaxFactory` statements no longer synthesize newline terminator tokens.
- `SyntaxFactory` convenience overloads can now be defined explicitly in `Syntax/Factories.xml`, and the node generator validates those overload definitions against the syntax model so invalid slot mappings and hazardous combinations like non-null `Body` plus `ExpressionBody` are rejected during generation.
- Nodes with explicit `Syntax/Factories.xml` definitions now expose only those validated red `SyntaxFactory` overloads, instead of also publishing a raw full-slot overload that could bypass invariants such as `AccessorList` plus `ExpressionBody` on the same declaration.
- Explicit syntax-factory overloads can now declare carefully-chosen aliases such as `StoredPropertyDeclaration`, with generated XML docs that make clear the alias is only a descriptive wrapper over the canonical factory shape.
- `Raven.CodeAnalysis` now emits XML documentation files, and PE symbol documentation lookup correctly resolves sidecar XML member IDs for generic parameter types, so Raven code can consume generated `SyntaxFactory` documentation from metadata references.
- Metadata documentation lookup now supports assembly-adjacent Markdown sidecars (`<AssemblyName>.docs/manifest.json` + symbol files), prefers Markdown over XML when both exist, and uses hashed XML-doc-ID filenames to keep metadata doc paths stable and filesystem-safe.
- Hover and signature help now render XML documentation comments into readable Markdown sections instead of showing raw XML fragments, so metadata docs from XML sidecars display cleanly in the editor.
- Markdown documentation comments now support structured `.NET`-style block tags such as `@param`, `@typeparam`, `@returns`, and `@remarks`, and the shared documentation formatter renders those tags into clean hover/signature-help sections instead of showing the raw tag lines.
- Added a sibling-project `markdown-docs` sample that exercises Markdown documentation, structured tags, `xref:` links, and XML/Markdown sidecar emission across a library and consumer project.
- Hover and signature help now rewrite documentation `xref:` links into actionable editor commands that open Raven symbol documentation pages, instead of degrading those references to plain display text.
- Markdown sidecar files may now carry optional top-of-file front matter such as `xref: ...`; that metadata is stripped before rendering and used only to bind/validate the document against a specific symbol.
- Markdown documentation structure extraction is now exposed through a shared API, and XML emission reuses that extracted summary/parameter/returns/remarks shape instead of flattening Markdown comments into a single raw `<summary>` blob.
- Documentation extraction is now centered on a format-neutral Raven documentation structure, so both Markdown and XML comments project into the same intermediate model before being rendered or emitted.
- Project builds now require an explicit `GenerateXmlDocumentationFromMarkdownComments` opt-in before Markdown-authored comments are projected into emitted XML documentation; XML-authored comments continue to emit normally without that flag.
- Recognized Markdown documentation headings such as `### Remarks` now flow through the shared documentation structure instead of being rendered once as raw body text and again as a structured section, so hover/signature-help output no longer duplicates those sections.
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
- Red `SyntaxFactory` now trends toward valid-by-construction APIs for nodes with explicit factory definitions, while low-level tests can still use node constructors when they intentionally need malformed or manually-tokenized syntax.
- Raven-authored tools and macro projects can now surface XML documentation from referenced `Raven.CodeAnalysis` APIs such as `SyntaxFactory` aliases instead of seeing empty metadata docs.
- Raven-authored tools and future RavenDoc output can target one shared metadata documentation convention, with Markdown sidecars taking precedence while preserving XML fallback for ordinary .NET libraries.
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
- Added a sibling-project `samples/projects/macro-freestanding` sample showing a Raven-authored freestanding expression macro plugin and executable app project using `#add(...)`.
- Added a sibling-project `samples/projects/macro-reactive` sample showing an attached property macro and a freestanding subscription macro working together in Raven-authored projects.
- Changed the VS Code extension defaults to disable color decorators in Raven files so freestanding macros like `#add(...)` do not trigger hex-color pickers.
- Changed macro contracts so `MacroKind` is inferred from `IAttachedDeclarationMacro` and `IFreestandingExpressionMacro`, removing redundant boilerplate from implementations.
- Changed `macro-reactive` to use `System.Reactive` and `IObservable<T>`/`Subject<T>` in the sample runtime shape instead of a custom in-sample observable type.
- Fixed sequence-point emission for macro-generated zero-width spans so generic introduced-member initializers no longer crash emit.
- Changed: compiler-emitted documentation now writes symbol-addressable outputs.
  Markdown uses assembly-adjacent `.docs/` sidecars with an `invariant/`
  locale root, and XML uses standard `<doc><members>` symbol IDs instead of the
  old file/line dump format. This aligns emitted docs with metadata lookup in
  the IDE/compiler and leaves room for RavenDoc/localization integration later.
- Changed: Raven's workspace/MSBuild project model now preserves
  `GenerateDocumentationFile`, `GenerateMarkdownDocumentationFile`,
  `DocumentationFile`, and `MarkdownDocumentationOutputPath` on open/save so
  documentation emission settings round-trip cleanly through project editing.
## Unreleased

### Added
- Added a separate context-driven code refactoring provider pipeline so editor suggestions can appear without requiring a backing diagnostic. The workspace and language server now surface diagnostic-backed quick fixes and diagnostic-free refactorings as distinct code action sources.
- RavenDoc now publishes Raven API-reference sites from `.rvnproj` projects,
  individual source files, source directories, or compiled libraries with
  adjacent Markdown `.docs` sidecars. Its built-in responsive presentation now
  uses Raven-specific navigation, branding, light/dark styling, and offline
  syntax highlighting for fenced Raven code.
- RavenDoc and the browser Playground now share a consolidated Raven visual
  foundation across light and dark modes, including brand, color, typography,
  surface, and code-presentation tokens. The Playground's Monaco editor follows
  the active system color scheme with matching Raven syntax themes, and its
  persisted theme selector can explicitly choose System, Light, or Dark.

### Changed
- Removed the legacy `new Foo(...)` object-creation syntax. Raven object construction now uses direct type invocation (`Foo(...)`) consistently across parsing, samples, and tests.
- Top-level type declarations are now hoisted for binding, so console-app file-scope code can be interleaved with `class`, `struct`, `record`, `enum`, `union`, `interface`, and `delegate` declarations without triggering ordering diagnostics.
- Parenthesized unions now support nominal deconstruction patterns over their declared member types, so matches like `Cash(val amount)` and `Card(val reference)` lower through the same `TryGetValue` carrier extraction path as `Cash cash` and `Card card`.
- Function expressions can now be iterator generators both with declared iterator return types and with inferred iterator return types. Raven now lowers `yield` inside lambda/function-expression bodies to the same synthesized iterator state machines used for ordinary functions, including `IEnumerable<T>` and `IAsyncEnumerable<T>` shapes.

Impact:
- Higher-order Raven APIs can now keep generator logic inline in function expressions instead of forcing local helper functions just to use `yield`.
