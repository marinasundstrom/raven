# Raven Changelog

Behavior-focused timeline covering **2025-09-12** to **2026-05-09**.

## Unreleased

- Added a static-hostable Blazor WebAssembly playground starter with a Monaco
  editor, Raven TextMate highlighting, separate Compile and Run commands, and
  in-browser compilation and execution of emitted Raven assemblies.
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

### Changed
- Removed the legacy `new Foo(...)` object-creation syntax. Raven object construction now uses direct type invocation (`Foo(...)`) consistently across parsing, samples, and tests.
- Top-level type declarations are now hoisted for binding, so console-app file-scope code can be interleaved with `class`, `struct`, `record`, `enum`, `union`, `interface`, and `delegate` declarations without triggering ordering diagnostics.
- Parenthesized unions now support nominal deconstruction patterns over their declared member types, so matches like `Cash(val amount)` and `Card(val reference)` lower through the same `TryGetValue` carrier extraction path as `Cash cash` and `Card card`.
- Function expressions can now be iterator generators both with declared iterator return types and with inferred iterator return types. Raven now lowers `yield` inside lambda/function-expression bodies to the same synthesized iterator state machines used for ordinary functions, including `IEnumerable<T>` and `IAsyncEnumerable<T>` shapes.

Impact:
- Higher-order Raven APIs can now keep generator logic inline in function expressions instead of forcing local helper functions just to use `yield`.
