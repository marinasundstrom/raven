# Pattern-first parameters

Status: coverage validation is implemented for existing function-expression
patterns, and tuple/sequence/nominal patterns now work on by-value named functions and
methods with bodies. Source and imported signatures now display these patterns through
`IParameterSymbol.BindingPattern`; `PatternParameterAttribute` preserves their
structure in referenced assemblies. Uniform syntax-model migration, additional
pattern forms, and richer editor interactions remain proposed.

TextMate evaluation: existing function-declaration rules identify the function
name without consuming its parameter list, and existing punctuation/rest rules
cover the implemented tuple/sequence spelling. No new lexical token is required.
Semantic signature help uses compiler display; generated incoming parameter
names are suppressed in argument-name inlays.

The proposed breaking change makes a source parameter consist of a binding
pattern and an input type. The pattern introduces bindings in the function's
scope; the type describes one incoming argument. Patterns do not expand the
method's ABI parameter list or participate in overload identity.

Proposed examples (not assertions of current compiler support):

```raven
func foo(value: int)
func foo(_: int)
func foo((x, _): (int, int))
func foo({ x, y }: Point)
```

## User-facing model: parameter deconstruction

The main benefit is making an incoming value's components available inside the
method without a separate deconstruction statement. Patterns supply a common
structural language, but invocation is unconditional binding, not case selection.
Signature display communicates the decomposition and component names. The input
type remains the caller's contract; refutable extraction belongs in the body.

## Decision: irrefutable parameter binding

Function and lambda parameter patterns must be irrefutable for their resolved
input types. Reject refutable patterns with a compiler error: ordinary
invocation has no pattern-failure result channel. There is no implicit throw,
neoCLR fault, filtering, default return, or synthesized `Option`/`Result` return
for a parameter mismatch. Code that needs conditional extraction binds the
complete input and explicitly handles the match inside the body.

Use one structural parameter model for ordinary deconstruction and nested
patterns. Accept a supported pattern when coverage analysis proves that it
matches every value admitted by the input type. Type-check first, including
lambda target inference, then analyze coverage recursively. Do not classify
whole syntax categories as refutable.

Maximize the patterns accepted in parameter position by reusing the general
pattern language and its semantic rules. Do not introduce a smaller whitelist
merely because a pattern form is often refutable. A form is admissible when it
is well-typed, establishes valid body bindings, and covers its complete input
type. Alternative patterns, for example, must still introduce compatible
bindings on every successful path. Necessary binding/type restrictions remain
distinct from refutability errors.

Coverage improvements should expand accepted parameter patterns without
requiring new parameter-specific syntax rules. Test positive and negative
instances of each applicable form; prioritize completing the analysis where an
existing form can be total rather than declaring the entire form unsupported.

## Refutability analysis and diagnostics

The spelling `Foo(let name)` alone does not make a pattern refutable. When
`Foo` is an ordinary deconstructable type and the input is non-nullable `Foo`,
this is ordinary typed deconstruction. The nested binding accepts any extracted
name, and the function retains one `Foo` input parameter.

Conversely, if the input is `object`, a base type admitting other values, or
`Foo?`, the same pattern can reject an input. `Some(let value)` against
`Option<int>` excludes `None`. Nested patterns must also be analyzed: a total
outer deconstruction does not make a refutable payload pattern total.

| Pattern | Input type | Intended result |
| --- | --- | --- |
| `Foo(let name)` | Non-nullable ordinary `Foo` | Accept when deconstruction and nested binding are total |
| `Foo(let name)` | `object` or `Foo?` | Error: other types/null are not covered |
| `Some(let value)` | `Option<int>` | Error: `None` is not covered |
| `(let x, _)` | `(int, int)` | Accept |
| `[let x, let y]` | Non-nullable fixed-length `int[2]` | Accept when the input's length contract guarantees two elements |
| `[let x, let y]` | `int[]` | Error: other lengths are not covered |
| `[let head, ..let tail]` | `int[]` | Error: the empty array is not covered |
| `[..let items]` | Non-nullable `int[]` | Accept when rest extraction covers every length |

These are semantic goals, not claims that every form is implemented today.
The same principle applies to nominal, case, property, and dictionary patterns:
resolve their symbols and analyze the values admitted by the input type and
nested patterns. Do not reject a case pattern solely because it is a case
pattern, or a sequence solely because its length is tested. In particular,
case-specific or single-case inputs are total only when the actual type contract
rules out every other state, including null or inactive representations.

Report an error on the parameter pattern, preferably highlighting the smallest
responsible refutable subpattern. Include an uncovered case or shape when
available. Suggested diagnostic:

> Parameter pattern is refutable for type 'Option<int>': 'None' is not covered.
> Bind the complete argument and handle the missing case in the function body.

Prefer one actionable coverage diagnostic per parameter rather than repeating
it on every enclosing pattern. Ordinary exceptions from a property getter or
`Deconstruct` method are not pattern mismatch and do not make a pattern
refutable. Non-nullable reference inputs follow the existing language type
contract; foreign code violating that contract is not an additional modeled
case for this analysis.

Run the analysis after input types are resolved and report diagnostics for the
selected binding rather than rejected speculative overload candidates. Do not
narrow a contextually supplied input type merely to make its pattern total.
Error or unresolved input types should defer coverage diagnostics to avoid
cascades. Incompatible and unsupported forms receive their own errors.

The analysis belongs in compiler binding/coverage code and should share rules
with match exhaustiveness checking. Audit `BlockBinder.IsTotalPattern` before
using it as the admission predicate: its current default `false` includes forms
it does not prove total, so it is not a complete refutability classifier.
Implement coverage for supported forms rather than turning that fallback into
blanket rejection. If proof is unavailable for an unsupported form, diagnose
that limitation accurately; do not assert an uncovered case without evidence.
The language server presents compiler diagnostics rather than classifying
pattern syntax independently.

Lower accepted patterns as argument extraction before the user body, without a
pattern-mismatch failure path. Preserve single evaluation of required getters
and deconstruction operations. Async/iterator extraction timing still needs a
specified relationship to the generated state machine, but there is no new
parameter-mismatch exception or runtime fault policy to implement.

## Current implementation

* `ParameterSyntax` still has an identifier token plus an optional pattern. The
  uniform syntax-model migration remains separate from these implementation slices.
* Named functions/methods with bodies and lambdas accept tuple, sequence, and
  nominal deconstruction. Short nominal lambdas work in calls such as
  `rows.Select(Row(let value) => value)`. Qualified and generic nominal heads use
  the existing type parser. Constructors keep their previous parameter grammar.
* Named and lambda body binders extract patterns from one incoming parameter
  before binding the body. Pattern names declare locals. Cold declaration queries
  go through the owning body binder; generated parameter names stay out of scope.
* `BlockBinder.ParameterPatterns` checks typed recursive coverage and reports
  `RAV1618` on the responsible refutable subpattern. Unsupported extraction forms
  separately report `RAV1619`; they must not reach an unsupported emitter branch.
* Nominal deconstruction uses existing `Deconstruct` binding and emission. It
  accepts total ordinary types and diagnoses nullable/narrowing inputs and
  refutable union cases. Calls preserve value-type copy behavior.
* Source and imported parameter symbols expose `BindingPattern`. Shared symbol
  display and LSP signature help render it beside the incoming type.
  `PatternParameterAttribute` preserves presentation across assembly boundaries.
* General property shorthand `{ x, y }`, nested property extraction, alternative
  extraction forms, and generic substitution inside explicit nominal type syntax
  remain unfinished. No syntax category should be called refutable merely
  because its extraction emitter is not implemented.

Validation covers modern .NET only; it is not evidence of execution on .NET
Framework, NanoFramework, or neoCLR.

## Proposed representation and ownership

Make `Pattern` the authoritative syntax for source binding parameters. An
identifier becomes a variable binding pattern and `_` becomes a discard
pattern. Keep input type annotations, attributes, defaults, and ABI modifiers
on the parameter. Do not encode a pattern in `IParameterSymbol.Name`.

Migration must explicitly handle type-only parameter forms and declarations
that use parameters to generate members. A missing recovery pattern must also
remain distinguishable from a user-written discard.

Retain one `IParameterSymbol` per incoming argument. Bind pattern-introduced
symbols in the owning function binder and expose them through authoritative
semantic APIs. A proposed API contract is:

* `GetDeclaredSymbol(parameterSyntax)` returns the incoming parameter symbol.
* `GetDeclaredSymbol(bindingDesignation)` returns the introduced symbol.
* `GetSymbolInfo` on a body reference returns that same introduced symbol.
* `GetTypeInfo` on a binding reports its extracted type; the parameter's type
  remains the complete input type.

There is an unresolved API choice for a simple identifier pattern: preserve
its `IParameterSymbol` identity, or introduce a distinct local as for structured
patterns. Uniform pattern syntax does not require an extra runtime local.
Preserving simple parameter identity is closer to Roslyn; making every binding
a local is a larger intentional API break affecting capture, rename, dataflow,
operations, and analyzers. By-reference aliasing must work under either model.

Do not build LSP-owned binding caches or syntax-scanning symbol substitutes.
Generic substitution must also preserve the relation between the incoming
parameter and its pattern when constructing method symbols.

## Display contract

| Surface | Proposed representation |
| --- | --- |
| Source declaration hover and signature help | `func foo((x, _): (int, int))` |
| Function type | `((int, int)) -> ()` |
| Hover on extracted `x` | Its local binding and extracted type |
| Active argument | One parameter range covering `(x, _): (int, int)` |
| Diagnostics | Pattern span or argument ordinal; no generated names |
| Imported ordinary .NET method | Its available parameter name and type |

Compiler display should own parameter rendering, with an explicit pattern
display option rather than overloading `IncludeName` with arbitrary text.
LSP signature help should consume that rendering. Pattern display should be
normalized and symbol-aware, not copied verbatim from source trivia.

Use offset ranges for signature-help parameter labels where practical: repeated
discards or identical patterns must still identify distinct argument positions.
Member names and introduced names in property patterns need separate navigation
and rename semantics, even when shorthand displays one token.

The CLI signature cannot reconstruct source patterns. Initially, pattern display
can be source-only with an honest metadata fallback. Cross-assembly pattern
display requires an explicit Raven metadata design and round-trip tests; it
must not be inferred from tuple element names or generated argument names.

## Semantic decisions before implementation

1. **Allowed binding forms.** Irrefutability is required as described above. Specify the
   binding-pattern grammar, including unparenthesized single-pattern lambdas,
   property shorthand, and supported logical/guarded pattern forms.
2. **Evaluation.** Define left-to-right parameter binding and single evaluation
   of property getters and deconstruction operations. Define when extraction
   runs in async and iterator methods, including the timing of ordinary
   getter/deconstruction exceptions.
3. **Call labels.** A conservative starting rule is that simple identifier
   patterns retain named arguments; structured and discard patterns are
   positional. Extracted names are never argument labels. Separate external
   labels would require their own syntax, semantic identity, and metadata rule.
4. **By-reference parameters.** A simple binding must preserve aliasing.
   Destructuring `ref`/`in` values needs an explicit copy-versus-alias rule;
   destructuring `out` cannot read the unassigned input. Initially diagnose
   unsupported combinations rather than silently copying or reading them.
5. **Scope and modifiers.** Define duplicate binding diagnostics across all
   parameters, nested binding mutability, attributes on the incoming slot,
   `params`, defaults, and lifetime rules for scoped/ref-like extracted values.
6. **Declaration kinds.** Specify support for functions, methods, lambdas,
   constructors, indexers, delegates, interface/abstract declarations,
   extension receivers, and macro parameters. Primary-constructor promotion
   and union payload names also define members and cannot be generalized by
   blindly replacing their identifier accessors.
7. **Signature compatibility.** Patterns must not distinguish overloads with
   identical input types and modifiers. Define how pattern presentation relates
   to interfaces and overrides; destructuring belongs to the implementation.

## Implementation slices

1. Settle the semantic decisions above and write proposed grammar. Migrate the
   syntax model, generated factories, both parameter parsers, simple lambdas,
   and parser recovery. Inventory all `ParameterSyntax.Identifier` consumers.
2. Introduce shared parameter-pattern binding for lambdas and named functions.
   First prove identifier, discard, and tuple behavior, public semantic APIs,
   duplicate names, target typing, and incremental edit invalidation. Add
   type-aware coverage analysis and parameter errors, accepting total
   ordinary nominal deconstruction and recursively checking nested patterns.
3. Add property shorthand and binding for supported forms with shared coverage
   rules. Prove that total fixed-length/rest-only sequence patterns are accepted
   while refutable union, length, key, null, and nested tests are rejected. Apply
   the rule to existing lambda patterns as part of the breaking change.
4. Centralize display and update signature help, hover, completion, inlays,
   navigation, rename, unused-binding analysis, and TextMate grammar coverage.
5. Validate ABI shape, optional/named calls, generic substitution, closures,
   async/iterators, and permitted by-reference combinations. Update language
   specs, grammar, compiler/API documentation, runtime-contract documentation
   where relevant, and the changelog for implemented behavior.

Before code edits, run the full baseline: this migration changes generated
syntax and crosses compiler layers, both triggers in
`docs/testing/test-impact-map.md`. Regenerate/build with
`scripts/codex-build.sh` after model changes. During implementation, use focused
parameter parser and semantic tests plus the `patterns`, `functions-async`, and
`overload-resolution` suites as each layer changes. Run focused emitted metadata
and observable runtime tests separately; do not assert opcode sequences.

Important regressions include cold `GetDeclaredSymbol` on pattern bindings,
renaming a binding without changing the input type, switching an identifier to
a tuple pattern during editing, repeated discards, one tuple argument versus
two arguments, and source-versus-imported display behavior. Verify both accepted
and rejected patterns within each supported syntax category, fixed versus
unknown lengths, rest-only sequences, nullable inputs, and nested coverage.
Diagnostic tests must distinguish `Foo(let name)` on `Foo` from the same pattern
on broader/nullable inputs, verify uncovered-case messages and nested locations,
and ensure speculative binding and cold semantic queries do not duplicate or
lose diagnostics. Runtime tests cover extraction values, getter evaluation
counts, and async/iterator extraction timing for accepted patterns.
