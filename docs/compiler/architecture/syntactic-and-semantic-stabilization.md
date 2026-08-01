# Syntactic and semantic stabilization

Raven should stabilize the meaning of its language before the compiler is
ported to Raven. The current C# compiler will be the behavioral reference for
that port, so its syntax and semantic behavior must be dependable enough to
distinguish an intentional language rule from an implementation accident.

This is not only preparation for self-hosting. The current compiler is a
product in its own right and should provide a great command-line, build, and
editor experience. A future port is not a reason to defer correctness,
recovery, diagnostics, or responsiveness.

Stabilization does not mean preserving every current behavior or avoiding
breaking changes. Raven is still free to revise syntax and semantics. It means
that changes are deliberate, documented, consistently implemented, and covered
by tests across full and incremental compilation.

Familiarity and interoperability are the default when established C# and .NET
conventions carry observable meaning. Raven may intentionally diverge in source
semantics where that produces an idiomatic Raven model, but its .NET ABI must
preserve the platform convention so consumers do not need Raven-specific
knowledge to interpret a public contract.

The review criteria, current divergence inventory, and emitted-IL policy are
recorded in [.NET conformance, Raven divergences, and emitted
IL](dotnet-conformance-and-divergence.md). A mismatch should remain listed there
as a gap until it is fixed or earns an explicit Raven rationale with interop
evidence.

## What must be stable before a port

A compiler port should begin only after these properties hold for the language
surface needed to write the compiler:

- A full parse and an incremental parse of the same text produce equivalent
  syntax, trivia, missing tokens, and diagnostics.
- Incomplete and malformed source produces useful diagnostics without throwing,
  hanging, or silently abandoning later declarations.
- One-shot and incrementally updated compilations produce the same declarations,
  symbols, types, conversions, diagnostics, and operations.
- Semantic answers do not depend on whether diagnostics, hover, completion, or
  another semantic query happened first.
- Public semantic APIs agree with compiler diagnostics and the behavior used by
  lowering.
- A broken declaration does not invalidate unrelated declarations. This
  includes macro functions.
- Every accepted construct either binds and lowers according to a documented
  rule or produces a direct diagnostic.
- The core type, conversion, overload-resolution, flow-analysis, and pattern
  rules needed by compiler code have conformance tests.

These are correctness boundaries. Implementation language, internal class
layout, and the exact cache representation are not.

### Bootstrap soundness gates

Three semantic areas are explicit gates for porting compiler code:

1. **Overload resolution and generics.** Candidate construction, inference,
   constraints, conversion ranking, ambiguity, and selected symbols must agree
   across ordinary binding, public semantic queries, and incremental edits.
2. **Control-flow analysis.** Reachability, abrupt completion, joins, definite
   assignment, return analysis, and loop ownership must be conservative and
   internally consistent for every construct used by compiler code.
3. **Unified null-state analysis.** Reference and value nullability must use
   the same Raven semantic model while importing and emitting the correct .NET
   ABI annotations. Branches, loops, patterns, calls, and metadata flow
   attributes must publish the same state to diagnostics and `TypeInfo`.

A serious unresolved inconsistency in one of these areas blocks a trusted
bootstrap even if the affected program happens to emit runnable code.

## Bootstrap compiler and port boundary

The current C# compiler is the bootstrap compiler and must become a trusted
reference implementation before a Raven compiler port starts. The port must
not be the first compiler-shaped Raven workload. Raven-written libraries,
macros, tools, generators, and reduced binder/semantic workloads should first
exercise the compiler-writing subset and establish that the C# compiler can
compile it reliably.

Future port failures must be distinguishable between a bootstrap-compiler
miscompilation, an error in the Raven-authored compiler, and an unspecified
language rule. Keep deterministic compiler-shaped fixtures and, once a second
implementation exists, run differential tests over public diagnostics, symbol
and operation shapes, emitted metadata, and observable runtime behavior. Exact
internal lowering or instruction sequences are not the compatibility oracle.

## Project-system boundary

Workspace projects must remain constructible without MSBuild. The workspace
API owns the host-independent project, document, reference, option, and
diagnostic model; MSBuild is a production adapter over that model rather than
the model itself. Single-file execution, tests, custom hosts, and future project
systems must be able to provide equivalent inputs directly.

The MSBuild integration should nevertheless behave like normal .NET tooling:
standard targets and reference conventions, reliable incremental inputs and
outputs, design-time build support, and diagnostics that identify the owning
project and source. Add equivalence tests that load the same project through
MSBuild and direct workspace construction and compare documents, references,
options, target framework, and resulting diagnostics.

## Stabilization levels

### Crucial before porting

Fix behavior that could make the current compiler an unreliable reference:

1. Parser totality and recovery.
2. Full-versus-incremental syntax equivalence.
3. Declaration and symbol isolation after errors.
4. Query-order-independent semantic binding and caching.
5. Definite assignment, nullability, control flow, and return analysis.
6. Conversion and overload-resolution consistency.
7. Pattern binding and exhaustiveness across every supported type family.
8. Ordinary and macro-function declaration parity.
9. Removal of reachable `NotImplementedException` and generic exception paths
   from syntax and semantic APIs.

Nullable signature metadata now includes the conventional non-null context on
emitted source types, while explicit nullable positions retain their transform
flags. This preserves Raven's strict source model for both .NET reflection and
Raven metadata consumers. PE symbols now project flow-direction attributes on
method returns and parameters, including their typed constructor arguments.
Applying those contracts to Raven's call-site null-state transitions remains a
separate flow-analysis slice.

### Improve in the current compiler, without necessarily blocking a port

Some work makes this compiler substantially better but can proceed in parallel
with later porting:

- Replace whole-string editing and avoid substring allocation in source and
  parsing paths.
- Broaden incremental parsing beyond the most common node categories.
- Consolidate duplicated binder and constraint logic.
- Complete less frequently used public API conveniences.
- Improve cache granularity, memory use, cancellation, and concurrent query
  performance after correctness is locked down.
- Replace internal recovery fallbacks with clearer result types and telemetry
  where the fallback is already semantically correct.

The obsolete local table in `LocalScopeBinder` has been removed. It was never
populated with valid symbols and duplicated the lexical ownership already held
by `BlockBinder`; the remaining binder now serves only as a forwarding scope
boundary for semantic queries.

These items should not be ignored. They simply should not delay a port when
their behavior is specified, tested, and can be preserved or improved during
the port.

### Suitable for the port or after it

Changes tied primarily to implementation structure can be made while porting:

- Translating binder and lowering components into idiomatic Raven.
- Replacing C#-specific helpers with Raven-native abstractions.
- Rearranging internal types and pass boundaries without changing observable
  behavior.
- Porting generators after their generated contracts are stable.
- Replacing cache implementations while retaining the same snapshot and query
  semantics.

The port must not become the first test of what the language means.

### Idiomatic absence in a Raven-authored compiler

A Raven port should not mechanically preserve C#'s internal use of `null`.
Expected absence should normally use `Option`, recoverable failure should use
`Result`, and closed compiler states should use unions. This makes the compiler
exercise Raven's own flow, pattern, and exhaustiveness model and avoids carrying
ambiguous nullable state into Raven-native component boundaries.

The .NET boundary remains distinct. Metadata, reflection, and interoperable
public APIs should continue to honor platform nullability and ABI conventions,
including nullable references where the underlying .NET contract uses them.
Adapters at those boundaries can project a nullable platform result into an
idiomatic Raven carrier for internal use; they should not falsify the external
contract merely to eliminate `null` from its signature.

## Findings from the current implementation

The following are implementation findings, not conclusions drawn only from the
language specification.

### Code audit map

These are the first implementation seams to revisit. The list is not intended
to imply that every file must be rewritten before porting.

| Area | First places to inspect | Concern |
| --- | --- | --- |
| Incremental syntax | `Syntax/SyntaxTree.cs`, `Workspaces/InfoTypes/DocumentInfo.cs` | Fragment replacement, diagnostic preservation, and full-parse equivalence |
| Parser recovery | `Syntax/InternalSyntax/Parser/LanguageParser.cs`, `Syntax/InternalSyntax/Parser/Parsers/*` | Reachable throws, missing-token construction, recovery boundaries, and fragment coverage |
| Source snapshots | `Text/SourceText.cs`, `Text/TextLineCollection.cs` | Whole-string edits, substring readers, incomplete APIs, and snapshot identity |
| Contextual binding | `Binder/BlockBinder.cs` | Target-type-sensitive cache identity and query-order dependence |
| Semantic query orchestration | `SemanticModel.cs` | Specialized fast paths can bypass contextual binding and publish different symbols by query order |
| Declaration validation | `MemberSignatureDeclarationPass.cs`, `Binder/TypeParameterInitializer.cs`, declaration binders | Repeated initialization paths must share one idempotent validation result |
| Recovery during binding | `Binder/BlockBinder.Statements.cs`, `Binder/BlockBinder.MemberAccess.cs`, `Binder/MethodBodyBinder.cs` | Broad exception suppression and binder fallbacks |
| Public flow information | `TypeInfo.cs`, `Binder/BlockBinder.cs`, `Binder/BlockBinder.Statements.cs` | Agreement between nullability diagnostics, narrowing, and semantic APIs |
| Overload resolution | `OverloadResolver.cs`, `Binder/BlockBinder.MemberAccess.cs` | Generic method groups, inference, ambiguity, and conversion ranking |
| Pattern semantics | `BoundTree/BoundIsPatternExpression.cs`, pattern binding in `Binder/BlockBinder.cs`, `CodeGen/Generators/ExpressionGenerator.Patterns.cs` | Binding, flow, exhaustiveness, and lowering agreement |
| Symbol contracts | `Symbols/Constructed/*`, `Symbols/Source/SourceModuleSymbol.cs`, `Symbols/PE/PEModuleSymbol.cs` | Reachable incomplete lookup and construction APIs |
| Macro declarations | `Compilation.LocalMacros.cs`, macro-function declaration and binding paths | Signature/body isolation, diagnostics, and ordinary declaration parity |

### Incremental syntax diagnostics must remain equivalent

`SyntaxTree.WithChangedText` reparses a fragment and replaces the old node.
Incremental updates now retain diagnostics produced by that fragment parse,
discard stale diagnostics owned by the replaced syntax, and shift unaffected
diagnostics after the edit. Green-node replacement preserves unchanged
subtrees. This corrected earlier paths that attached an empty diagnostic set
and rebuilt unaffected siblings after a successful partial reparse.

The workspace uses `WithChangedText`, so equivalence remains a correctness
requirement even when an independent full-parse diagnostic lane could hide a
compiler defect in the editor. Incremental syntax tests now compare exact tree
shape and diagnostics with a full parse for representative valid, incomplete,
repair, shifted-diagnostic, and macro-function edits. The matrix should expand
alongside parser recovery coverage.

### Some malformed declarations can still throw

Constructor declarations with a missing block or expression body now produce a
missing block and a targeted diagnostic, then continue parsing later
declarations. This replaced a generic exception in the parameterized
constructor path and silent acceptance in the parameterless path.

Type-only declaration patterns now use the syntax model's optional designation
instead of manufacturing a designation containing `None` tokens. The remaining
parser exceptions are guarded construction and dispatch invariants; mutation
coverage exercises the relevant directive, operator, relational-pattern, and
macro boundaries so that no user edit reaches them.

### Target-sensitive binding uses contextual cache identity

Target-typed expression binding now caches every expression form by syntax and
target type. This replaces the manually enumerated syntax-kind allowlist, which
could omit wrappers or newly added expressions and make the first requested
target type affect later semantic answers.

Focused permutation tests bind the same parenthesized expression under
different target types in both orders. Additional binding context beyond target
type, such as expression-return policy, still needs an explicit identity audit.
Workspace coverage now also changes a collection expression's contextual array
element type from `int` to `long` and back, alternating cold and
diagnostics-first queries. The converted collection type must follow every
snapshot without retaining the previous target.

### Broad exception suppression can hide semantic differences

Ordinary and macro functions now have focused coverage proving that a broken
body retains its valid signature, does not invalidate a valid sibling, and
confines body diagnostics to the broken declaration. The ordinary-function
coverage includes an incremental workspace edit.

`if` expression branches now always use their dedicated local-scope binders;
the previous broad exception fallback to the enclosing binder could silently
change lookup and diagnostic behavior.

Missing-return, unreachable-code, and let-else validation now consume the
control-flow result directly instead of suppressing every exception and
silently dropping the secondary diagnostic. Body errors remain ordinary bound
error states, so they do not prevent the walker from reporting independent
flow diagnostics.

The control-flow walker now models unconditional `loop` statements,
literal-true `while` statements, reachable `break` exits, `unsafe` blocks,
`finally` execution, and exhaustive match statements with abrupt arms
explicitly. In particular, a completing `finally` preserves the completion
state of the associated `try` and `catch` clauses, while an abrupt `finally`
makes the whole statement abrupt. Match flow evaluates exhaustiveness from
already-bound match facts and stays conservative for missing coverage, guards,
and completing arms. Focused tests cover these rules through public control-flow
analysis, missing-return diagnostics, let-else validation, macro functions,
and bodies that already contain binding errors.

This is the first flow-semantics slice rather than a complete flow model.
Definite assignment, public nullability state, broader constant-expression
loops, nested expression-level abrupt flow, and broader join behavior remain to
be stabilized. Boolean logical negation is now folded by the shared constant
evaluator, so `while !false` has the same completion semantics as
`while true`.

### Public nullability information is becoming flow-sensitive

`TypeInfo` now preserves an expression's declared nullable annotation while
projecting the bound expression's current flow state. Strict null-check branches
and null guards therefore report `NotNull` through the public semantic model in
both cold and diagnostics-first query orders, matching the state already used
by binding and nullable-access diagnostics.

This contract is deliberately uniform across reference and value types. Both
`string?` and `int?` remain declared nullable symbols when narrowed, and both
publish the contextual `NotNull` fact through `Nullability.FlowState`. Their
different .NET runtime encodings must not leak into semantic analysis.
The same annotation and flow results are required whether diagnostics or type
information is requested first, and repeated semantic queries must preserve
both `Nullability` and `ConvertedNullability`.

The remaining conformance matrix needs broader joins, loops, richer pattern
tests, and incremental edits that change control flow. Nullable standard unions
now have cold and diagnostics-first coverage proving that a null guard preserves
the declared nullable union while publishing a non-null flow state for the
continuing return expression.

Branch-join coverage now distinguishes facts established on every completing
path from facts established on only one path. Ordinary `while` bodies also bind
under the condition's true-state nullability facts, while post-loop state stays
conservative when a `break` or outward `goto` can bypass the condition. Without
such an exit, normal completion projects the condition's false-state facts.
Break ownership follows the nearest loop, so a nested loop's break does not
erase facts from the enclosing loop. Cold queries bind the enclosing loop to
preserve the same context.

Loop back-edges now invalidate facts for mutable locals and parameters assigned
on a path that can continue iterating. `break` exits carry the null state at the
transfer point and multiple exits are intersected, while mutations followed by
an unconditional exit do not pollute an earlier body use. Assignment binding
uses the declared writable target rather than its flow-narrowed read shape; a
narrowed nullable local can therefore be assigned `null`, after which both
diagnostics and `TypeInfo` report it as maybe-null.

Exception regions use the same path-sensitive join rule. A `catch` starts from
the try-entry facts minus values that may have been assigned before an
exception, each catch is bound independently, and the completing try/catch
states are intersected before `finally` runs. This prevents a preceding catch
from seeding a sibling and ensures a mutation in any completing path or in the
finally block is visible after the statement.

Incremental workspace coverage changes a `while` condition between `is not
null` and `is null`, then restores it. Public `TypeInfo` flow state and
possible-null diagnostics must update together in all three snapshots.

Successful declaration patterns with a non-null declared type establish that
their scrutinee is non-null on the true path. Ordinary `if ... is` and dedicated
`while let` binding apply the same fact to their bodies, and cold semantic
queries bind those enclosing constructs before publishing `TypeInfo`.
Negation reverses any recognized pattern nullability fact rather than relying
on a special case for `not null`; consequently, an early-exit `is not T` guard
publishes the same non-null state on its continuing path as a positive typed
pattern publishes inside its body.
Property-pattern success also establishes a non-null scrutinee, matching the
runtime meaning of both typed property patterns and the empty `{ }` pattern.
Nominal deconstruction patterns establish the same fact before their
deconstruction method is invoked.
Pattern flow distinguishes adding, removing, and preserving a nullability fact.
A type or shape mismatch does not make an already non-null scrutinee nullable;
only a branch that proves a null match removes the fact.
For conjunctive patterns, a successful operand that requires a non-null input
establishes that fact for the successful combined pattern.
For disjunctive patterns, every successful alternative must require non-null
before the combined true path narrows; the false path may use a guarantee shared
by the required operand failures.
Sequence patterns follow the same recursive-pattern rule as property and
deconstruction patterns: nullable reference inputs are accepted, `null` does
not match, and success establishes non-null flow.
Dictionary patterns apply that rule to the underlying dictionary-compatible
type while retaining the nullable annotation on the scrutinee outside a
successful match.

Definite assignment joins only paths that can actually complete the construct.
Exhaustive match arms are intersected across their completing exits, and
non-exhaustive matches retain the unmodified incoming path. A `loop` has no
zero-iteration exit, so out-parameter assignment is now intersected across its
reachable `break` states rather than discarded wholesale; one unassigned break
continues to make the method exit invalid.

Null-flow joins treat a branch as abrupt when its block ends in `return`,
`throw`, `break`, or `continue`, even when ordinary statements precede that
exit. Nested `if`/`else` statements are abrupt when both branches are abrupt.
This allows a multi-statement null guard to narrow subsequent code without
weakening the conservative loop rules: a direct `break` path still prevents a
while-condition's false-state narrowing, while nested-loop breaks do not leak
into the outer loop.

The .NET boundary is an ABI contract rather than an implementation detail.
Raven must consume and emit the platform's nullable metadata conventions in
every relevant signature position, including nullable context/annotation
attributes, flow attributes such as `MaybeNull` and `NotNull`, generic type
arguments, arrays, and by-reference parameters and returns. Raven-authored
public APIs, including Raven.Core and Raven.Macros, are part of the same
contract and need metadata round-trip tests from both Raven and C# consumers.

Nullable annotation emission and import now use the .NET transform-flag walk
for nested generic arguments, arrays, generic value-type placeholders, and
by-reference positions. Both the uniform single-byte form and the positional
byte-array form round-trip through `NullabilityInfoContext` and Raven metadata
symbols. Metadata method returns and parameters also expose their flow
attributes through the public symbol APIs. Nullable context placement and the
remaining call-site interpretation of those flow contracts remain separate ABI
slices. Conditional `NotNullWhen` parameter contracts now narrow all annotated
arguments on the matching Boolean branch, including metadata methods with more
than one flow-annotated parameter; the opposite branch remains conservative.
`MaybeNull` return contracts likewise affect the call result's flow state
without changing its declared return annotation. This applies uniformly to
reference and generic results after construction, while a non-nullable value
type remains definitely non-null because its runtime representation cannot
carry `null`. Assignment and member-access diagnostics consume that flow view
rather than mutating the public method signature.

Imported `NotNullIfNotNull` return contracts now resolve their named parameter
and inspect the corresponding argument's Raven flow state. A non-null literal
or narrowed nullable argument produces a non-null invocation result, while a
maybe-null argument preserves the declared nullable result. Metadata-symbol and
call-site tests use a separately compiled C# fixture so attribute decoding and
semantic interpretation cannot accidentally pass through a source-only path.

By-reference metadata loading now reads an `out` parameter's write-state
nullability and falls back to the root reflection node when no separate element
node is available. Calls apply unconditional `NotNull` and `MaybeNull`
postconditions to the referenced local or parameter, including attributes on an
open generic `out T` after method construction. Cached invocations replay the
same transition so diagnostics-first and cold semantic queries agree.

### Control transfers have one expression-context policy

`return` and `throw` are useful non-completing expressions. Expression blocks
project their `return` and `throw` items as expression statements, and abrupt
paths do not contribute a value to type joins. Bare expression-form `return`
has an implicit `unit` payload.

`break` and `continue` remain statement-only loop control. They report
`RAV1902` and `RAV1903` from expression blocks even when a lexical loop exists.
Focused parser, semantic, macro-body, analyzer, and runtime tests cover the
projection, diagnostics, reachability, and lowering policy.

Abrupt-expression classification now unwraps parentheses and conversions and
recognizes an `if` expression as abrupt when both branches are abrupt. Control
flow applies that classification to local initializers, so a declaration whose
initializer returns on every path makes the enclosing endpoint unreachable in
both cold and diagnostics-first semantic queries.

### Local initialization and `out` assignment are distinct rules

Raven locals must be initialized where they are declared. An initializer-less
local reports `RAV0166`, so there is no intermediate local state for a separate
use-before-assignment diagnostic to analyze. The unused `RAV0165` descriptor was
removed rather than presenting a rule the compiler never reported.

Two declarations with the same name in one lexical scope report the binding
error `RAV0167`. This is distinct from shadowing a declaration in an enclosing
scope, which remains the configurable `RAV0168` warning. Treating a same-scope
duplicate as ordinary shadowing leaves lookup and symbol ownership ambiguous
and is therefore not suitable recovery for a bootstrap compiler.

`out` parameters are different because the caller supplies their storage. The
callee must assign each `out` parameter on every normal exit. The focused
conformance matrix covers straight-line exits, `if` joins, exhaustive and
non-exhaustive matches, return and throw arms, and terminating versus breakable
loops. Match joins use bound exhaustiveness facts, while proven non-terminating
loops reuse ordinary control-flow completion.

### Constructed and module symbol queries are total

Array and tuple symbols now answer nested-type and member queries through their
projected runtime types instead of throwing. Tuple symbols
also expose projected element names alongside the underlying `ValueTuple`
members and follow the ordinary non-generic `Construct()` contract. Source and
metadata modules project an assembly namespace to their own namespace
constituent by metadata path, returning `null` when that module has no such
constituent.

The remaining unsupported construction member is confined to the error-type
sentinel, which is not a constructible named type. New public symbol families
should receive the same no-throw query coverage as part of their introduction.

The obsolete anonymous alternative-type symbol family has since been removed.
Raven unions are nominal union symbols, including the `System.Union<...>` carriers used
by ad-hoc union syntax; branch inference never synthesizes a separate union type.
This follows the familiar tuple projection model: syntax maps to a standard
runtime generic type, and the semantic model exposes that constructed named type.

Bound-tree walkers now delegate expression and statement dispatch to the
generated visitor contract. This removes a manually maintained type allowlist
that silently ignored newer expressions and threw when the general `Visit`
entry point received an ordinary non-block statement. Walker traversal hooks
remain explicit, but dispatch completeness now evolves with the bound model.

Nullable symbol member queries now follow the same decorator rule as the rest
of unified nullability. `GetMembers` projects the underlying declared members
for nullable reference and value types alike, matching `LookupType` and
`IsMemberDefined`; CLR wrapper/base representation does not replace the Raven
semantic surface.

The diagnostic factory no longer carries the obsolete untyped
`MemberAccessOnUnit` overload. That overload survived removal of the associated
language restriction and could only throw; all diagnostic construction now
requires a descriptor.

The unused `TypeParameterConstraintCollector` prototype was also removed. Its
clause-name matcher was never implemented and no declaration path called it;
active declaration binders use the complete analyzer and pre-index constraint
clauses by `TypeParameter.Identifier.ValueText`.

### Generic overload resolution has explicit gaps

Overload resolution now constructs open generic method-group candidates when
their type arguments can be inferred from the target delegate parameters. The
constructed signature participates both in outer generic inference and in the
final delegate conversion, so calls such as `Apply(21, Identity)` consistently
resolve both methods. Conversion ranking still uses coarse fallback scores in
several cases, and the remaining generic inference forms need a conformance
matrix before they can be treated as stable language rules.

The method-group matrix now also covers a constrained generic transform passed
to a separately generic higher-order function. Inference constructs both
methods, preserves the transform's `struct` constraint, and publishes the same
selected symbols when diagnostics or symbol information is requested first.
An incompatible inferred transform now carries its structured constraint
failure through the outer overload resolution instead of silently accepting
the call. Workspace coverage edits the argument from a satisfying value type
to a violating reference type and back, requiring both diagnostics and the
contextually constructed method symbol to update.

Constraint clauses are declaration contracts rather than optional binder
hints. A clause naming an undeclared type parameter reports `RAV0360` for
namespace functions, type members, function expressions, and macro functions;
it is never silently discarded by whichever declaration path runs first.
Metadata methods whose constraints refer to a containing type parameter are
also covered at the constructed-symbol boundary. Both applicability and the
public method type-parameter constraint view substitute the containing type's
arguments, so `GenericContainer<object>.Coerce<string>` is accepted while the
inverse `GenericContainer<string>.Coerce<object>` is rejected.
On rejection, the invocation publishes that method as an overload-resolution
candidate and retains the constraint diagnostic; it does not degrade into an
ambiguity or a missing-name result. This keeps IDE inspection useful even when
an explicit constructed metadata call is invalid.
Workspace coverage now edits such a call from valid to constraint-invalid and
back. Diagnostics-first and symbol-first queries must both discard the stale
constructed method, publish the invalid candidate while broken, and restore
the newly constructed valid symbol after the edit.

Source-to-PE round-trip coverage now constructs the same generic type and
method on both sides of emit and requires equal hashes and symbols. This found
two projection inconsistencies rather than an overload bug: in-memory PE
modules could lose both their scope name and containing-symbol edge, and source
generic methods incorrectly placed documentation-ID double-backtick arity in
`MetadataName`. The CLI stores generic method arity in the signature, so Raven
now keeps `MetadataName` identical to the emitted method name.

The broader open-to-constructed symbol path remains a stabilization risk.
`ConstructedNamedTypeSymbol`, `SubstitutedMethodSymbol`, and
`ConstructedMethodSymbol` form a wrapper chain whose containing symbols, type
parameter owners, constraint substitutions, nullability, definitions, and
hashes must agree. Do not replace that architecture during a focused bug fix;
continue adding source/PE and repeated-construction invariants, then evaluate a
single substitution service or interned constructed-symbol factory as a
separate design slice.

Repeated source construction now has an explicit ownership invariant. Two
independent constructions of the same containing generic type may be distinct
objects, but their open and constructed methods must compare and hash equally.
Each substituted method type parameter remains owned by its own open method
wrapper, while constraints that mention containing type parameters resolve to
the constructed containing arguments.

Nested construction now has the corresponding source-to-PE invariant across
all three substitution layers. `Outer<int>.Inner<object>.Combine<string>` must
retain the constructed outer and inner containers, substitute the method
signature and its `V: U` constraint, and compare and hash equally after an
emit/reload boundary. PE name-indexed lookup normalizes a nested generic CLI
name such as `Inner`1` to the Raven-facing member name `Inner`; lazy lookup and
full member loading therefore expose the same symbol set.

This is an integration invariant, not only an internal symbol invariant. A
source-only test can pass while the same API fails as soon as it is packaged in
a library and consumed through metadata. Risky symbol-model coverage should
therefore use a source declaration, emit it, load it as a reference in a second
compilation, and repeat the public lookup and construction operations a normal
third-party consumer would perform.

Dependent method constraints now have that emitted-library coverage as well.
For a method constrained with `TDerived: TBase`, both fully explicit and fully
inferred valid calls select a constructed method, while the reversed type
arguments report the constraint failure. The diagnostics and semantic-query
paths agree: an invalid call has no selected symbol, reports
`OverloadResolutionFailure`, and retains the rejected method as a candidate.
Fast semantic queries must use the same constraint applicability check as full
overload resolution rather than publishing the only visible method solely
because lookup found one candidate.

Constraint filtering must also happen before conversion ranking without
creating a separate preference rule for generic methods. Given a constrained
generic overload and an `object` fallback, a value-type argument chooses the
generic identity conversion, while a reference-type argument that violates the
constraint chooses the fallback. This result is independent of declaration
order and whether diagnostics or symbol information is requested first. The
semantic fast path scores every remaining applicable candidate together; it
does not prefer a non-generic candidate before comparing conversions.

Loop null flow now includes the mutation side of its fixed-point approximation.
Before binding a repeated body, Raven removes entry narrowings for locals and
parameters assigned anywhere on that loop back-edge. A `while` condition then
applies its true-branch facts to this conservative header state, so a condition
such as `value is not null` still narrows every iteration. The same mutations
are removed from a possibly executing loop's exit state. This prevents both an
unconditional `loop` and an ordinary `while` from carrying a first-iteration
fact across an assignment to null.

Source named-type `MetadataName` currently includes its namespace and nesting
path while PE symbols expose the unqualified CLI member name. The equality
comparer now normalizes this known projection difference and separately checks
the containing symbol, which restores the equality/hash contract without
changing emission. A later API stabilization slice should choose one public
`MetadataName` contract and migrate code generation to
`ToFullyQualifiedMetadataName` where a complete CLI identity is actually
required.

One unsafe shortcut has been removed from that chain:
`ConstructedMethodSymbol.Equals(object)` and `GetHashCode()` no longer proxy to
the open definition. They use the same constructed signature identity as the
symbol comparer, preserving reflexivity and distinguishing different type
arguments without maintaining a second equality path.

Nullable parameter syntax is resolved in the declaration skeleton before
duplicate-signature checks. Reference nullability remains excluded from CLR
overload identity, but distinct underlying types remain distinct; a null
literal selects the more specific applicable reference parameter independently
of candidate or declaration order.

Available-state invocation lookup only publishes a result when it can select a
sound candidate. Ambiguous calls fall back to ordinary binding, and the bound
error retains its candidate set so `GetSymbolInfo` agrees with the ambiguity
diagnostic rather than reporting a generic overload-resolution failure.
Inapplicable overload sets likewise remain attached to the bound error for
language-service inspection.

Tests should cover inference, constraints, variance, extension methods, method
groups, lambdas, `null`, unions, user-defined conversions, and ambiguity. Each
test should assert the chosen symbol or diagnostic, not an internal lowering
shape.

Ad-hoc-union target typing must be tested with an available `System.Union<...>`
carrier. Carrier coverage verifies alternative construction in assignment,
argument, and return contexts. If no compatible carrier exists, binding reports
a missing runtime capability; it must not infer a common nominal type or revive
an anonymous alternative-type mechanism.

Raven.Core's current `System.Union<...>` definitions are compatibility shims.
If .NET standardizes compatible platform union types, Raven should prefer the
platform definitions and stop treating its own declarations as the canonical
ABI. Selection must be based on a validated well-known-type contract—supported
arities, construction surface, metadata shape, and runtime semantics—not only
the metadata name.

The assembly transition needs explicit compatibility handling because equal
metadata names in Raven.Core and the platform assembly are not equal CLR type
identities. Raven.Core should omit its definitions for target frameworks that
provide the platform types and, where binary compatibility requires it, use
type forwarding to the platform definitions. Tests must cover Raven and C#
consumers, metadata round trips, mixed old/new referenced assemblies, and each
supported target framework.

### Pattern semantics are a high-risk convergence point

Pattern parsing, binding, flow narrowing, exhaustiveness, code fixes, and
lowering all need to agree. Current bound-pattern evaluation still has
unimplemented pattern kinds and recovery hacks for constant patterns.

The matrix must distinguish at least:

- sealed class hierarchies;
- sealed record hierarchies;
- parameterized unions;
- unions with declared cases;
- enums;
- nullable and union types;
- tuple, property, list, relational, logical, and discard patterns.

Empty and incomplete matches must receive diagnostics inside ordinary and macro
function bodies. Adding a new closed-hierarchy member must invalidate the
appropriate exhaustiveness results.

Workspace-edit coverage now exercises that invalidation contract for source
enums, declared union cases, and explicit sealed-hierarchy permit lists. Each
family is checked across the exhaustive, newly non-exhaustive, and restored
snapshots, with diagnostics and `GetMatchExhaustiveness` required to agree.

Pattern binding also treats unsupported pattern node kinds as recoverable
semantic errors. Programmatically constructed or transient edited trees now
produce an invalid-term diagnostic and bound error pattern for unknown general,
comparison, unary, and binary forms rather than reaching a
`NotImplementedException`.

Literal binding follows the same totality rule. Unsupported token values and
programmatically constructed literal-expression kinds produce an invalid-term
diagnostic and bound error expression rather than a generic exception.
Interpolated-string binding also treats incomplete edited content as a normal
string-shaped recovery result and reports unknown constructed content rather
than throwing.

Pattern emission runs only after error diagnostics have prevented invalid bound
trees from reaching code generation. Its remaining totality checks therefore
name the unexpected bound pattern, designation, or operator kind as an internal
invariant violation; they are not user-facing claims that a valid Raven pattern
is unsupported.

The remaining root-binder function path is an ownership invariant, not an
unimplemented language feature. Function statements are bound by their
executable block binder; reaching the root now reports that invariant as an
internal invalid operation. Ordinary top-level, namespace-level, and nested
functions continue through block-owned declaration and body binding.

### Macro functions need declaration parity

Recent work has improved local macro partitioning and isolation, but macro
declarations still travel through specialized compilation and source-masking
paths. They should follow the same principles as ordinary declarations:

- recognize the declaration and signature even when its body is broken;
- publish body diagnostics at the declaration;
- preserve unrelated macro declarations;
- resolve invocations to the declaration without replacing the body error with
  a misleading “macro not found” error;
- provide the same hover, outline, completion, navigation, and edit recovery.

Special treatment should remain only where compile-time execution genuinely
requires it.

### Source text remains allocation-heavy

`SourceText` now implements its public copy, line, and write APIs. Line
collections are snapshot-owned and cached, preserve line-break spans, and do
not recreate line objects on repeated queries. Writes use spans rather than
creating substring values.

Edits still rebuild whole strings, and positioned readers still slice strings.
This is primarily a quality and performance lane now that the reachable public
surface is complete.

The eventual representation should support cheap snapshots and spans without
creating strings in normal compiler paths. Caching should be snapshot-owned,
bounded, and invalidated by source identity rather than hidden global state.

### Diagnostic rendering repeats formatting work

`Diagnostic` is immutable, but each `GetMessage()` call currently projects
symbol arguments to display strings, allocates a new argument array, and runs
composite formatting again. Diagnostics are rendered repeatedly by sorting,
equality, command-line output, and language-server publication.

Investigate lazy formatted-message caching or argument normalization at
construction time. Any change must be backed by allocation measurements over
diagnostic-heavy compilations and tests proving identical messages, equality,
and hash codes for null, symbol, and ordinary value arguments. Measure retained
memory as well as throughput: caching every formatted message may trade short
lived allocations for excessive snapshot retention.

## Required test strategy

### Syntax equivalence

For every syntax family, start from valid and invalid examples and apply
insertions, deletions, and replacements at token boundaries. Compare a full
parse with the incrementally updated tree:

- root text;
- node, token, and trivia kinds;
- spans and full spans;
- missing and skipped tokens;
- diagnostic IDs, arguments, locations, and order.

Mutation tests should be seeded from current Raven samples, including macros,
unions, generics, attributes, patterns, interpolated strings, directives, and
file-scope declarations. They must assert equivalence, not merely that parsing
does not throw.

### Semantic equivalence

Build the same program both from scratch and through workspace edits. Query it
in multiple orders:

1. diagnostics before semantic information;
2. type information before diagnostics;
3. symbol information from declaration then use, and use then declaration;
4. concurrent queries from different documents;
5. edits that break and then restore a declaration.

Compare declared symbols, symbol and type information, conversions, constant
values, operations, data-flow results, and diagnostics.

Coverage is risk-based rather than combinatorial. Every fix should select
representative paths that exercise distinct compiler ownership or projection
mechanisms, then add boundary cases where a failure would contaminate later
binding, flow, emission, metadata, or editor results. High-value seams include
source versus metadata symbols, open versus constructed generics, declaration
versus use sites, contextual versus uncontextual binding, full versus
incremental snapshots, valid-to-invalid-to-valid edits, and ordinary versus
macro declarations.

Public operations must describe source semantics rather than lowering
artifacts. Pattern-based `for` loops therefore expose their source pattern and
do not publish the synthetic iteration temporary as a user local. Ordinary and
pattern-based `while`, `for`, `loop`, and `match` forms have focused operation
coverage so analyzers see the same accepted control-flow families as binding.

### Error isolation

Each declaration family needs tests where its signature is valid and its body
is broken. Later declarations and uses must continue to bind. Repeat this for
top-level functions, methods, accessors, constructors, local functions, macro
functions, types, extensions, and union cases.

Namespace-level generic functions now have focused isolation coverage. A body
error does not prevent a valid sibling from being explicitly constructed, and
an invalid constraint clause naming a missing type parameter does not poison
sibling lookup. Both diagnostics-first and symbol-first queries keep the
selected constructed sibling and confine errors to the broken declaration.

### Behavioral conformance

Prefer observable language behavior and public compiler APIs over assertions
about internal bound nodes, lowering steps, or emitted instruction sequences.
Where the specification and implementation disagree, reduce the case and make
an explicit language decision before locking the test.

## Current risk register

The highest remaining risks after the current stabilization batch are:

1. **Open-to-constructed generic symbols (high)** — wrapper ownership and
   substitution are still distributed across named types, methods, parameters,
   constraints, and metadata projection. Nested source/PE construction now has
   a three-layer invariant, but the source/PE named-type `MetadataName` contract
   remains inconsistent. Continue boundary invariants and settle that API
   before considering a central construction redesign.
2. **Flow fixed points (high)** — branch, loop transfer, and ordinary
   try/catch/finally joins are covered, but the binder-owned non-null set is not
   yet a general control-flow fixed-point engine. Prioritize nested cycles,
   labeled transfers, and joins that mix abrupt and completing exceptional
   paths.
3. **Generic overload conformance (high)** — explicit and inferred constraints,
   higher-order method groups, metadata methods, candidates, and edit recovery
   have representative coverage; conversion ranking and less common inference
   forms still need a matrix.
4. **Unified nullability contracts (high)** — declared annotation and flow state
   now consume `MaybeNull`, `NotNullWhen`, `NotNullIfNotNull`, and unconditional
   by-reference postconditions without splitting reference and value semantics.
   `AllowNull`, `DisallowNull`, `MaybeNullWhen`, member postconditions, generic
   constraints, and Raven emit/C# consume round trips still need equivalent
   coverage.
5. **Incremental declaration isolation (high)** — ordinary and generic namespace
   functions have body/signature query-order coverage, but macro partitions and
   other declaration families remain less complete. Broken signatures and
   bodies must never contaminate unrelated declarations or replace local errors
   with invocation-site resolution failures.

These priorities describe correctness risk, not a request for a broad rewrite.
Each slice should keep using the smallest failing semantic boundary and a
public diagnostic, symbol, type, operation, metadata, or runtime assertion.

## Proposed order of work

1. **Incremental syntax correctness** — preserve or recompute diagnostics and
   add full-versus-incremental equivalence tests.
2. **Parser totality** — remove source-reachable throws and expand recovery
   mutation tests using current syntax.
3. **Semantic cache identity** — test query permutations and replace manual
   target-sensitive cache classification with a general model.
4. **Declaration isolation** — make broken ordinary and macro bodies retain
   valid signatures and localize diagnostics.
5. **Flow semantics** — settle definite assignment, nullability, returns,
   reachability, and public flow APIs.
6. **Conversions and overloads** — create a conformance matrix and resolve
   generic method-group and ambiguity gaps.
7. **Patterns and exhaustiveness** — align parsing, binding, flow, diagnostics,
   code fixes, and lowering for every closed family.
8. **Symbol API audit** — eliminate reachable incomplete contracts and
   centralize duplicated constraint logic.
9. **Source and cache performance** — adopt non-copying text snapshots and
   measure compiler and language-server edit workloads.

The first seven items define the dependable behavioral reference needed by a
port. The later items can overlap with port preparation when their observable
contracts are already established.

## Exit criteria for beginning the compiler port

The port can begin incrementally when:

- the compiler-writing subset has an explicit supported-feature inventory;
- the C# bootstrap compiler builds representative Raven-written
  compiler-shaped workloads deterministically;
- no source edit in that subset reaches a generic exception or an unimplemented
  semantic contract;
- full and incremental syntax and semantic equivalence suites pass;
- semantic results are independent of query order and concurrency;
- error recovery preserves unrelated declarations;
- flow, conversions, overloads, generics, and patterns have conformance
  coverage sufficient for compiler code;
- direct workspace construction and MSBuild loading agree on equivalent project
  inputs, while the workspace remains usable without MSBuild;
- remaining gaps are documented as intentionally out of the compiler-writing
  subset or safe to address during the port.

At that point, Raven-authored compiler components can be introduced one
boundary at a time and checked against the same suites. The existing compiler
continues to receive correctness, diagnostics, recovery, and performance
improvements throughout that process.
