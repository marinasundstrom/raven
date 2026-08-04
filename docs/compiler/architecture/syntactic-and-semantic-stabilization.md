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

> **Current nullability decision:** Raven no longer performs nullable-specific
> flow refinement. A nullable expression remains `T?` until an explicit pattern,
> conversion, conditional access, or suppression produces another static type.
> Earlier null-flow entries in the chronological progress record below document
> superseded implementation work; they are not the current language contract.

Familiarity and interoperability are the default when established C# and .NET
conventions carry observable meaning. Raven may intentionally diverge in source
semantics where that produces an idiomatic Raven model, but its .NET ABI must
preserve the platform convention so consumers do not need Raven-specific
knowledge to interpret a public contract.

That ABI commitment does not delegate ownership of the type system to C#.
Raven's semantic types, source projections, patterns, and analysis rules form a
Raven-native model whose runtime representations remain compatible with the
CLR. Stabilization must test both sides of that boundary: the Raven meaning
must be internally consistent, and its imported/emitted metadata projection
must be conventional. Unified nullability is a defining example of this split.

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
3. **Unified nullability contracts.** Reference and value nullability must use
   the same Raven semantic model while importing and emitting the correct .NET
   ABI annotations. Static `T?` identity, conversions, dereference checks,
   generic substitution, and explicit pattern bindings must agree across source
   and PE symbols and through `TypeInfo`.

Nullability is primarily a type-system and .NET-boundary soundness gate, not
the preferred domain model for compiler code. Raven code should normally
eliminate nullable states with patterns and project meaningful absence or
failure into `Option`, `Result`, or a domain union. Stabilization therefore
prioritizes sound imported/emitted contracts, a single static semantic answer,
and explicit unwrapping paths. It must not reintroduce contextual null-state
caches in binders, public semantic APIs, or the language server.

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

### Porting phases and defect feedback

The initial Raven implementation is a port of the C# compiler, not an
independent redesign. Its first objective is trustworthy behavioral parity at
stable component boundaries. That does not mean mechanically translating
known defects, severe structural problems, accidental coupling, or recovery
paths that have already proved unsound. Keep a porting ledger that identifies
which behavior is contractual, which implementation detail is temporary, and
which known defect must be removed rather than inherited.

Use the following feedback rule whenever porting exposes a problem:

- If the C# bootstrap compiler is wrong, reduce the failure, add a focused test,
  and fix the C# implementation first. Port the corrected behavior rather than
  teaching the Raven implementation to agree with a bug.
- If the language rule or public compiler contract is unclear, settle and
  document it, then make both implementations agree with that decision.
- If only the Raven implementation is wrong, keep the C# result as the oracle
  and add differential coverage before correcting the port.
- If the problem is structural, define the boundary and invariants that should
  replace it. Migrate the C# implementation when its structure affects
  correctness or makes it an unreliable oracle; otherwise avoid reproducing
  the structure in Raven while retaining equivalent public behavior.

Port one coherent component or boundary at a time. Bring its focused tests
with it, including diagnostics, symbols, operations, metadata shape, and
observable runtime behavior as applicable. Run the C# and Raven
implementations against the same fixtures whenever both can execute the
boundary. Tests are part of the ported contract, not a final activity after
all compiler code has moved. Port Raven-authored test programs and test
infrastructure gradually where doing so does not remove the independent C#
oracle; preserve shared inputs and expected results so the two compilers can
still be compared directly.

Components should be replaceable gradually behind explicit interfaces or
data contracts. If a subsystem such as code generation can consume the stable
bound or lowered representation independently, introduce the Raven version as
an alternative implementation and compare it before making it the default.
The same approach applies to parsers, analyzers, metadata readers, lowering,
and other separable passes when their inputs and outputs are sufficiently
stable. Do not require a flag-day rewrite of the entire compiler.

During this parity phase, prefer direct and reviewable translations. Local
idiomatic Raven constructs are welcome when they are obviously equivalent,
well tested, and reduce risk, but broad cleanup or architectural invention
should remain limited. After the Raven compiler is stable enough to compile
itself and its results agree with the C# oracle, begin a distinct idiomatic
cleanup phase. That phase can reshape internal APIs, replace expected absence
with `Option`, use `Result` for recoverable failure, model closed states with
unions, and simplify code around Raven-native language features without
obscuring whether an earlier mismatch was a porting error.

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

- Translating binder and lowering components with behavior-preserving Raven
  structure and only small, clearly equivalent idiomatic improvements.
- Replacing C#-specific helpers with Raven-native abstractions when the
  replacement is local, test-backed, and does not obscure parity.
- Rearranging internal types and pass boundaries without changing observable
  behavior.
- Porting generators after their generated contracts are stable.
- Replacing cache implementations while retaining the same snapshot and query
  semantics.

Broader idiomatic cleanup belongs after the corresponding Raven component has
reached parity and remained stable. The port must not become the first test of
what the language means, and cleanup must not make parity failures difficult
to attribute.

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
| Public type information | `TypeInfo.cs`, `Binder/BlockBinder.cs`, `Binder/BlockBinder.Statements.cs` | Agreement between static nullability diagnostics, explicit pattern bindings, and semantic APIs |
| Overload resolution | `OverloadResolver.cs`, `Binder/BlockBinder.MemberAccess.cs` | Generic method groups, inference, ambiguity, and conversion ranking |
| Pattern semantics | `BoundTree/BoundIsPatternExpression.cs`, pattern binding in `Binder/BlockBinder.cs`, `CodeGen/Generators/ExpressionGenerator.Patterns.cs` | Binding, flow, exhaustiveness, and lowering agreement |
| Symbol contracts | `Symbols/Constructed/*`, `Symbols/Source/SourceModuleSymbol.cs`, `Symbols/PE/PEModuleSymbol.cs` | Reachable incomplete lookup and construction APIs; duplicated structural substitution across constructed wrappers |
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

Semantic recovery now also treats object-initializer syntax as a queryable
expression boundary. Token deletion can temporarily detach an initializer from
an object construction; `GetTypeInfo` and `GetSymbolInfo` return an error result
instead of reaching the binder's unsupported-expression invariant. When the
initializer is attached to a valid construction, its type information projects
the constructed type and its symbol information remains empty. A focused
token-deletion matrix covers both diagnostics-first binding and direct semantic
queries across representative flow and pattern syntax.

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

Macro isolation also includes declaration-level generic errors. An unknown
type parameter in one macro's constraint clause leaves both authored macro
symbols queryable, keeps the broken macro invocation resolved to its
declaration, and permits a valid sibling to compile and expand. The constraint
diagnostic is published without a downstream `RAVM010`, independent of whether
diagnostics or semantic symbols are requested first.

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

### Removed nullable-flow experiment

Raven no longer carries or publishes nullable-specific flow state. The former
implementation history is available in version control, but it is not part of
the stabilization contract. A nullable receiver remains `T?` after every direct
null check. Only an explicit pattern binding, conversion, conditional-access
result, or suppression expression produces a different static type. See
[Nullability and absence](../../lang/nullability.md).

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

Dependent source constraints now have the same acceptance/rejection invariant
as their emitted-metadata counterparts: `TDerived: TBase` accepts
`<Base, Derived>`, rejects the reversed construction with the constrained method
as a candidate, and is independent of diagnostics-versus-symbol query order.

Lexical function lookup now retains an overload set per name rather than a
single last-declared function. This matters at the higher-order boundary:
generic and non-generic namespace-function overloads passed as a method group
remain visible together, and the compatible generic transform is selected for
`Apply(21, Convert)` in either declaration order. Namespace-member promotion
uses signature identity for the same reason instead of collapsing overloads
through broad symbol equality.

Repeated inferences for the same method type parameter now have an
argument-order invariant. Given a base value and a derived value,
`Choose<T>(T, T)` widens the inferred bound to the base type regardless of
which argument appears first; it no longer keeps a derived first bound and
then rejects the base argument during applicability. The same rule applies
when Raven's partial explicit type-argument form fixes trailing parameters:
ordinary namespace-function calls preserve the open method until invocation
resolution combines those fixed arguments with the inferred leading bounds.
Explicit arguments do not live in the mutable inference map, so widening an
inferred bound cannot rewrite a call-site type argument.

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

PE and reflection loading require a dedicated boundary-consistency slice.
Every loader boundary must return the same compilation-owned semantic symbol
for the same CLI type whether it arrived from a reference facade, a runtime
implementation assembly, a forwarded signature, or a dependent library.
That audit must cover definitions and constructed forms, source/PE parity,
equality and hashing, generic parameter ownership, type forwarding, and
re-entrant member loading. Fix inconsistencies in the loader or symbol factory
that introduces them; extension lookup, overload resolution, workspaces, the
language server, and hosts such as the playground must not compensate for
split symbol identities.

The first contained invariant covers framework collection contracts. A
reflection signature resolving `IEnumerable<T>` through
`System.Private.CoreLib` and an implemented interface resolving it through the
`System.Runtime` facade now share the compilation's canonical collection
definition. This restores Raven Core extension lookup in mixed reference
universes. Canonicalizing every framework special type is intentionally left
to the dedicated slice because the remaining definitions have not yet been
audited for forwarded arity, ownership, construction, and identity behavior.

Nested inference now descends structurally through nullable, array, tuple, and
constructed named-type layers. Raven tuple symbols are source projections over
`System.ValueTuple`; substitution preserves that public projection, while
metadata identity treats the projection and its underlying runtime type as the
same ABI shape. Source and emitted-library tests require identical inference
for a type parameter nested in both `T?[]` and another constructed generic
argument inside a tuple. The available-state semantic path also refuses to
drop a generic candidate it cannot fully construct: it falls back to
authoritative binding instead of allowing a weaker non-generic overload to win
because of incomplete cached state.

This fix deliberately centralizes tuple reconstruction in `TypeSubstitution`,
but structural substitution is still duplicated by constructed named types,
constructed methods, overload inference, and semantic fast paths. Converging
those implementations onto one cycle-safe structural substitution service is
a prioritized symbol-model simplification. It must retain source/PE parity,
tuple element names, nullable wrappers, array rank and fixed length, ref/address
wrappers, nested containing-type re-anchoring, and type-parameter ownership.
The existing integration matrix is the safety boundary for that refactoring.
Structural substitution now uses child identity—not broad semantic equality—to
decide whether an immutable parent must be rebuilt. This matters for source
tuple projections: a tuple can remain ABI-equal to its original shape while
its projected element types have changed from method parameters to inferred
arguments. Tuple reconstruction also consumes the public field contract and
carries the substituted element type explicitly rather than requiring one
particular constructed-field wrapper used by only some symbol sources.

Repeated source construction now has an explicit ownership invariant. Two
independent constructions of the same containing generic type may be distinct
objects, but their open and constructed methods must compare and hash equally.
Each substituted method type parameter remains owned by its own open method
wrapper, while constraints that mention containing type parameters resolve to
the constructed containing arguments.

Incremental declaration isolation now covers generic members inside generic
source types as well as namespace functions. Editing one member body from valid
code to an unresolved name must leave a sibling's constructed containing type,
method type argument, and original declaration identity available in either
diagnostics-first or semantic-query-first order. The authored error remains
confined to the edited member body; it does not invalidate the constructed
sibling call.

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

Input contracts are kept separate from those declared/read types. Ordinary
non-nullable parameters reject null and nullable arguments, `AllowNull`
permits such input without making reads nullable, and `DisallowNull` rejects it
even when the declaration type is nullable. Property assignment follows the
same rule using the setter value parameter, which is where C# emits these
attributes. PE properties now project their own metadata attributes as well;
accessor-parameter contracts remain owned by the corresponding method symbol.

Source and PE named types now share the Roslyn-like `MetadataName` contract: it
is the local CLI name, including generic arity but excluding namespace and
containing-type paths. `ToFullyQualifiedMetadataName` composes the complete
identity. Top-level code generation asks for that complete identity explicitly,
while nested types continue through `DefineNestedType`; public API consistency
therefore no longer depends on code generation interpreting a source-only name
shape.

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

### Lambda capture ownership needs one authoritative model

Target-typed lambda replay and Result propagation recently exposed a split
capture model: a lowered namespace function could retain an outer-parameter
access in a lambda body while the lambda's recorded capture set omitted that
parameter. Emission now contains this case by materializing the missing closure
field from the actual parameter access and copying the canonical outer argument.

Allocate a dedicated stabilization slice to inspect the relevant closure,
lambda-replay, canonical-method-symbol, and propagation-lowering changes from
the preceding 150 commits. Determine whether binder capture analysis should be
the single authoritative result consumed by the semantic model, lowering, and
emission, or whether the capture representation needs replacement. The end
state should remove reconciliation paths where possible and cover ordinary,
namespace, local, async, iterator, nested, and propagation-containing lambdas
with source/runtime behavior tests.

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

Nullable matching now composes the `null` case with the complete non-null
domain rather than replacing that domain with one opaque underlying-type case.
Focused coverage includes scalars, booleans, enums, sealed hierarchies, open
hierarchies with base-type or discard fallbacks, match statements, and the
missing-case code fix. A nullable sealed hierarchy therefore reports its
specific missing leaves followed by `null`, while an open hierarchy reports `_`
until a base-type or discard fallback is present. The same complete and missing
case sets are now required from source and emitted-metadata hierarchies in both
diagnostics-first and semantic-query-first order. Incremental edits that add a
permitted leaf invalidate the nullable match immediately, report only that new
leaf when `null` was already handled, and restore the original exhaustive state
without retaining stale diagnostics.
The same query-order matrix preserves the scrutinee's declared nullable symbol
while publishing non-null leaf types for each explicit typed arm binding, for
both source and PE hierarchy symbols.

Nested declared-union patterns now have a corresponding convergence invariant.
An outer payload case whose inner domain is covered by a logical `or` pattern
must be exhaustive both while the union declarations are in source and after
their assembly is emitted and reloaded, independent of diagnostic-query order.
This checks case ownership, nested payload projection, logical-pattern coverage,
and metadata reconstruction together instead of testing each layer in isolation.

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

Accessor and constructor bodies have the same incremental invariant. Editing
either body to contain an unresolved name preserves its declared signature and
a sibling method's declaration and call-site resolution, regardless of whether
diagnostics or semantic queries force binding first. Diagnostics remain within
the edited declaration instead of contaminating the containing type.

Macro functions now carry that invariant through a workspace edit as well. An
invalid match introduced into one macro body publishes its authored diagnostic,
the macro invocation still resolves to the recognized declaration, and a valid
sibling macro continues to expand. This is covered in both diagnostics-first
and semantic-query-first request order so editor feedback cannot depend on
which language-service request happened first.

The isolation boundary also holds across documents. Editing a generic function
body to an unresolved name in one file preserves that declaration's generic
signature and leaves a constructed call to a valid generic function in another
file fully resolved. Diagnostics remain attached to the edited tree in both
diagnostics-first and semantic-query-first order.
The same isolation now covers overload siblings in one document: introducing a
body error into a generic overload preserves a valid non-generic overload and
its selected call symbol, while diagnostics remain confined to the edited
declaration.

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
   a three-layer invariant and a shared local `MetadataName` contract.
   Constructed methods also preserve their own type-parameter ownership while
   substituting a containing type argument inside nested generic constraints
   such as `IEnumerable<T>`. Equivalent source and emitted-metadata constructed
   types now have equality and hash coverage across constructors, parameters,
   properties, accessors, and generic methods. Continue boundary invariants
   before considering a central construction redesign.

Constructed properties, indexers, and events now form a self-consistent member
graph on both sides of emit. Their accessor methods associate with the
substituted property or event rather than the open definition, accessor and
ordinary method parameters are owned by the substituted method, and repeated
accessor queries return the same immutable projection. Generic property,
indexer, event, accessor, constructor, and ordinary-method types compare and
hash equally after Raven source is emitted and consumed through PE symbols.

Nested generic construction follows a definition/view split: an inner type
parameter remains owned by its generic definition, while a lookup through a
constructed outer type reanchors the inner view and substitutes outer and inner
arguments into its members independently. Tests assert both halves so future
construction work does not confuse definition identity with projected member
ownership.
Constraint views now follow the same rule. An inner parameter declared as
`TInner : IBox<TOuter>` remains owned by the inner definition, but a lookup
through `Outer<string>.Inner<TInner>` publishes `IBox<string>`. Constructor
binding validates the final constructed type as a backstop for member-access
paths that defer generic construction; consequently an invalid nested
construction is rejected consistently from source and emitted metadata.
Independent reconstruction of the same outer, nested, and method layers now
also has an equality/hash invariant, including the final method's containing
type. This catches substitution views that look correct but lose stable symbol
identity when rebuilt through a separate lookup path.

Source and emitted-library invocation now also agree on successful nested
construction and on rejecting a method constraint such as
`TValue: IEnumerable<TInner>`, including diagnostics-first and symbol-first
queries. Rejected candidates retain both constructed containing-type layers in
source and metadata. The source fix belongs to lazy declaration reconstruction:
refreshing an inner type's member signatures must reanchor the refreshed type
to its already-constructed outer receiver instead of manufacturing a shallow
`Inner<TInner>` view. Public constructor type information likewise reports that
constructed receiver rather than the constructor's `unit` return type.
Inferred calls rejected by a generic constraint now retain the constructed
candidate for public semantic queries, including its inferred arguments,
return type, and projected nullable parameter. Source `T?` and metadata
`System.Nullable<T>` normalize to the same Raven nullable shape during
inference and candidate construction.
Inference through a nullable constructed value parameter such as `Box<T>?`
also agrees across source and emitted metadata, retaining Raven's nullable
wrapper around the constructed `Box<string>` view in either query order.

The constructed named-type wrapper still exposes a category inconsistency:
every constructed named type implements both `IUnionSymbol` and
`IUnionCaseTypeSymbol`, even when its definition is an ordinary class. Internal
helpers compensate by checking `IsUnion` and `IsUnionCase`, but a compiler API
consumer can reasonably interpret the interfaces themselves as category
markers. A focused subtype-splitting experiment could not yet isolate that
change because the current Raven Core source fixture already fails around
generic `Option`/`Result` case projection. Restore and lock that integration
baseline before changing the wrapper categories. The eventual fix must then
route all named-type construction and nested reanchoring through a
category-preserving factory, cover ordinary types and generic unions from both
source and PE symbols, and only then remove the union interfaces from the
ordinary constructed wrapper. Until that migration is complete, compiler code
must use `TryGetUnion()` and `TryGetUnionCase()` rather than raw interface
tests.
2. **Flow fixed points (high)** — branch, loop transfer, and ordinary
   try/catch/finally joins are covered, but the binder-owned non-null set is not
   yet a general control-flow fixed-point engine. Labeled loop transfers now
   share the ordinary loop-owned state, and abrupt `try`/`catch` branches are
   excluded from normal joins. A completing `finally` is applied after the
   exceptional join and can establish a definite non-null fact independent of
   diagnostics query order. Nested labeled `continue` transfers now contribute
   to the target outer loop's back edge, and a filtered catch plus its fallback
   catch join to the repaired post-state in either query order. A `continue`
   inside a `try` also contributes its mutation to the enclosing loop's back
   edge despite a sibling catch exit, in either query order. A completing
   `finally` is applied before that transfer reaches the back edge, so its
   mutations affect the next iteration as required. Conversely, an abrupt
   `finally` replaces the pending transfer and prevents mutations in the
   abandoned path from weakening a nonexistent next iteration. The public
   reachability analysis independently recognizes that replacement and marks
   code following the resulting non-completing loop unreachable. A nested
   labeled `continue` passing through `finally` now has an explicit outer-loop
   invariant as well: mutations in the finalizer reach the target loop's back
   edge in either semantic-query order. Continue with mixed loop/exception
   cycles rather than rechecking the covered single-cycle forms.

Abrupt expressions must remain abrupt through every eagerly evaluated wrapper,
not only when they are the direct initializer or statement expression. Public
reachability now follows nested `return`/`throw` expressions through invocation
arguments, unary operations, receivers, indexing, assignments, and object
construction, and reports nested return nodes in `ControlFlowAnalysis`. Lazy
operators and conditional access retain their branch-sensitive rules; an abrupt
operand makes the whole expression abrupt only when that operand must execute.
3. **Generic overload conformance (high)** — explicit and inferred constraints,
   higher-order method groups, metadata methods, candidates, and edit recovery
   have representative coverage. Equivalent constructed signatures now apply
   the standard non-generic-over-generic tie-breaker independent of declaration
   order for both direct invocation and method-group conversion. The latter is
   covered in diagnostics-first and symbol-first query orders; conversion
   ranking and less common inference forms still need a matrix. Numeric ranking
   now also has a three-candidate transitivity invariant: an `int` argument
   selects `long` over both `float` and `double`, independent of declaration and
   semantic-query order. Generic inference independently extracts multiple type
   arguments from a constructed metadata parameter such as
   `Dictionary<TKey, TValue>`, also in both query orders. Dependent constraints
   loaded from emitted metadata are likewise checked consistently whether
   diagnostics or the selected constructed method is requested first.
   Raven's partially explicit generic calls also infer leading type parameters
   through nested constructed arguments while right-aligning the explicit
   arguments, in either query order.
   Higher-order inference now has a source/PE parity invariant as well: a
   generic static method group passed to a generic `Func<TInput, TResult>`
   consumer constructs both methods with the same inferred input and result
   types before and after the provider assembly is emitted, independent of
   whether diagnostics or symbol information is requested first.
   Generic by-reference calls now infer from the referenced storage element
   rather than constructing illegal address-shaped type arguments. Source and
   metadata `ref T`, `out T`, and `in T` methods agree in both query orders,
   while ordinary applicability retains exact by-reference-kind validation.
   Lifted numeric conversions use that same ranking under Raven's nullable
   wrapper: `int?` selects `long?` over `double?`, independent of declaration
   and query order. Nullable wrapping does not introduce a second ranking
   policy.
   User-defined conversion classification now compares every applicable
   candidate on its standard source and target legs. Exact source and exact
   target operators win independently of declaration and semantic-query order;
   candidates that improve conflicting legs remain ambiguous instead of being
   selected by member enumeration order. Exact implicit conversions retain a
   bounded fast path so deterministic selection does not turn global extension
   conversion discovery into a build-time penalty.
4. **Unified nullability contracts (high)** — declared annotations are the
   semantic truth for both reference and value types. Imported `MaybeNull`
   returns project to a static nullable result, while `AllowNull` and
   `DisallowNull` affect input contracts. Conditional .NET flow attributes do
   not refine Raven storage. Generic type and method
   parameters now preserve `class`, `struct`, `new()`, `notnull`, base-class,
   and interface constraints across Raven source and emitted metadata. PE
   loading recognizes Raven's standard `NullableAttribute(1)` encoding for an
   otherwise unconstrained `notnull` parameter instead of dropping that
   semantic constraint. Dependent and nested constraint shapes still need
   equivalent construction coverage. Source and PE projections must yield the
   same static type without query-order-dependent nullability state.

Raven-emitted nullable generic contracts now have an explicit round-trip
invariant. For a reference-constrained `T`, nullable parameter and return uses
are observed as nullable by .NET reflection, reload as nullable type-parameter
uses through PE symbols, and remain nullable after constructing the containing
type with `string`. This covers annotation emission, metadata loading, generic
ownership, and substitution at the interop seam without expanding flow rules.

Generic variance has the same source/ABI parity requirement. Raven now emits
the CLR covariance and contravariance flags for `out` and `in` interface type
parameters, and tests apply the corresponding interface conversions before and
after the Raven assembly is reloaded through PE symbols. A variance rule is not
stable if it works only while the declaration remains in source.

Explicit postfix null suppression has its own semantic identity. It removes the
nullable annotation from the expression's public `TypeInfo` and from an
inferred local without pretending that branch analysis established a reusable
flow fact. This distinction prevents `value!` from changing the declared symbol
or leaking a narrowing fact to later reads of `value`.

Conditional access performs an expression-local receiver conversion from `T?`
to `T`, so extension receiver applicability sees `T` only inside the `?.` path
while the overall expression remains nullable. The original receiver storage
is not refined.
5. **Incremental declaration isolation (high)** — ordinary and generic namespace
   functions have body/signature query-order coverage. Accessors, constructors,
   generic members, overloads, and field initializers now preserve sibling
   resolution after body or initializer edits. Event and property accessor edits
   also reconstruct the accessor method/body binder through the incremental
   semantic path, retaining parameters and fresh local symbols without forcing
   complete declaration binding. Typed conditional bindings recover
   equivalently under incremental and full parsing and publish fresh semantic
   identities after a rename. Macro partitions and less common declaration
   families remain less complete. Broken signatures and bodies must never
   contaminate unrelated declarations or replace local errors with
   invocation-site resolution failures.

Nullable match editing now has a focused recovery invariant. Removing an arm
expression must produce the same syntax and diagnostics under incremental and
full parsing, retain the surrounding match node, and leave unrelated sibling
declarations resolvable. Restoring the expression must remove the transient
errors and recover the original exhaustiveness result in either semantic-query
order. This keeps an ordinary incomplete editor snapshot local instead of
turning it into stale semantic state or document-wide declaration loss.

These priorities describe correctness risk, not a request for a broad rewrite.
Each slice should keep using the smallest failing semantic boundary and a
public diagnostic, symbol, type, operation, metadata, or runtime assertion.

### Refactoring signals from recent stabilization

The recent commit history is concentrated in `BlockBinder`, semantic-model
queries, overload resolution, control-flow analysis, and constructed symbols.
That concentration suggests the following bounded refactoring direction:

- keep contextual target typing scoped to the expression root that owns the
  target; descendant expressions receive a target only through an explicit
  language rule;
- prefer control-flow analysis over already-bound roots, so public analysis
  cannot re-enter the binder merely to rediscover a condition or filter;
- centralize construction, substitution, and nested reanchoring before
  splitting symbol wrappers into more precise public categories;
- share parser recovery boundaries between declaration families, while
  retaining family-specific parsing where their grammars genuinely differ;
- keep declared nullability, nullable metadata contracts, and optional null
  flow facts as separate layers with explicit hand-off points.

These are maintenance boundaries, not large rewrites. Each extraction should
follow a failing or ambiguity-revealing test and preserve source/PE and
diagnostics-first/symbol-first parity. The root-scoped target-type frames and
shared parameter-list recovery boundary are completed examples of this style.

### Latest focused verification gates

The latest stabilization batch added or re-established these boundaries:

- required conditions, loop expressions, lock receivers, and match scrutinees
  propagate nested abrupt completion into public control-flow analysis;
- catch filters preserve abrupt completion through binary, member-access, and
  unary wrappers without re-entering semantic binding;
- constructed generic conversion operators retain substituted parameter,
  return, and containing-type symbols in source and emitted metadata, in either
  semantic-query order;
- nested constructed self types and generic extension-method inference agree
  between source and emitted metadata;
- target typing is scoped to the owning expression root, preventing a return or
  other nested expression from inheriting the surrounding Boolean target;
- incomplete namespace-function and method parameter lists recover at a body,
  expression body, or constraint boundary while retaining later declarations;
- alias directives over ad-hoc unions resolve to the constructed
  `System.Union<...>` representation supplied by Raven Core rather than the
  removed legacy type-union mechanism;
- macro body edits publish authored diagnostics while retaining sibling macro
  declarations, invocation resolution, and method-like hover behavior;
- `MaybeNullWhen` source, Raven-emitted PE, and external-metadata behavior,
  incremental parser recovery, overload resolution, and assignment/reachability
  analysis all have focused passing suites. These gates should remain targeted
  during ordinary stabilization; a full baseline is reserved for broad or
  release-boundary validation.

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

Error-type construction is now an absorbing recovery operation: calling the
public named-type `Construct(...)` API on an `IErrorTypeSymbol` preserves that
symbol for any supplied arguments rather than throwing. Tooling can therefore
continue generic-shaped semantic inspection while declarations are incomplete.

The port can begin incrementally when:

- the compiler-writing subset has an explicit supported-feature inventory;
- the porting ledger distinguishes contractual behavior, known defects,
  structural debt, and cleanup that is intentionally deferred;
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
boundary at a time and checked against the same suites. Each component carries
its focused tests and can replace the C# implementation independently when the
architecture permits it. The existing compiler continues to receive
correctness, diagnostics, recovery, and performance improvements throughout
that process; after parity is established, the Raven implementation enters a
separate Raven-native cleanup phase.
