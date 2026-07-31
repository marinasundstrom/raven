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
Definite assignment, public nullability state, non-literal constant loop
conditions, nested expression-level abrupt flow, and broader join behavior
remain to be stabilized.

### Public nullability information is becoming flow-sensitive

`TypeInfo` now preserves an expression's declared nullable annotation while
projecting the bound expression's current flow state. Strict null-check branches
and null guards therefore report `NotNull` through the public semantic model in
both cold and diagnostics-first query orders, matching the state already used
by binding and nullable-access diagnostics.

The remaining conformance matrix needs joins, loops, richer pattern tests,
nullable unions, and incremental edits that change control flow.

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
symbols. Flow attributes and nullable context placement remain separate ABI
slices.

### Control transfers have one expression-context policy

`return` and `throw` are useful non-completing expressions. Expression blocks
project their `return` and `throw` items as expression statements, and abrupt
paths do not contribute a value to type joins. Bare expression-form `return`
has an implicit `unit` payload.

`break` and `continue` remain statement-only loop control. They report
`RAV1902` and `RAV1903` from expression blocks even when a lexical loop exists.
Focused parser, semantic, macro-body, analyzer, and runtime tests cover the
projection, diagnostics, reachability, and lowering policy.

### Local initialization and `out` assignment are distinct rules

Raven locals must be initialized where they are declared. An initializer-less
local reports `RAV0166`, so there is no intermediate local state for a separate
use-before-assignment diagnostic to analyze. The unused `RAV0165` descriptor was
removed rather than presenting a rule the compiler never reported.

`out` parameters are different because the caller supplies their storage. The
callee must assign each `out` parameter on every normal exit. The focused
conformance matrix covers straight-line exits, `if` joins, exhaustive and
non-exhaustive matches, return and throw arms, and terminating versus breakable
loops. Match joins use bound exhaustiveness facts, while proven non-terminating
loops reuse ordinary control-flow completion.

### Reachable symbol contracts contain incomplete members

Array, tuple, type-union, and module symbol implementations contain
`NotImplementedException` or unsupported lookup members. Each member should be
classified as:

- reachable and required, in which case it must be implemented and tested;
- intentionally unsupported, in which case the public contract should express
  that without a runtime surprise; or
- dead, in which case it should be removed.

This audit matters before compiler code begins consuming the public API from
Raven.

### Generic overload resolution has explicit gaps

Overload resolution currently skips some open generic method-group candidates,
and conversion ranking uses coarse fallback scores in several cases. This may
be adequate for existing samples without being a stable language rule.

Tests should cover inference, constraints, variance, extension methods, method
groups, lambdas, `null`, unions, user-defined conversions, and ambiguity. Each
test should assert the chosen symbol or diagnostic, not an internal lowering
shape.

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

### Source text is both incomplete and allocation-heavy

`SourceText` currently rebuilds whole strings for edits, slices strings when
creating positioned readers, and leaves line, copy, and write APIs
unimplemented. This is primarily a quality and performance lane, but the
unimplemented public surface is a correctness concern if reachable.

The eventual representation should support cheap snapshots and spans without
creating strings in normal compiler paths. Caching should be snapshot-owned,
bounded, and invalidated by source identity rather than hidden global state.

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

### Error isolation

Each declaration family needs tests where its signature is valid and its body
is broken. Later declarations and uses must continue to bind. Repeat this for
top-level functions, methods, accessors, constructors, local functions, macro
functions, types, extensions, and union cases.

### Behavioral conformance

Prefer observable language behavior and public compiler APIs over assertions
about internal bound nodes, lowering steps, or emitted instruction sequences.
Where the specification and implementation disagree, reduce the case and make
an explicit language decision before locking the test.

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
- no source edit in that subset reaches a generic exception or an unimplemented
  semantic contract;
- full and incremental syntax and semantic equivalence suites pass;
- semantic results are independent of query order and concurrency;
- error recovery preserves unrelated declarations;
- flow, conversions, overloads, generics, and patterns have conformance
  coverage sufficient for compiler code;
- remaining gaps are documented as intentionally out of the compiler-writing
  subset or safe to address during the port.

At that point, Raven-authored compiler components can be introduced one
boundary at a time and checked against the same suites. The existing compiler
continues to receive correctness, diagnostics, recovery, and performance
improvements throughout that process.
