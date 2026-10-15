# Consistent ranges and generic bounds

Status: generic capability/library proposal, 2026-10-15. The endpoint migration is
implemented separately; see [compiler lowering](../../compiler/range-endpoints.md).
The author requested a reconsideration of ranges across .NET and neoCLR, starting
with generic bounds and endpoint consistency and then integer iteration. Structural
type work remains parked. All known Raven consumers are project-owned; the author
explicitly removed compatibility as a concern. No legacy behavior switch is planned.

## Recommended language contract

Use `a..b` for an inclusive start/exclusive end and `a..=b` for both endpoints included,
consistently in loops, collection expansion, patterns, stored bounds and slices.
Remove `..<` in a coordinated migration. The author directed syntax/semantic work and integration on Raven main first,
including migration of owned code; neoCLR-specific semantics follow afterwards.

It aligns half-open syntax with .NET slicing and Rust while retaining an explicit
inclusive form. Before this migration, Raven numeric `..` loops included the end while emitted
System.Range values excluded it. The different contexts must no longer reinterpret endpoint
inclusion. Changing the target must not change the endpoint rule either.

`a..`, `..b`, `..=b` and `..` describe unbounded endpoints. An abstract omitted lower
bound is not zero. A numeric loop may explicitly define a zero-start convenience;
initial iteration support still requires a finite end and rejects from-end bounds.

## Types and lowering

Proposed ordinary nominal library APIs, with names/packaging provisional:

- Bound<T>: Included(T), Excluded(T), Unbounded.
- Interval<T>: immutable ordered lower/upper bounds with comparer-based containment.
- Int32 progression: immutable start, end, inclusion and signed nonzero step, with
  independent enumerator state. Descending progressions are not reversed intervals.

Keep System.Index and System.Range for collection-relative positions and slices.
Numeric interval values support negative endpoints; they must not be forced through
System.Index construction. Expected-type and indexing contexts can select collection
bounds, but cannot change inclusion. The natural type of numeric standalone ranges,
fully unbounded inference and overload ranking need focused binder qualification.
Do not add structural metadata kinds or runtime opcodes for these library values.

.NET retains its actual System.Range ABI. The compiler now translates inclusive
indices to equivalent exclusive boundaries with checked arithmetic: `n` becomes
`n + 1`, and `^n` becomes `^(n - 1)`, after validating the original Index. This needs
no collection length. Inclusive `^0` and an unrepresentable successor reject;
consuming APIs perform collection bounds checks. System.Range preserves selection,
but cannot retain the source endpoint spelling. This supersedes the initial proposal
that inclusive conversion always needs a length-aware adapter. Proposed generic
bounds retain inclusion explicitly. Custom indexers follow their declared contract;
they cannot be assumed to expose a length or support generic bounds. Native neoCLR
lowering still needs separate implementation and qualification.

Ordered intervals do not imply arithmetic, enumeration or Count. Use a consistent
total-order comparer for containment; do not mix comparison operators and comparer
semantics. Domain-aware emptiness is separate: an open interval between adjacent
integers is empty even though its lower endpoint compares less than its upper one.
Partial orders and generalized interval algebra are deferred.

Endpoint agreement does not erase runtime differences: neoCLR string positions count
graphemes and .NET string positions count UTF-16 code units; error transport and
copy/view behavior follow the consuming platform API. No new Runtime Contract
configuration is added in this design slice. Future backend capability checks must
reject unsupported forms rather than substitute different range semantics.

## Explicit capabilities

Author clarification (2026-10-15): types declare their supported qualities through
interfaces and generic contracts; capabilities are not added to every type. Ordering
and stepping are separate opt-ins. Bound<T> storage needs neither. Default containment
requires an ordering contract; an explicit comparer supports externally owned types.
.NET IComparable<T>/IComparer<T> and neoCLR ComparableTo<T>/Comparer<T> are existing
platform building blocks, not identical metadata identities.

Iteration additionally needs a declared stepping contract or an explicit generic
stepper provider that detects exhaustion and guarantees directional progress without
wrapping. Addition alone is insufficient. Keep the step's type separate from the
value's type. Exact interface shape and static-member support remain design work.
Marker declarations must be backed by callable behavior where an operation needs it;
no duck-typed capability discovery or universal iteration/Count contract is proposed.
Integer specialization must preserve this contract while avoiding per-element dispatch.

## Integer iteration contract

Begin with Int32. Default step +1; negative step descends; a direction mismatch
produces an empty traversal. Zero step is a constant diagnostic or dynamic checked
failure. Start, end and step are evaluated once, left-to-right. Continue advances
once; break never advances. Stored progression enumeration is repeatable and independent.

Calculate the prospective Int32 successor in Int64, check the directional endpoint
and Int32 domain, then narrow. Out-of-domain advancement terminates instead of wrapping
or faulting after the final valid element. This handles inclusive integer extrema,
overshooting steps and int.MinValue as a step without computing its negation. Larger
integer widths need a separately proven pre-addition check. Do not compute an Int32
Count to drive iteration. Direct loops should use scalar locals/branches; enumeration
must not require allocating an array or boxing each element.

Existing floating-point, decimal and char loops require coordinated endpoint syntax
migration, but their advancement rules need separate review. Integer overflow proofs
do not establish floating-point progress or Unicode iteration correctness.

## Implementation map and checks

Source inspection at main 41abd72ea found the split in BlockBinder.BindRangeExpression,
BoundRangeExpression.IsUpperExclusive, ExpressionGenerator.EmitRangeExpression and
range-loop/collection emitters. StatementGenerator increments after the body with
ordinary Add/conversion instructions. This is evidence for an overflow investigation,
not an executed regression result.

1. Parser/tokens/generated syntax: add `..=`, remove `..<`, update recovery, grammar
   and TextMate support. Follow the generator build workflow.
2. Binder/bound model/operations: represent inclusion explicitly, unify semantic
   meaning across patterns/loops/slices; specify natural types and conversion ranking.
   Public semantic queries, hover and incremental edits must agree.
3. Lowering: share integer progression semantics across direct loops and collection
   expansion; keep target adapters responsible only for representation/capabilities.
4. Libraries/emission/import: round-trip nominal bounds and progression contracts,
   qualify ordinary .NET first, then deliberately carry general changes to the neoCLR
   line and validate native interpreter/JIT/AOT. Do not merge entire compiler branches.
5. Migrate all owned samples/tests/docs, including range patterns, collection expansion
   and documentation snippets. Existing language specs remain truthful until code lands.

Focused tests must cover singleton/empty intervals, all endpoint flags, negative bounds,
^0/^1, inclusive maximum slicing, receiver/endpoint effects, overloads and imported
values; iteration around both Int32 limits, minimum step, overshoot, direction mismatch,
dynamic zero, break/continue and independent enumerators. Use bounded edge cases.
Validate parser recovery and range-pattern reachability after the endpoint change.
Compare the hot integer loop with a handwritten loop before making performance claims.
Modern .NET testing does not qualify .NET Framework/NanoFramework or native platforms.

## Research

Retrieved 2026-10-15: [.NET 10 Range](https://github.com/dotnet/runtime/blob/v10.0.0/src/libraries/System.Private.CoreLib/src/System/Range.cs),
[C# range spec](https://github.com/dotnet/csharplang/blob/main/proposals/csharp-8.0/ranges.md),
[Rust expressions](https://doc.rust-lang.org/reference/expressions/range-expr.html),
[Rust Bound](https://doc.rust-lang.org/std/ops/enum.Bound.html) and
[Noda Time 3.2 Interval](https://nodatime.org/3.2.x/api/NodaTime.Interval.html).
The [C# enumeration discussion](https://github.com/dotnet/csharplang/discussions/3004)
is experience/design discussion, not a language commitment. The paired
[neoCLR design](https://github.com/marinasundstrom/neoCLR/blob/main/docs/design/generic-bounds.md)
records the alternatives and runtime/library tradeoffs in detail.

Documentation-only slice: no compiler tests or new source-language execution claimed.
