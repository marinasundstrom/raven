# Range endpoints and target lowering

Raven's range endpoint rule is independent of context: `..` excludes the written
end; `..=` includes it. This applies to numeric loops, collection expansion,
comprehensions and range patterns as well as System.Range values used for slicing.
An omitted end is permitted only with `..`. The former `..<` spelling is removed.

This is a coordinated source change, not a compatibility mode. Existing inclusive
loops, expansions and patterns migrate from `..` to `..=`. Existing half-open slices
retain `..`; prior `..<` uses become `..`. Sequence-pattern fixed/rest segments and
`...` spreads are distinct constructs and retain their meanings.

## Terminology

Shared punctuation does not imply a shared construct:

| Construct | Meaning | Syntax model |
| --- | --- | --- |
| Range expression, `start..end` or `start..=end` | Describes bounds | `RangeExpressionSyntax` |
| Range pattern, `value is start..end` | Tests membership between bounds | `RangePatternSyntax` |
| Slice, `items[start..end]` | Selects a portion of a collection using a range | Element access containing a range expression |
| Fixed segment, `[..2 let pair, ...let rest]` | Matches a fixed number of sequence elements | `SequencePatternPrefixSyntax.SegmentLengthToken` |
| Rest pattern, `[...let rest]` | Matches the remaining sequence elements | `SequencePatternPrefixSyntax` without a segment length |
| Spread, `[...items]` | Expands elements into a collection | `SpreadElementSyntax` |
| Progression, `for n in start..end by step` | Visits successive values within bounds | A range expression consumed by a `for` expression |

Keep fixed token names lexical (`DotDotToken`, `EqualsToken`) and node names semantic.
The sequence prefix uses `OperatorToken`, because it accepts either `..` or `...`;
its former `DotDotToken` slot name incorrectly implied one spelling. Collection
and dictionary spreads expose `DotDotDotToken`; their factories use `...`.
Sequence segments/rest patterns are not range patterns. Slicing consumes bounds;
it is not itself a pattern. Progression describes iteration behavior, not a new
syntax node or a currently shipped generic library type.

## Syntax and semantic model

RangeExpressionSyntax and RangePatternSyntax expose optional EqualsToken in place
of LessThanToken. A present EqualsToken denotes inclusion. Bound range nodes retain
an explicit IsUpperExclusive flag, with exclusive defaults; operation nodes and
pattern analysis use the same flag. The punctuation has no assignment semantics.
Inclusive forms require an end expression. No Runtime Contract setting controls
endpoint inclusion.

## Ordinary .NET emission

System.Range retains its normal half-open ABI. For an inclusive end, Raven first
constructs the original System.Index, validating that its distance is nonnegative.
It then converts the valid end algebraically: start-relative n becomes n + 1;
end-relative ^n becomes ^(n - 1). Arithmetic is checked and the translated Index is
validated by its constructor. Thus ^0 cannot become an included element and a
negative original index cannot be made valid by incrementing it. A start-relative
Int32 maximum cannot be represented as an exclusive .NET boundary and throws on
conversion. These transformations do not need the collection length: resolution
and collection bounds checking remain with the consuming API.

Bounds are evaluated once and left-to-right. The same conversion applies to stored
Range values and direct slices. It preserves the selected positions, but System.Range
cannot preserve whether the source used inclusive spelling: its introspected End is
the equivalent exclusive boundary. Generic bounds that preserve endpoint kind remain
separate library design work. Range construction is not a promise that a later
collection has sufficient length.

Loops and patterns compare directly using endpoint inclusion, avoiding successor
arithmetic on the abstract endpoint. The separate integer-overflow follow-up concerns
advancement after a loop body; it is not part of this syntax migration. Existing
float/decimal/char stepping contracts are unchanged.

## Integration order

Author direction: first validate and integrate syntax/semantics and owned consumers
on Raven main; add neoCLR semantics afterwards. No native backend qualification is
claimed by ordinary .NET tests. No structural-type development is included. Future
native lowering must implement these same endpoint rules through explicit target
contracts, with supported capabilities and failure transport documented independently.

The [generic bounds design](../lang/proposals/consistent-ranges.md) separates explicit
ordering and stepping capabilities from syntax and from runtime representation.

## Validation (2026-10-15)

`scripts/codex-build.sh` and 318 focused compiler tests pass on .NET 11, covering
endpoint parsing, normalization, loops, collections, patterns and slicing. This
includes the endpoint-focused regression checks. A migrated hover regression passes on .NET 10.
Executable loop, range-pattern and array-slicing documentation consumers also pass.
All 72 compiler batches from `scripts/test-baseline.sh` pass (6,233 test
invocations). Its remaining projects pass after fixing the renamed factory
definition (96 test invocations). After the final token-name changes, the generator
build and 207 targeted compiler tests pass. The final .NET 10 pattern/deconstruction
hover selection passes 7 tests, with 2 pre-existing skipped tests. Focused filters cover Range, ForExpression, CollectionExpression,
PatternSyntaxParser, LanguageParser, ForStatementSemantic, PatternLocalCodeGen,
AsyncForLifetime, PdbSequencePoint and LoopStatementCodeGen tests.
No native neoCLR, .NET Framework or NanoFramework execution is claimed.
