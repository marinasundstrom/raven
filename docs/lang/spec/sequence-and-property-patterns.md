# Sequence and property patterns

* `[pattern1, pattern2, …]` — **sequence pattern**. Matches when the scrutinee
  is a sequence-deconstructable, indexable collection and each sequence element
  pattern matches the corresponding element or segment.

  * Sequence patterns are supported for arrays (`T[]`), `string`, and indexable
    collection types (`Count` + integer indexer, for example `IList<T>` /
    `IReadOnlyList<T>`).
  * The same bracketed shape is also used for collection deconstruction
    assignments and declarations (`[val a, val b] = values`), which support
    arrays, `string`, and indexable collection types.
  * In the syntax tree, bracketed patterns are represented as `SequencePatternSyntax`
    (with `SequencePatternElementSyntax`), distinct from parenthesized positional
    patterns (`PositionalPatternSyntax`).
  * A plain element pattern consumes exactly one element.
  * A fixed-size segment pattern `..N pattern` consumes exactly `N` elements as a
    subsequence.
  * An open rest segment `...pattern` consumes the remaining unmatched
    subsequence and may appear either in the middle of the pattern or at the
    end. A bare `...` is also permitted as a non-capturing rest segment that
    ignores the unmatched subsequence. Likewise, a fixed-size segment may omit
    its designation as `..N` to skip exactly `N` elements without creating a
    binding. At most one open rest segment is permitted.
  * Fixed-size segments may appear multiple times because their widths are fully
    determined by the syntax.
  * In freestanding and inline sequence
    patterns, captures must use `val`/`var`/`let`; bare identifiers are treated
    as value patterns against existing values. Type-constrained captures may be
    written as `val x: T` or `T x`. The same rule applies inside segment forms,
    for example `..2 val start` and `...val rest`. Bare `...` and bare `..N`
    are the non-capturing exceptions.
  * If a sequence pattern contains no open rest segment, the input length must
    match the total fixed width exactly.
  * If a sequence pattern contains an open rest segment, the input length must
    be at least the total fixed width of the non-rest elements.
  * For arrays and indexable collections, single-element captures bind the element
    type and segment captures bind an array slice.
  * When the input is a fixed-length array `T[N]`, captured fixed/rest array
    segments preserve an inferred fixed length when the segment width is
    statically known. For example, `[val a, val b, ...val rest]` against
    `int[4]` binds `rest` as `int[2]`, and `[..2 val head, val tail]` against
    `int[3]` binds `head` as `int[2]`.
  * For `string`, single-element captures bind `char` and segment captures bind
    `string`, even for `..1`.

## Property patterns

## Whole-pattern designations

Primary structural patterns may carry an optional trailing designation for the
entire matched value in binding-enabled pattern contexts:

* positional patterns: `(pattern1, pattern2) designation`
* sequence patterns: `[pattern1, ...pattern2] designation`
* member patterns: `.Case(...) designation`
* nominal deconstruction patterns: `Type(...) designation`
* property patterns: `Type { ... } designation`

The trailing designation is introduced only if the full pattern succeeds.
It is not valid in an `expr is pattern` expression. Use a dedicated pattern
statement when the whole matched value must be named.

```raven
if let (2, > 0.5) point = input {
    WriteLine(point)
}

for let Person(1, name, _) person in persons {
    WriteLine(person.Name)
}

match value {
    val Some((x, y)) pair => pair.Value
    _ => 0
}
```

Rules:

* A trailing designation may be written with an explicit binding keyword, such
  as `val point` or `var point`.
* In constructs that already carry an outer binding keyword (`if let ...`,
  `while let ...`, `for let ...`, `match { val ... => ... }`), the trailing
  designation may omit its own binding keyword and inherits the outer binding mode.
* Without a binding-enabled pattern context, a trailing designation is not part
  of the pattern. In particular, `expr is Type { ... } value` is invalid.
* Writing `_` discards the matched value while still enforcing the pattern.

* `Type { member1: pattern1, member2: pattern2, … }` — **property pattern**.
  Matches when the scrutinee is not `null` and can be treated as `Type`, then
  evaluates each listed member subpattern against the corresponding instance
  member.

  * Each `member: pattern` targets an accessible instance field or readable
    instance property.
  * Nested patterns are type-checked against the member’s type.
  * Member subpatterns are evaluated left-to-right; bindings from earlier entries
    are in scope for later ones.
  * The empty pattern `Type { }` matches any non-`null` value that can be treated
    as `Type`.

* `Type { … } designation` — **property pattern with designation**. Like
  `Type { … }`, but also introduces a designation for the matched receiver in a
  binding-enabled pattern context.

  * The designation may use an explicit binding keyword (`val`, `let`, or
    `var`), or inherit the binding mode from an outer construct such as
    `if let` / `while let` / `for let` / an outer match-arm binding keyword.
  * Writing `var p` produces a mutable binding. Omitting a binding keyword
    requires an outer binding mode.
  * The designation is introduced only if the entire property pattern succeeds.
  * Writing `_` discards the receiver value while still enforcing the pattern.

* `{ member1: pattern1, member2: pattern2, … }` — **inferred property pattern**.
  Like `Type { … }`, but the receiver type is inferred from the scrutinee’s static
  type.

  * If the receiver type cannot be inferred, the pattern is invalid and requires
    an explicit type.
  * `{ }` acts as a non-null test.

* `{ … } designation` — **inferred property pattern with designation**.

  * The designation may use an explicit binding keyword (`val`, `let`, or
    `var`), or inherit the binding mode from an outer construct.
  * Writing `var p` produces a mutable binding.
  * The form is invalid in an `is` expression; use `if let { ... } name = expr`
    or another dedicated pattern-binding construct to bind the receiver.
