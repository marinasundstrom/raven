# Collection expressions

Collection expressions use bracket syntax `[element0, element1, ...]` (with an optional
trailing comma) to build list-like collection types. Adjacent elements may also be
separated by a newline, in which case the syntax tree records `SyntaxKind.None` in the
separator slot. Raven also supports an explicit array form `[|element0, element1, ...|]`
with the same separator rules. Elements are evaluated from left to right.
In addition to ordinary expressions, an element may be written as
`...expression`—called a *spread*. Spreads enumerate the runtime value and insert each
item into the resulting collection in order. The spread source must be convertible to
`System.Collections.IEnumerable` (including arrays and `IEnumerable<T>` implementations);
otherwise diagnostic `RAV2022` is reported. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3620-L3670】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L260-L266】

Within a collection expression, a bare range element such as `1..3` or `1..<4` also
expands inline as a sequence of values rather than contributing a single `Range`
instance. This form follows the same iteration semantics as a comprehension sourced
from that range, so `..` is inclusive and `..<` excludes the upper bound.

Collection expressions also support a list-comprehension form:

* `[for item in source => selector]`
* `[for item in source if condition => selector]`
* `[for let (a, b) in source => selector]` and other pattern targets follow the same
  matching/binding rules as `for` statements.

Collection expressions also support dictionary-shaped elements:

* `key: value` inserts one entry,
* `...expr` spreads entries from another dictionary-compatible source,
* `...key: value` inserts one entry in spread position, and
* `for item in source => key: value` / `for item in source if condition => key: value`
  build entries through a dictionary comprehension.
  Comprehension targets may also be patterns, including deconstruction patterns.

When a collection expression contains any dictionary-shaped element, the entire
literal is treated as dictionary-shaped. Positional elements, range elements,
and value-producing collection comprehensions cannot be mixed into that same literal.

The `source` position also accepts range expressions. These follow the same range
iteration semantics as `for ... in start..end` loops.

Comprehensions are lowered by the compiler into collection-building loops, so they
follow the same target-typing and conversion rules as other collection elements.
Pattern-targeted comprehensions also inherit `for`-statement matching semantics:
non-matching elements are skipped, an optional `if` filter runs after the pattern
match succeeds, and outer `let` / `val` / `var` binding keywords supply the binding
mode for otherwise bare pattern captures.

Collection expressions are target-typed:

* **Array targets** — When the expected type is a one-dimensional array `T[]`, the expression allocates a
  new array of that element type. Each item is implicitly converted to `T` before storage,
  and spreads must enumerate values assignable to `T`. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3672-L3738】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L950-L1016】
  When the expected type is a fixed-length array `T[N]`, Raven also validates the statically
  known element count when it can prove one. Plain elements contribute `1`, and spreading a
  fixed-length array `T[M]` contributes `M`. If the proven total does not match `N`, binding
  reports a size-mismatch diagnostic instead of deferring the error to runtime.
  Multidimensional array types such as `T[,]` are not target-typed by collection/array literal
  syntax; they must be created through runtime APIs or other existing values and then used
  through normal indexing/assignment syntax.
* **Collection targets** — When the expected type is a non-array type with an accessible
  parameterless constructor and an instance `Add` method, the compiler constructs the
  target and calls `Add` for every element. The `Add` parameter determines the element
  conversions, and spread entries must supply compatible values. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3738-L3776】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1016-L1096】
  Dictionary-shaped collection expressions use the same builder model, but require an
  accessible instance `Add(key, value)` method instead. Each key expression is converted
  to the first parameter type and each value expression is converted to the second.
  Dictionary spreads require a source compatible with `IEnumerable<KeyValuePair<TKey, TValue>>`
  after key/value conversion.
* **No target type** — Without an expected type, Raven infers a best common element type by
  merging all element contributions (spreads use their enumerated element type). The resulting
  collection kind then defaults from the literal modifiers:
  * bare collection expressions produce `ImmutableList<T>`;
  * `![...]` collection expressions produce `List<T>`;
  * `[| ... |]` array expressions produce CLR arrays.
  Spreads and range elements contribute element types only; they do not change the default
  collection kind. If no compatible common element type can be inferred without falling back to
  `object`, `System.ValueType`, or interfaces, inference fails with a type-mismatch diagnostic
  and an explicit target type is required.
    【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3776-L3861】
  Dictionary-entry collection expressions instead infer key and value types separately. Bare
  literals default to `ImmutableDictionary<TKey, TValue>`, while `![...]` dictionary-entry
  literals default to `Dictionary<TKey, TValue>`.

Collection expressions also carry optional literal syntax:

* bare literals are immutable-by-default;
* `![...]` requests a mutable default for targetless inference;
* `[| ... |]` requests explicit CLR-array fallback instead of list-family fallback;
* explicit target typing may still override mutability and collection kind.

The intent of this design is to make local collection literals default toward immutable
data-processing code. Mutable collection creation remains available, but it must be made
intentional either through the `!` marker or through an explicit mutable target type.

When no explicit target type is present, bare collection expressions fall back to
`ImmutableList<T>`, `![...]` expressions fall back to `List<T>`, and `[| ... |]`
expressions fall back to CLR arrays.
If code wants another concrete collection type such as `Queue<T>` or `Stack<T>`, it must
provide an explicit target type so Raven can bind the literal through the normal array or
collection-builder rules. The choice between explicit commas and implicit newline separators
does not affect the inferred collection kind.

The current targetless default matrix is therefore:

* `[a, b]` -> `ImmutableList<T>`
* `[
      a
      b
  ]` -> `ImmutableList<T>`
* `![a, b]` -> `List<T>`
* `![
      a
      b
  ]` -> `List<T>`
* `[|a, b|]` -> `T[N]` / `T[]`

An empty collection expression `[]` must be used in a context that supplies a target type;
otherwise its type cannot be inferred. When a target type is available, the compiler
produces an empty instance of that type (an empty array or an initialized collection).
【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3620-L3651】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1170-L1192】

```raven
val numbers: int[] = [1, 2, 3]
val combined = [0, ...numbers, 4]
val squares = [for n in numbers => n * n]
val evenSquares = [for n in numbers if n % 2 == 0 => n * n]
val evenSquaresInRange = [for n in 4..250 if n % 2 == 0 => n * n]
val selectedNames = [for let (2, name) in [(1, "Ada"), (2, "Bob")] => name]

val names: List<string> = ["a", "b"]
val inferred = [1, 2.0]      // inferred as ImmutableList<double>
val inferredList = [1; 2; 3] // inferred as ImmutableList<int>
val mutableList = ![1, 2, 3] // inferred as List<int>
val mutableListAlt = ![1; 2; 3]  // inferred as List<int>
val inferredArray = [|1, 2, 3|] // inferred as int[3]
val expandedArray = [|...inferredArray, 4|] // inferred as int[4]

val byName = ["a": 1, "b": 2] // inferred as ImmutableDictionary<string, int>
val mutableByName = !["a": 1, "b": 2] // inferred as Dictionary<string, int>
val merged = [..."a": 1, ...mutableByName, "c": 3]
val lengths = [for key in [|"a", "bb"|] => key: key.Length]
val doubled = [for let (key, value) in [("a", 1), ("b", 2)] => key: value * 2]
val readonlyLookup: IReadOnlyDictionary<string, int> = ["a": 1, "b": 2]

val baseList: ImmutableList<int> = [2; 3; 4]
val preserved = [7, ...baseList, 5] // inferred as ImmutableList<int>

val a: ImmutableList<int> = [1]
val b: List<int> = [2]
val defaulted = [...a, ...b] // inferred as ImmutableList<int>

val forced: List<int> = [...a, ...b]
val forcedObject: object[] = [1, true]
```

### Element access

```raven
val list = [1, 42, 3]
val a = list[1]
```
