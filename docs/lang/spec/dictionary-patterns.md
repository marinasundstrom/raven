# Dictionary patterns

Dictionary patterns require particular keys and match their values at the same
time. Use one when a branch cares about a known subset of a lookup:

```raven
if lookup is ["name": let name, "active": true] {
    Console.WriteLine(name)
}
```

The pattern succeeds only when the input is dictionary-compatible, every
written key exists, and every nested value pattern succeeds.

Raven supports `IDictionary<TKey, TValue>`,
`IReadOnlyDictionary<TKey, TValue>`, and types that implement either interface.
Each key expression is converted to `TKey`; its nested pattern is checked
against `TValue`.

Entries are evaluated from left to right. A key is evaluated before its value
is read and matched.

## Dictionary deconstruction

The same keyed shape can extract values in a declaration or assignment:

```raven
let values: IReadOnlyDictionary<string, int> = ["a": 2, "b": 3]
let ["a": first, "b": second] = values

var left = 0
var right = 0
["a": left, "b": right] = values
```

Deconstruction is extraction-oriented rather than a boolean test. Raven first
checks that the source is dictionary-compatible, then reads each requested key
and assigns its value through the nested designation or deconstruction pattern.

General dictionary matching supports the same nested pattern vocabulary as
other matching contexts. Deconstruction uses the narrower extraction forms
described under [Matching and
deconstruction](pattern-matching.md#matching-and-deconstruction).
