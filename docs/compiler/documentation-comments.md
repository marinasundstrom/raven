# Documentation comment values

`DocumentationComment` is an immutable reference-type value object. Equality and
hashing include the format, raw text, normalized content, body, multiline flag,
and ordered tag values. Independently parsed comments with the same contents
compare equal through `IEquatable<DocumentationComment>`, `Equals(object)`, `==`
and `!=`. Null is unequal to a comment. Raw-text differences remain significant,
even when normalization produces the same body.

`DocumentationTag` remains a record class: kind, name, optional argument and
content define its value. The comment's tag array is compared by its elements,
not by its backing-array identity. `DocumentationComment` retains explicit value
members and its private constructor so callers cannot bypass parsing invariants;
a default record equality implementation would not provide structural array
comparison. The mutable internal tag builder retains reference identity.

The Raven implementation preserves the C# parsing and normalization behavior:

- `TryParse` accepts documentation-comment trivia and returns false with a null
  output for other trivia kinds.
- Prefix removal and common-indentation normalization preserve partial tab widths.
- Markdown tags retain their source spelling, recognized kind, argument and
  continuation text; XML content is preserved without Markdown tag extraction.
- Internal creation and merge helpers retain their existing ordering and
  single-comment identity behavior.
- Blank-line trimming uses an array range, avoiding repeated front removal from
  a list while preserving interior blank lines.

Shared C# tests cover these contracts against both implementations. Existing
trivia tests also run against the selected syntax implementation. Semantic symbol
attachment and display-formatting tests remain C# tests until those dependent
components are ported. This work does not generate RavenDoc output or qualify
native neoCLR execution; the values and parsing rules are target-independent.
