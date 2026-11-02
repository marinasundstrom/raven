# Resolving bang expressions and macro invocations

This is the resolution contract adopted during the compiler port. Implementation
is in progress: current binding rejects some shadowed expression forms rather
than interpreting their ordinary value meaning. This document does not claim
that every example below already compiles.

## Syntax position selects the possible interpretations

The parser determines which interpretations the syntax and containing position
permit. Semantic resolution selects between those interpretations only when
there is a real ambiguity. A bang token alone does not establish that a name
refers to a macro.

| Form and position | Resolution |
| --- | --- |
| Prefix `!value` | Ordinary unary operator. |
| Bare postfix `value!` | Ordinary nullable suppression or carrier unwrap. |
| `(value!)(args)` or `(value!)[index]` | Ordinary call or indexing of the explicitly parenthesized value. |
| `name!(args)` in expression position | Ordinary value interpretation wins when normal scope lookup resolves `name` as a value; otherwise resolve a macro. |
| `name![items]` in expression position | The same scope rule selects value indexing. If no value resolves, a macro interpretation requires a supported sequence carrier; reserving a future carrier must not reserve this spelling away from ordinary indexing. |
| Macro-only token-tree/expression-header syntax, such as `name! { ... }` | Resolve a macro. A value name does not turn this macro-only carrier into a value expression. |
| Declaration-shaped macro invocation in a declaration position | Resolve a declaration macro and validate its output for that declaration position. Ordinary operator expressions are not candidates. |

These rules are shared by .NET and neoCLR. They do not depend on runtime
representation or on whether the receiver happens to be nullable.

## Scope decides genuinely ambiguous expression forms

Use ordinary Raven name lookup, including its existing rules for locals,
parameters, pattern bindings, local functions, and members. Do not introduce a
second notion of scope for macros. A lookup result denoting an ordinary value or
callable expression selects the value interpretation. A namespace or type name
alone is not a value binding.

For a qualified name, resolving its receiver to a value commits to ordinary
member lookup. A missing or inaccessible member is then a value-resolution
error, not a reason to retry the complete spelling as a macro name. A qualified
macro name whose prefix resolves as a namespace can bypass an unqualified local
name collision. Qualification must itself obey normal lookup; it is not an
unconditional escape from shadowing.

Selection precedes operand type checking. For example, a local `array: int[]?`
makes `array![0]` suppression followed by indexing, and a local delegate makes
`callback!()` suppression followed by invocation. A local integer with the same
name still wins lookup, but attempting to call or index it produces an ordinary
type error. The compiler must not run a macro after that error. Accessibility,
use-before-declaration, definite-assignment and overload errors likewise do not
cause fallback to another interpretation.

Postfix `!` retains Raven's existing semantics: suppress nullable references,
extract nullable values, and unwrap-or-fail for propagation carriers. This
resolution rule does not redefine that operator or make it conditional on a
macro being present.

## Compiler ownership

Keep candidate syntax independent of symbol availability. The parser must not
consult a macro registry or keep a parallel table of semantic local bindings.
Shared binding owns one decision for each candidate: ordinary expression,
macro invocation, or an error in the selected interpretation. The representation
must retain enough authored syntax to bind either permitted reading without
losing argument/index spans or changing source ownership.

Use the same decision before expression/statement binding, macro expansion,
`GetSymbolInfo`, `GetTypeInfo`, `GetOperation`, macro token classification,
signature help and embedded-language tooling. A syntax node shaped like a macro
candidate is not permission to invoke a provider. Cold semantic queries must
agree with full compilation. IDE layers consume the compiler's answer rather
than performing their own lookup or retrying failed macro resolution.

Cache resolution in binder-owned semantic state. Adding, removing or shadowing
a value declaration can change the selected interpretation even if the bang
expression's text remains unchanged. Incremental invalidation must account for
that scope change before reusing a macro expansion or semantic result.

## Adding macro styles

Every new carrier/style must specify its allowed positions, whether it has an
ordinary-expression interpretation, and how the syntax preserves both candidates
when ambiguous. Carrier-shape validation runs after the name-resolution choice;
a macro-only shape has no ordinary candidate, while an ambiguous shape follows
the scope rule above. Whitespace must not become an undocumented escape hatch.

In particular, implementing sequence carriers later must preserve value indexing
for `array![i]`. It must not change existing value expressions into macro calls
merely because a macro with that spelling has been imported.

## Validation requirements

- Same spelling with and without an imported macro; local/parameter, pattern,
  local-function and member bindings; inner-scope shadowing and qualified names.
- Nullable array indexing, nullable delegate invocation, carrier unwrap followed
  by access, and nonnullable values using the existing postfix-operator rules.
- Invalid ordinary calls/indexes report ordinary diagnostics and never execute a
  macro provider; macro-only and declaration forms remain macro invocations.
- Explicit parentheses, prefix bang, carrier delimiters and expression/statement/
  declaration positions, with source spans and syntax round-tripping preserved.
- Cold symbol/type/operation queries, macro-expansion queries and IDE features
  agree with compilation; edits introducing/removing a shadow invalidate caches.
- Runtime tests preserve evaluation order and evaluate each value receiver once.
