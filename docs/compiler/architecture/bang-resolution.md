# Bang syntax and macro resolution

Raven reserves macro-shaped bang forms for macro syntax. Scope lookup does not
reinterpret a macro-shaped form as an ordinary postfix operator expression.
This avoids ambiguity as more macro carrier styles are introduced.

| Form | Meaning |
| --- | --- |
| `!value` | Ordinary prefix operator. |
| Bare `value!` | Postfix nullable suppression or carrier unwrap. |
| `foo!(args)` | Macro invocation syntax, not suppression followed by invocation. |
| `foo![items]` | Reserved macro sequence syntax, not suppression followed by indexing. The macro sequence carrier is not implemented yet. |
| `foo! { ... }` and other macro carriers | Macro syntax, subject to the carrier and containing-position rules. |
| `foo?(args)` | Conditional invocation. |
| `foo?[index]` | Conditional element access. |
| `(foo!)(args)` | Explicit suppression/unwrap, then ordinary invocation. |
| `(foo!)[index]` | Explicit suppression/unwrap, then ordinary indexing. |
| `foo!.Member` | Suppression/unwrap, then ordinary member access. |

The distinction is syntactic. A nullable local variable, parameter or member named
`foo` does not make `foo!(args)` or `foo![items]` an ordinary value expression.
Use conditional access to skip the operation when the receiver is null, or
parenthesize the explicitly unwrapped/suppressed value to select ordinary access.
Whitespace alone is not a disambiguation mechanism.

Postfix `!` retains its existing semantics: suppression for nullable references,
value extraction for nullable value types, and unwrap-or-fail for propagation
carriers. Parentheses change the syntax grouping, not these semantics. An invalid
ordinary call or index reports an ordinary diagnostic; it does not fall back to
macro expansion. An unknown, shadowed or unsupported macro-shaped invocation
likewise does not fall back to ordinary postfix access.

## Position and scope

Syntax position still determines which macro carriers are permitted and what
output they can produce. Declaration-shaped invocations must resolve to macros
valid in that declaration position. Expression and statement carriers retain
their respective output requirements.

Scope participates in resolving the selected name within those rules, including
existing macro imports, aliases and shadowing. It does not change the syntactic
category. A parsed macro-shaped candidate is not permission to execute a provider
before macro name and carrier validation succeed.

Shared compiler resolution must give compilation, cold symbol/type/operation
queries, macro-expansion queries, highlighting and signature help the same answer.
IDE layers must not independently reinterpret a rejected macro as value access.
This rule is shared by .NET and neoCLR.

## Future macro styles and porting

Every new macro style must specify its carrier syntax and permitted positions.
In particular, future sequence carriers can use `Name![items]` without changing
ordinary value-indexing precedence: ordinary explicit unwrap/index access is
already written `(value!)[index]`.

During C# porting, translate `array![index]` to `(array!)[index]` and nullable
delegate `callback!(args)` to `(callback!)(args)` where the intent is an asserted
non-null value operation. Do not replace these with conditional access unless
skipping the operation is the intended behavior. Bare suppression and ordinary
member access need no added parentheses for this distinction.

Existing runtime tests cover conditional delegate invocation and null/non-null
conditional array indexing. A direct compiler analysis probe also accepts both
conditional forms and both parenthesized explicit-suppression forms. Macro-style
indexing remains reserved; these statements do not claim that sequence macros
are implemented.
