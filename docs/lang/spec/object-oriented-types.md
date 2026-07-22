# Operator precedence and expression disambiguation

For guidance on declaring classes, structs, members, and interfaces, see
[Classes, structs, and interfaces](classes-and-members.md).

## Operators (precedence summary)

Lowest → highest (all left-associative unless noted):

1. Assignment: `=  +=  -=  *=  /=  %=  &=  |=`
2. Null-coalescing: `??`
3. Logical OR: `||`
4. Logical AND: `&&`
5. Bitwise OR: `|`
6. Bitwise AND: `&`
7. Equality: `==  !=`
8. Relational: `<  >  <=  >=`
9. Type tests: `is  as` (binds after relational)
10. Range: `..` (boundaries optionally prefixed with `^`)
11. Additive: `+  -`
12. Multiplicative: `*  /  %`
13. Cast: `(T)expr`
14. Unary (prefix): `+  -  !  fixed  typeof`
15. Postfix trailers: call `()`, member `.`, index `[]`, nullable suppression `!`, propagation `?`

> 🧭 **Disambiguation:**
>
> * `(<expr>)` is a **parenthesized expression** unless a comma appears (including trailing), in which case it’s a **tuple**.
> * `<` starts **type arguments** only in a **type context**; elsewhere it’s the less-than operator.
> * The LHS of assignment must be either an **assignable expression** (identifier, member access, element access, etc.) or a
>   **pattern** such as a positional deconstruction.
> * `^` index expressions are parsed as an adjacent prefix form (`^expr`); whitespace between `^` and the operand is not allowed.
> * Prefix unary `+`/`-` are also adjacent forms (`+3`, `-2`); whitespace between the operator and operand is not allowed.
