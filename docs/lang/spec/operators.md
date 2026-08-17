# Operators

Operators combine, compare, transform, or assign values. Raven follows familiar
.NET arithmetic and comparison conventions and adds language-specific operators
for pipelines, ranges, null handling, error propagation, and from-end indexes.

```raven
let total = price * quantity + shipping
let last = values[^1]
let page = values[10..<20]
let normalized = input |> Trim() |> Normalize()
```

## Precedence

Precedence determines which parts of an expression are grouped together when
parentheses are absent. For example, `a + b * c` means `a + (b * c)` because
multiplication binds more tightly than addition.

From lowest to highest precedence:

1. Pipeline: `|>`
2. Assignment: `=  +=  -=  *=  /=  %=  &=  |=  ^=  ??=`
3. Null-coalescing: `??`
4. Logical OR: `or`, `||`
5. Logical AND: `and`, `&&`
6. Bitwise OR: `|`
7. Bitwise XOR and AND: `^  &`
8. Equality and relational: `==  !=  <  >  <=  >=`
9. Type tests: `is  as`
10. Range and shifts: `..  ..<  <<  >>`
11. Additive: `+  -`
12. Multiplicative: `*  /  %`
13. Cast: `(T)expression`
14. Prefix: `+  -  !  not  ~  await  fixed  stackalloc  typeof`
15. Postfix: call `()`, member `.`, element access `[]`, nullable suppression
    `!`, and propagation `?`

Assignments associate from right to left. Other binary operators associate from
left to right unless their feature article states otherwise. Use parentheses
when they make the intended grouping clearer to a reader.

## From-end indexes

Prefix an integer expression with `^` to create a `System.Index` that counts
from the end of a sequence:

```raven
let last = values[^1]
let secondFromEnd: System.Index = ^2
```

The operand must be implicitly convertible to `int`. The result retains its
`System.Index` type even without a target annotation.

When an array is indexed from the end, Raven computes the position from the
array's length. The receiver and index operand are each evaluated exactly once.

The prefix is adjacent: `^1` is valid, while `^ 1` is not.

## Ranges

Use `..` for a range with inclusive notation and `..<` for a half-open range
whose upper bound is excluded:

```raven
let range = 3..^5
let halfOpen = 3..<10
let head = ..3
let tail = 3..
let all = ..
```

Either bound may be omitted or prefixed with `^` to count from the end. Each
written bound must be implicitly convertible to `int` and is evaluated once,
from left to right. The expression retains its `System.Range` type when no
target type is supplied.

Ordinary boundaries use `Index.FromStart`; `^` boundaries use
`Index.FromEnd`. Omitting a boundary produces the corresponding
`Range.StartAt`, `Range.EndAt`, or `Range.All` shape.

A one-dimensional array accepts one range argument and produces a sliced array:

```raven
let middle = values[2..<5]
```

Other receivers resolve `[]` through their indexers, so overloads accepting
`Index` and `Range` can coexist.

Inside a collection expression or `for` source, a numeric range is enumerated
instead of being stored as a `Range` value. See [Collection
expressions](collection-expressions.md) and [Control-flow expressions and
loops](control-flow-expressions.md#range-loops).

## Bitwise operators

Raven supports bitwise complement `~`, bitwise `&`, `|`, and `^`, and shifts
`<<` and `>>`.

* `~` is defined for `int` and `long`.
* `&`, `|`, and `^` are defined for `int`, `long`, `bool`, and matching enum
  operands.
* `<<` and `>>` accept an `int` or `long` left operand and an `int` shift count.

When both operands are `bool`, `&`, `|`, and `^` evaluate both sides without
short-circuiting and return `bool`. Enum operands must have the same enum type,
and the result keeps that type.

Compound assignments `&=`, `|=`, and `^=` apply the corresponding operator
after evaluating the assignment target once.

Target-typed leading-dot syntax is convenient for enum flags:

```raven
let flags: BindingFlags = .NonPublic | .Static

func withBinding(flags: BindingFlags) {
    // ...
}

withBinding(.Public | .Instance)
```

## Ambiguous-looking forms

* `(<expression>)` is a parenthesized expression. A comma, including a trailing
  comma, makes it a tuple instead.
* `<` begins type arguments only in a type context. Elsewhere it is the
  less-than operator.
* The left side of assignment must be an assignable expression—such as an
  identifier, member, or element access—or an assignment pattern such as
  positional deconstruction.
* From-end indexes are adjacent prefix forms: write `^1`, not `^ 1`.
* Prefix `+` and `-` are also adjacent: write `+3` and `-2`, without whitespace
  between the operator and operand.

Custom operator declarations and overload selection are described under
[Parameters, overloading, and
operators](parameters-overloading-and-operators.md#operator-declarations).
