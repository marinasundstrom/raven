# Values and statements

Most Raven code is organized around expressions and the values they produce,
including many control-flow forms. Raven nevertheless retains a syntactic and
semantic distinction between expressions and statements.

An expression computes a value and may appear where a value is expected. A
statement controls execution, introduces a declaration, or evaluates an
expression without passing its value to a surrounding expression. Some
constructs, such as `if` and `match`, have both expression and statement syntax
forms. Their surface spelling may be similar, but the parser represents them as
different syntax nodes according to their context.

Calling Raven **expression-oriented** means that value-producing composition is
available in more places than in a conventionally statement-oriented language.
It does not mean that every construct is an expression or that the syntax tree
erases the expression/statement boundary.

Raven has no `void` type. The absence of a meaningful value is represented by the
`unit` type, which has exactly one value written `()`. The type itself may be
spelled `unit` or `()`. Functions without an explicit return type implicitly
return `unit`. In .NET, `unit` corresponds to `void` (see [implementation notes](dotnet-implementation.md#unit-type)). The `unit` type participates in generics and tuples like any other type.

`unit` is also a source-level contract: it says that no value flows out of the
computation. It is not only an emitted substitute for CLR `void`. Consequently,
a non-`unit` expression in the tail position of a `unit` callable is diagnosed
as an unused result. An intentional discard is written explicitly as
`_ = expression`.

## Null and absence

Raven distinguishes nullable values from `unit`:

* `T?` is the canonical way to represent nullable values.
* `unit` (`()`) represents no meaningful result (`void`-like), not nullability.

Carrier types such as `Option<T>` and `Result<T, E>` are described in the
carrier sections of this specification rather than as part of nullability.

## Statements

Raven is primarily **expression-oriented**: most constructs yield values and can
appear wherever an expression is expected. For details on statement forms,
terminators, and control-flow constructs, see [Control flow](control-flow.md).
Structured exception handling is covered in [Error handling](error-handling.md).

### Variable bindings

`let` introduces an immutable lexical binding, `var` introduces a mutable one, and `const`
produces an immutable binding whose value is baked in at compile time. A binding may
declare its type explicitly or rely on the compiler to infer it from the initializer
expression.

A lexical binding is a declaration statement, not an expression. Its
initializer is an expression whose value is assigned to the newly introduced
name. For example, in `let x = 2 + a`, `2 + a` is an expression, but the whole
construct is a statement. Raven has no expression form of that construct that
introduces `x` into the surrounding lexical scope:

```raven
let x = 2 + a

let answer = 42         // inferred int

var name = "Alice"    // inferred string, mutable

const greeting = "Hi"  // inferred string constant

let count: long = 0     // explicit type
```

Standard Raven style prefers `let` for immutable lexical bindings. `val`
remains accepted for source compatibility and is the semantic display spelling
for a read-only binding.

If the type annotation is omitted, an initializer is required so the compiler can
determine the variable's type. Const bindings always require an initializer, even when
annotated, and the expression must be a .NET compile-time constant (numeric and
character literals, `true`/`false`, strings, or `null`).

Value-producing forms such as `if` and `match` can be expressions. They also
have statement forms when used for control flow without passing a value to a
surrounding expression. `while`, `for`, and `loop` are statements in the
current language.

Later declarations in the same scope may **shadow** earlier bindings. Each declaration
introduces a new symbol; code that follows binds to the most recent declaration.
Shadowing is permitted for both `let` and `var` bindings, but it produces the
warning diagnostic `RAV0168` to help catch unintentional redeclarations. Parameters of
the enclosing function count as previous declarations for this purpose, so a local that
reuses a parameter name both shadows it and triggers the same warning.

```raven
let answer = 41
let answer = answer + 1 // RAV0168 (warning)
```

### File-scope code

File-scope code is supported—no `Main` function is required.

```raven
import System.*
alias print = System.Console.WriteLine

sayHello()

func sayHello() {
    print("Hello, World!")
}
```
