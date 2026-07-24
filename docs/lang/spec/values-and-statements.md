# Values and statements

Most Raven code produces values, including many control-flow forms. Statements
are used when code performs an action without needing to keep a result.

Raven has no `void` type. The absence of a meaningful value is represented by the
`unit` type, which has exactly one value written `()`. The type itself may be
spelled `unit` or `()`. Functions without an explicit return type implicitly
return `unit`. In .NET, `unit` corresponds to `void` (see [implementation notes](dotnet-implementation.md#unit-type)). The `unit` type participates in generics and tuples like any other type.

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

```raven
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

Control-flow constructs such as `if`, `while`, and `for` are expressions whose
statement forms are described in [Control flow](control-flow.md).

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
