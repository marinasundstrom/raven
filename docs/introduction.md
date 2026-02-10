# Raven

Raven is a modern, expression-oriented programming language for the .NET platform.  
It keeps everyday code **compact, explicit, and safe**, while still feeling familiar to developers coming from **C#**, **Swift**, **Kotlin**, and **F#**.

Raven is built around a few simple ideas:

- **Expressions first** — most constructs produce values.  
- **Declarative composition** — build values and object graphs directly in code.  
- **Pattern matching** — matching is a first-class way to test and extract structure.  
- **Explicit failure** — recoverable errors are modeled as data.  
- **Propagation** — unwrap `Result` and `Option` with `?` to short-circuit on failure.
- **Carrier-first pipelines** — LINQ-style helpers over `Option`/`Result` keep flows linear.

Raven encourages you to make mutability, flow, and error handling explicit. The compiler helps keep code honest with built-in diagnostics and analyzers.

> ℹ️ **Note:** Raven has no `void`. Instead it uses `unit`, a real type with the single value `()`.  
> When a `unit`-returning function is emitted to .NET, it becomes `void`.

This page is a quick tour of Raven’s style and core features.

---

## Hello, World!

Raven doesn’t require a specific entry-point shape like C#’s `static void Main`.  
You can write file-scope code (top-level statements), and the compiler synthesizes the correct .NET entry point.

```raven
import System.Console.*

WriteLine("Hello, World!")
```

> ℹ️ **Note:** In a file, executable top-level statements must appear **before** type declarations.  
> Top-level `func` declarations are treated as statements too, but they are **hoisted**, so you may call them before their textual declaration within the file-scope region.

---

## A quick taste

Here’s a small sample that shows Raven in its natural style:  
**expressions everywhere, explicit failure with `Result`, and lightweight propagation using `?`.**

```raven
import System.*
import System.Console.*

func Main() -> () {
    val inputs = ["10", "3", "abc", "42"]

    ProcessNumbers(inputs)
        .Map(_ => "All numbers processed")
        .UnwrapOrElse(err => "Failed: $err")
        |> WriteLine
}

func ProcessNumbers(inputs: string[]) -> Result<(), string> {
    for text in inputs {
        // Propagate on failure using `?`
        val no = parseInt(text)?

        // Pattern matching as an expression
        val message = no match {
            10 => "Ten exactly!"
            val value => "Parsed: $value"
        }

        WriteLine(message)
    }

    return .Ok(())
}

func parseInt(text: string) -> Result<int, string> {
    return try int.Parse(text)
        .Map(v => v)                     // carrier-aware transform
        .OrElse(_ => .Error("\"$text\" is not a number"))
}
```

This short example highlights several core ideas:

- **`?` propagation** — failures return early without ceremony  
- **`Map` / `OrElse`** — transform or recover without leaving expression world  
- **Pattern matching** — clean branching on values  
- **No exceptions for flow control** — errors are just data  

Raven code tends to read like a pipeline of intent rather than nested control structures.

---

## Expressions

Raven supports both block expressions and block statements.

- A **block expression** evaluates to the value of its last expression.
- A **block statement** is effect-only; any expression values inside it are discarded.

```raven
val x = {
    val a = 10
    val b = 20
    a + b
}
// x = 30
```

An empty block evaluates to `()`.

### Bindings and mutability

Bindings are explicit:

- `val` — **immutable** binding  
- `var` — **mutable** binding

```raven
val name = "Raven"
// name = "Other"   // error

var count = 0
count = count + 1
```

### `if` as an expression

`if` returns a value in expression position.

```raven
val pet = if flag { Dog() } else { Cat() }
// pet : Dog | Cat
```

When used as a statement, `if` is effect-only:

```raven
func tap<T>(opt: Option<T>, action: T -> ()) -> Option<T> {
    if opt is .Some(val value) {
        action(value)   // value discarded in statement context
    }
    return opt
}
```

When branches yield different types, Raven infers a **union** instead of collapsing to a base type.

### Collection expressions and spread

Collection expressions use brackets with `..` spreads to inline sequences.

```raven
val xs = [1, 2, 3]
val ys = [0, ..xs, 4]
// ys = [0, 1, 2, 3, 4]
```

Collections are **target-typed**:

- If the context expects an **array** (`T[]`), the expression produces an array.  
- If it expects a **collection type** (such as `List<T>`), Raven constructs that collection.  
- With **no target type**, Raven defaults to an array.

```raven
import System.Collections.Generic.*

val values: List<int> = [1, 2, 3]
```

> ❗ **Tip:** `[]` needs a target type so Raven knows what to construct.

---

## Type system

Raven’s type system is practical and safe-by-default, designed for smooth .NET interop.

- **Built-in types** — `int`, `long`, `byte`, `double`, `char`, `bool`, `string`, and more  
- **Arrays** — fixed-size contiguous collections  
- **User-defined types** — classes, interfaces, records, enums, unions  
- **Nullability** — unified and enabled by default with flow analysis  

> ❗ **Prefer `Result` and `Option`** — model failure and optional values explicitly; use `T?` mainly for .NET interop.

---

## Functions

Functions are first-class values: you can declare them, pass them around, and store them in variables.

Raven distinguishes between:

- **Function declarations** (`func`) — named functions  
- **Function expressions** (lambdas) — values created inline

Behind the scenes, function values map to .NET delegates.

> ℹ️ **Note:** A `func` declaration never captures locals (not a closure).  
> A lambda **can** capture values from the surrounding scope.

### Function declarations

```raven
func add(a: int, b: int) -> int => a + b

func greet(name: string, punct: string = "!") {
    WriteLine("Hello $name$punct")
}
```

### Lambdas

```raven
val a = 42
val addA = (x: int) => x + a

addA(1) // 43
```

### Passing functions

```raven
func applyTwice(f: int -> int, x: int) -> int => f(f(x))

val plusOne = (x: int) => x + 1
applyTwice(plusOne, 10) // 12
```

---

## Extensions and traits

Extensions add members to existing types.

```raven
extension StringExt for string {
    ToSlug() -> string =>
        self.Trim().ToLowerInvariant().Replace(" ", "-")
}

import MyApp.StringExt.*

val slug = "Hello World".ToSlug()
```

Extensions can also provide **static** members and DSL-style APIs without modifying the original type.

---

## Pattern matching

Patterns test structure and extract data at the same time.

```raven
val label = character match {
    { Species: .Dog } => "Dog"
    { Species: .Human } => "Human"
    _ => "Other"
}
```

### Binding values

```raven
if result is .Ok(val value) {
    WriteLine(value)
}
```

---

## Result and Option

Raven models recoverable failure as data. In practice, `Option<T>` and
`Result<T, E>` are not isolated features; they are the primary carriers for
flow-oriented APIs and extension-method pipelines.

### Result

```raven
func Divide(a: int, b: int) -> Result<int, string> {
    if b == 0 { return .Error("Division by zero") }
    return .Ok(a / b)
}
```

Propagation with `?` returns early on error:

```raven
val value = Divide(a, b)?
```

### Option

```raven
func FindFirstEven(values: int[]) -> Option<int> { ... }
```

`?` also propagates `.None`.

### LINQ-style helpers on flow carriers

Raven encourages extension methods that make `Option`/`Result` feel like native
pipeline types. Typical helpers include `Map`, `Then`, `OrElse`, `UnwrapOr`,
`FirstOrNone`, and `FirstOrError`.

```raven
import System.Linq.*

func ResolvePrimaryEmail(users: User[]) -> Result<string, string> {
    return users
        .Where(u => u.IsActive)
        .Select(u => u.Email)
        .FirstOrError(() => "No active users")?
        .Trim()
        .ToLowerInvariant()
}
```

This style keeps success and failure paths explicit while avoiding nested
control flow.

---

## Exception handling

`try expr` converts exceptions to `Result<T, Exception>`.

```raven
val value = try int.Parse(text) match {
    .Ok(val v) => v
    .Error(_) => 0
}
```

`try?` combines `try` with propagation.

---

## Async and await

```raven
async func DownloadLength(url: string) -> Task<int> {
    use http = HttpClient()
    val text = await http.GetStringAsync(url)
    text.Length
}
```

---

## Declarative programming

### Object initializer trailers

```raven
val window = Window {
    Title = "Main"
    Width = 800

    Button { Text = "OK" }
    Button { Text = "Cancel" }
}
```

---

## .NET interop

```raven
import System.Console.*
import System.Collections.Generic.*

val numbers: List<int> = [1, 2, 3]
WriteLine(numbers.Count)
```

---

## Object-oriented programming

Raven supports classes, interfaces, methods, properties, and events alongside unions and pattern matching.

---

## Where to go next

- **Language specification:** `docs/lang/spec/language-specification.md`  
- **Samples:** `samples/`  
- **.NET mapping:** `docs/lang/dotnet-implementation.md`

> ⚠️ This is a living document and may change.
