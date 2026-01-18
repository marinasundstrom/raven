# Raven

Raven is a modern, expression-oriented programming language for the .NET platform.
It keeps everyday code **compact, explicit, and safe**, while still feeling familiar to developers coming from **C#**, **Swift**, **Kotlin**, and **F#**.

Raven is built around a few simple ideas:

- **Expressions first** — most constructs produce values.
- **Composability** — functions and small building blocks are encouraged.
- **Pattern matching** — matching is a first-class way to test and extract structure.
- **Explicit failure** — recoverable errors are modeled as data.

Raven encourages you to make mutability, flow, and error-handling explicit. The compiler helps keep code honest with built-in diagnostics and analyzers.

> ℹ️ **Note:** Raven has no `void`. Instead it uses `unit`, a real type with the single value `()`. When a `unit`-returning function is emitted to .NET, it becomes `void`.

This page is a quick tour of Raven’s style and core features.

---

## Hello, World!

Raven doesn’t require a specific entry point shape like C#’s `static void Main`.
You can write file-scope code (top-level statements), and the compiler synthesizes the correct .NET entry point.

```raven
import System.Console.*

WriteLine("Hello, World!")
```

> ℹ️ **Note:** In a file, any executable top-level statements must appear **before** type declarations.
>
> Top-level `func` declarations are treated as statements too, but they are **hoisted**, so you may call them before their textual declaration within the file-scope region.

---

## Expressions

In Raven, blocks are expressions: they evaluate to the value of their last expression.

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

- `val` creates an **immutable** binding.
- `var` creates a **mutable** binding.

```raven
val name = "Raven"
// name = "Other"   // error

var count = 0
count = count + 1
```

### `if` as an expression

`if` returns a value.

```raven
val pet = if flag { Dog() } else { Cat() }
// pet : Dog | Cat
```

When branches yield different types, Raven infers a **union** instead of collapsing to a base type like `Animal`.

### Collection expressions and spread

Collection expressions use brackets, with `..` spreads to inline sequences.

```raven
val xs = [1, 2, 3]
val ys = [0, ..xs, 4]
// ys = [0, 1, 2, 3, 4]
```

Collections are **target-typed**:

- If the surrounding context expects an **array** (`T[]`), the expression produces an array.
- If it expects a **collection type** (such as `List<T>`), Raven constructs that collection.
- If there is **no target type**, Raven defaults to producing an array.

```raven
import System.Collections.Generic.*

val values: List<int> = [1, 2, 3]
// values is a List<int> (not an array)
```

> ❗ **Tip:** `[]` needs a target type (like `int[]` or `List<int>`) so Raven knows what to construct.

---
## Type system

Raven’s type system is practical and safe-by-default, while still designed for smooth .NET interop.

### Nullability

Nullability is a unified feature in Raven’s type system and is **enabled by default**.

- `T` is non-nullable
- `T?` allows `null`

Raven performs **control-flow analysis** and produces diagnostics when values may be `null`, helping you avoid null-reference bugs early.

### Prefer `Option<T>` for optional values

> ↪️ **Read more:** [Result and Option](#user-content-result-and-option)

In Raven code, prefer `Option<T>` for optional values rather than relying on `T?`.

Use nullable types mainly for **.NET interop**, where `null` is the standard representation of “no value”.

Raven provides an implicit conversion from `Option<T>` to `T?` so optional values can be passed to APIs that expect `null`:

```raven
val name: Option<string> = .Some("Raven")

val text: string? = name // implicit Option<T> -> T?
```

---

## Functions

Functions are first-class values: you can declare them, pass them around, and store them in variables.

Raven distinguishes between two related concepts:

- **Function declarations** (`func`) — statements that declare a named function.
- **Function expressions** (lambdas) — expressions that produce a function value.

Behind the scenes, Raven represents function values as .NET delegates (`Action`, `Func`, or synthesized delegate types).

> ℹ️ **Note:** A `func` declaration never captures locals (it is not a closure). A lambda **can** capture values from the surrounding scope.

### Function declarations (`func`)

```raven
import System.Console.*

func add(a: int, b: int) -> int => a + b

func greet(name: string, punct: string = "!") {
    WriteLine("Hello $name$punct")
}

WriteLine(add(2, 3))
greet("Raven")
```

You can write either a block body or a concise expression body with `=>`.

Local `func` declarations are allowed, but they are **not closures**:

```raven
func outer(x: int) -> int {
    func inner(y: int) -> int => y + 1
    inner(x)
}
```

### Lambdas (function expressions)

Lambda expressions create function values inline.
They **are closures**, meaning they can capture values from the surrounding scope.

```raven
val a = 42
val addA = (x: int) => x + a

addA(1) // 43
```

### Passing functions as values

```raven
import System.Console.*

func applyTwice(f: int -> int, x: int) -> int => f(f(x))

val plusOne = (x: int) => x + 1
val result = applyTwice(plusOne, 10)

WriteLine(result) // 12
```

> ❗ **Interop note:** Raven function types map to .NET delegates when possible (`Func`/`Action`).
>
> When a function type doesn’t fit a predefined delegate shape (for example due to `ref`/`out` parameters), Raven synthesizes a compatible delegate type so interop remains smooth.

---

## Pattern matching

Patterns let you test structure and extract data at the same time.

```raven
import System.Console.*

val character = Character("Rex", .Dog, 5)

val label = character match {
    { Species: .Dog } => "Dog"
    { Species: .Human } => "Human"
    _ => "Other"
}

WriteLine(label)

enum Species { Human, Dog }
record class Character(Name: string, Species: Species, Age: int)
```

> ❗ **Tip:** In patterns, a bare identifier is a constant/value pattern. New bindings require `val`, `var`, or `let`.

### Binding values in patterns

To extract a value, introduce a binding in the pattern.

```raven
if result is .Ok(val value) {
    WriteLine(value)
}
```

You can also bind with a type when you want to both test and extract:

```raven
if err is FormatException ex {
    WriteLine(ex.Message)
}
```

Bindings introduced by a pattern exist only in the **success scope** of that pattern.

### Property patterns

```raven
import System.Console.*

if character is { Age: not > 34, Species: .Dog, Name: val name } {
    WriteLine("Dog under 35: $name")
}
```

### Record (positional) patterns

Record patterns match by positional structure.

```raven
if character is Character(val name, _, val age) {
    if age >= 35 {
        WriteLine("35+: $name")
    }
}
```

Positional patterns also work for tuples:

```raven
val point = (10, 20)

if point is (val x, val y) {
    WriteLine("x = $x, y = $y")
}
```

---

## Result and Option

Raven encourages modeling recoverable failure as data instead of relying on exceptions.
Two common shapes are **Result** (success vs error) and **Option** (some value vs none).

### Result

Use `Result<T, E>` to return either a value or an error payload.

```raven
import System.Console.*

func Divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        return .Error("Division by zero")
    }

    return .Ok(a / b)
}

val message = Divide(10, 2) match {
    .Ok(val value) => "Ok: $value"
    .Error(val err) => "Error: $err"
}

WriteLine(message)
```

### Option

Use `Option<T>` to represent an optional value without using `null`.

```raven
import System.Console.*

func FindFirstEven(values: int[]) -> Option<int> {
    for value in values {
        if value % 2 == 0 {
            return .Some(value)
        }
    }

    return .None
}

val label = FindFirstEven([1, 3, 4, 5]) match {
    .Some(val value) => "First even: $value"
    .None => "No even numbers"
}

WriteLine(label)
```

> ℹ️ **Note:** `match` over unions is exhaustive. If you miss a case, the compiler guides you with diagnostics.

### Defining your own union

```raven
import System.Console.*

union Lookup {
    Found(id: int)
    Missing
    Unauthorized(reason: string)
}

func Describe(value: Lookup) -> string {
    return value match {
        .Found(val id) => "Found $id"
        .Missing => "Missing"
        .Unauthorized(val reason) => "Unauthorized: $reason"
    }
}

WriteLine(Describe(.Found(42)))
```

---

## Exception handling

Raven prefers `Result` and `Option` for recoverable failure, but it also interops naturally with .NET APIs that throw exceptions.

A `try expr` evaluates `expr` and returns a `Result<T, Exception>`:

- `.Ok(value)` when the expression succeeds
- `.Error(exception)` when it throws

### Capturing exceptions with `try`

```raven
import System.Console.*

func ParseInt(text: string) -> string {
    return try int.Parse(text) match {
        .Ok(val value) => "Parsed: $value"
        .Error(FormatException ex) => "Invalid format: ${ex.Message}"
        .Error(_) => "Other error"
    }
}

WriteLine(ParseInt("123"))
WriteLine(ParseInt("foo"))
```

### `try` returning `unit`

```raven
import System.*

func SaveFile(path: string, text: string) -> string {
    return try {
        System.IO.File.WriteAllText(path, text)
    } match {
        .Ok => "Saved!"
        .Error(UnauthorizedAccessException ex) => "Access denied: ${ex.Message}"
        .Error(IOException ex) => "I/O error: ${ex.Message}"
        .Error(_) => "Unknown error"
    }
}
```

When the operand produces no value, the success case is `.Ok(())`. A bare `.Ok` arm is sugar for `.Ok(())`.

---

## Async and await

Async code uses `async` and `await`, and integrates with .NET `Task`/`Task<T>`.

```raven
import System.Net.Http.*
import System.Threading.Tasks.*

async func DownloadLength(url: string) -> Task<int> {
    val http = HttpClient()
    val text = await http.GetStringAsync(url)
    return text.Length
}
```

> ℹ️ **Note:** `await` is an expression, so it composes naturally with other expression-based constructs.

---

## .NET interop

Raven is designed to feel natural on top of the .NET ecosystem:

```raven
import System.Console.*
import System.Collections.Generic.*

val numbers: List<int> = [1, 2, 3]
WriteLine(numbers.Count)

val now = System.DateTime.UtcNow
WriteLine(now)
```

> ❗ **Tip:** `import Some.Type.*` can bring static members into scope (similar to C# `using static`).

---
## Object-oriented programming

Raven supports object-oriented programming alongside its expression-oriented design:

- classes
- interfaces
- methods
- properties
- events

This makes it natural to build traditional .NET-style libraries and applications, while still benefiting from Raven’s union types and pattern matching.

---

## Where to go next

- Read the full **Language specification**: `docs/lang/spec/language-specification.md`
- Browse runnable programs in `samples/`
- See how Raven maps to .NET in `docs/lang/dotnet-implementation.md`

> ⚠️ Raven is a living project. The syntax and semantics are evolving.