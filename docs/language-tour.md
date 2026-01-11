# Raven Language Tour

This tour introduces Raven through small, focused examples. Raven is a statically typed, expression-oriented language designed for seamless .NET interoperability, with modern features such as union types, pattern matching, and flow-sensitive typing.

---

## Hello, Raven

Raven supports file-scope code. No `Main` boilerplate is required.

```raven
import System.Console.*

WriteLine("Hello, Raven!")
```

Functions declared in file scope are hoisted and may be used anywhere in the file.

---

## Variables and bindings

Raven distinguishes between immutable and mutable bindings.

```raven
let answer = 42        // immutable
var count = 0          // mutable
const pi = 3.14159     // compile-time constant
```

Types are inferred unless annotated:

```raven
let name: string = "Alice"
var total: long = 0
```

Shadowing is allowed but produces a warning to catch mistakes.

---

## Expression-oriented control flow

Most constructs are expressions and produce values.

```raven
let result =
    if x > 0 {
        x * 2
    } else {
        0
    }
```

Blocks are expressions; their value is the last expression (or `()` if none).

```raven
let value = {
    let a = 10
    let b = 20
    a + b
}
```

The absence of a meaningful value is represented by `unit` (`()`), not `void`.

---

## Functions and lambdas

Functions are declared with `func` and may use block or arrow bodies.

```raven
func add(a: int, b: int) -> int {
    a + b
}

func square(x: int) -> int => x * x
```

Lambdas are target-typed and concise:

```raven
let inc: int -> int = x => x + 1
```

Functions and methods are first-class values:

```raven
let log: string -> () = Console.WriteLine
log("Hello")
```

---

## Union types and type inference

When control flow produces different types, Raven infers a **union**.

```raven
let value =
    if flag {
        42
    } else {
        "unknown"
    }
// value : int | string
```

Unions preserve information instead of collapsing to `object`.

Assignments from unions require every branch to be compatible:

```raven
let u = if flag { 1 } else { 2.0 }   // int | double
let x: int = u      // error
let y: object = u   // ok
```

---

## Literal types

Literals can act as singleton types and combine naturally with unions.

```raven
let mode: "on" | "off" = "on"
```

Literals widen automatically when needed:

```raven
let n = 1        // inferred as int
let d: double = 1
```

This enables finite domains without enums.

---

## Pattern matching and flow narrowing

Patterns appear in `is` expressions and `match`.

```raven
if value is int n {
    WriteLine(n + 1)
} else if value is "unknown" {
    WriteLine("no value")
}
```

Patterns narrow types **flow-sensitively** inside their scope.

### Match expressions

```raven
let description =
    token match {
        .Identifier(text) => $"id: ${text}"
        .Number(n)        => $"num: ${n}"
        .Unknown          => "?"
    }
```

The result type is inferred as the union of all arm results.

---

## Discriminated unions

Raven supports declared algebraic data types.

```raven
union Result<T> {
    Ok(value: T)
    Error(message: string)
}
```

Cases are real CLR structs and convert implicitly to the union:

```raven
let r: Result<int> = .Ok(42)
```

Pattern matching is exhaustive:

```raven
r match {
    .Ok(v)    => WriteLine(v)
    .Error(e) => WriteLine($"error: ${e}")
}
```

---

## Nullability and safe access

Nullability is explicit for both reference and value types.

```raven
let text: string? = null
```

Null-conditional access works for members, calls, and indexing:

```raven
let length = text?.Length
let result = f?(2)
let first  = array?[0]
```

Flow analysis recognizes null checks:

```raven
if text != null {
    WriteLine(text.Length) // safe
}
```

---

## Object creation and invocation

Creating objects is just calling the type name:

```raven
let list = List<int>()
list.Add(1)
```

`new` is optional and explicit:

```raven
let sb = new StringBuilder()
```

Types may define a `self` method to become callable:

```raven
let result = multiplier(5)
```

---

## Extensions and pipelines

Extensions add methods and properties without modifying the original type.

```raven
extension StringExt for string {
    Words() -> int {
        self.Split(' ').Length
    }
}
```

```raven
import StringExt.*

"hello world".Words()
```

The pipe operator enables fluent composition:

```raven
let result =
    5
    |> Square()
    |> AddOne()
```

---

## Async and await

Async functions look familiar and compile to proper CLR state machines.

```raven
async func load() -> string {
    let text = await File.ReadAllTextAsync("data.txt")
    text
}
```

Return types are inferred as `Task` or `Task<T>` when omitted.

---

## Interop with .NET

Raven consumes .NET libraries directly:

```raven
let ids: Guid[] = [Guid.NewGuid()]
Console.WriteLine(ids[0])
```

Delegates, generics, async, and reflection all behave as expected.

---

## Summary

Raven combines:

* **CLR-faithful typing**
* **Expression-oriented syntax**
* **Union types and pattern matching**
* **Explicit nullability**
* **First-class functions**
* **Seamless .NET interop**

…into a language that is precise, modern, and practical for real .NET systems.

---

Here’s a motivating sample that shows Raven’s “sweet spot”: **CLR-native code that stays honest about types**, with **Result/Option-style modeling**, **pattern matching**, **pipelines**, **extensions**, and **file-scope entry**—without turning into ceremony.