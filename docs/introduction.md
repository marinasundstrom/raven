# Raven Programming Language

Raven is a general-purpose programming language.

It mainly draws inspiration from Swift, Rust, and Kotlin in its syntax, and features from functional programming languages like F#.

It is expression-oriented and expression-first but supports imperative constructs when those are preferred. It also has a number of functional programming constructs, but it is an impure language and supports OOP.

The type system is flexible and supports type unions and literals as their own types. Types flow with expressions, similar to TypeScript. The language uses `unit` instead of `void`.

Since Raven is a .NET language, it supports interop with C#, projecting Raven types into C# and back again. There is also a C# analyzer for type unions.

The compiler is based on the Roslyn compiler architecture, which provides compiler-as-a-service and tooling such as analyzers.

## String interpolation example

In Raven, `${...}` interpolation works inside ordinary quoted strings—there is no `$` prefix like C#'s interpolated strings. For example:

```raven
let name = "Avery"
let product = "Raven"
let message = "Hello ${name}, welcome to ${product}!"
```

The compiler substitutes the embedded expressions, so `message` evaluates to `Hello Avery, welcome to Raven!`.

The `string-interpolation.rav` sample provides a second example with right-to-left text. Running it produces the greeting `שלום דניאל! ברוך הבא לתל אביב`, showing that interpolation works correctly even when the output mixes RTL and Latin characters.

