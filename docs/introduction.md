# Raven Programming Language

Raven is a general-purpose programming language.

It mainly draws inspiration from Swift, Rust, and Kotlin in its syntax, and features from functional programming languages like F#.

It is expression-oriented and expression-first but supports imperative constructs when those are preferred. It also has a number of functional programming constructs, but it is an impure language and supports OOP.

The type system is flexible and supports type unions and literals as their own types. Types flow with expressions, similar to TypeScript. The language uses `unit` instead of `void`.

Since Raven is a .NET language, it supports interop with C#, projecting Raven types into C# and back again. There is also a C# analyzer for type unions.

The compiler is based on the Roslyn compiler architecture, which provides compiler-as-a-service and tooling such as analyzers.

## String interpolation example

The `string-interpolation.rav` sample demonstrates Raven's `${...}` interpolation syntax. Unlike C#, Raven string literals do not require a `$` prefix to enable interpolation—the `${...}` form can appear in any ordinary quoted string. Running that sample with the compiler produces the greeting `שלום דניאל! ברוך הבא לתל אביב`, showing that the embedded expressions are substituted correctly and preserve the right-to-left text flow in the output.

