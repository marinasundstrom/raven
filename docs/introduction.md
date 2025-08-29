# Raven Programming Language

Raven is a general-purpose programming language.

It mainly draws inspiration from Swift, Rust, and Kotlin in its syntax, and features from functional programming languages like F#.

It is expression-based but supports imperative constructs when those are preferred. It also has a number of functional programming constructs, but it is an impure language and supports OOP.

The type system is flexible and supports type unions and literals as their own types. Types flow with expressions, similar to TypeScript. The language uses `unit` instead of `void`.

Since Raven is a .NET language, it supports interop with C#, projecting Raven types into C# and back again. There is also a C# analyzer for type unions.

The compiler is based on the Roslyn compiler architecture, which provides compiler-as-a-service and tooling such as analyzers.

