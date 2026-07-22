# Raven programming language

This section covers Raven as it exists today: a .NET language with an
expression-oriented style, explicit mutability, pattern matching,
`Option`/`Result`-based flow, records and primary constructors,
extensions/traits, and direct .NET interop.

## Start here

* [Introduction](../introduction.md)
* [Raven for absolute beginners](../raven-for-absolute-beginners.md)
* [Raven for C# developers](../raven-for-csharp-developers.md)
* [Language philosophy](philosophy.md)
* [Domain modeling](domain-modeling.md)
* [Language reference](spec/language-specification.md)
* [Type system](spec/type-system.md)
* [Style guide](style-guide.md)

## Language topics

Use the [language reference index](spec/language-specification.md) for the
complete topic list. The reference is grouped into recognizable areas:

* [Language fundamentals](spec/language-specification.md#language-fundamentals)
* [Types](spec/language-specification.md#types)
* [Expressions](spec/language-specification.md#expressions)
* [Declarations and members](spec/language-specification.md#declarations-and-members)
* [Metaprogramming](spec/language-specification.md#metaprogramming)
* [Pattern matching](spec/language-specification.md#pattern-matching)
* [Control flow](spec/language-specification.md#control-flow)
* [Error handling](spec/language-specification.md#error-handling)
* [.NET interoperability](spec/language-specification.md#net-interoperability)

## Current shape in one screen

```raven
import System.Console.*

func Main() -> () {
    let result = match ParsePort("8080") {
        Ok(let port) => "Listening on $port"
        Error(let err) => "Invalid port: $err"
    }

    WriteLine(result)
}

func ParsePort(text: string) -> Result<int, string> {
    return int.Parse(text) match {
        Ok(let value) => Ok(value)
        Error(_) => Error("not a number")
    }
}
```

Use the introduction for a guided overview and the language reference for
feature documentation and the common rules developers need in practice.

Language proposals and compiler development notes are maintained separately in
the source repository. They are useful to contributors, but do not define the
user-facing language documentation.
