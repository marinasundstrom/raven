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
* [Language specification](spec/language-specification.md)
* [Type system](spec/type-system.md)
* [Style guide](style-guide.md)

## Specification chapters

* [Core language specification](spec/language-specification.md)
* [Lexical structure](spec/lexical-structure.md)
* [Values and statements](spec/values-and-statements.md)
* [Expressions and type inference](spec/expressions-and-inference.md)
* [Async and error propagation](spec/async-and-error-propagation.md)
* [Fundamental expressions](spec/fundamental-expressions.md)
* [Collection expressions](spec/collection-expressions.md)
* [Function invocation](spec/invocations.md)
* [Extensions and traits](spec/extensions.md)
* [Operators and object creation](spec/operators-and-object-creation.md)
* [Control-flow expressions and statements](spec/control-flow-expressions.md)
* [Pattern matching](spec/pattern-matching.md)
    * [Dictionary patterns and exhaustiveness](spec/dictionary-patterns-and-exhaustiveness.md)
    * [Match forms](spec/match-forms.md)
    * [Pattern forms and contexts](spec/pattern-contexts.md)
    * [Fundamental patterns](spec/fundamental-patterns.md)
    * [Sequence and property patterns](spec/sequence-and-property-patterns.md)
    * [Deconstruction and union patterns](spec/deconstruction-and-union-patterns.md)
* [Namespaces, imports, and aliases](spec/namespaces-and-imports.md)
* [Enum declarations](spec/enum-declarations.md)
* [Top-level code and entry points](spec/top-level-code-and-entry-points.md)
* [Delegate declarations](spec/delegate-declarations.md)
* [Functions](spec/functions.md)
* [Unsafe code and interoperability](spec/unsafe-code-and-interop.md)
* [Local declarations](spec/local-declarations.md)
* [Types and unions](spec/types-and-unions.md)
* [Object-oriented types and operator precedence](spec/object-oriented-types.md)
* [Types and conversions](spec/type-system.md)
* [Classes, structs, and interfaces](spec/classes-and-members.md)
    * [Type declarations and initialization](spec/type-declarations-and-initialization.md)
    * [Properties and events](spec/properties-and-events.md)
    * [Inheritance and partial types](spec/inheritance-and-partial-types.md)
    * [Parameters, overloading, and operators](spec/parameters-overloading-and-operators.md)
    * [Interfaces](spec/interfaces.md)
* [Control flow](spec/control-flow.md)
    * [Assignment and expression statements](spec/assignment-and-expression-statements.md)
    * [Match statements](spec/match-statements.md)
    * [Return and yield](spec/returns-and-yield.md)
    * [Jumps and labels](spec/jumps-and-labels.md)
* [Error handling](spec/error-handling.md)
* [.NET implementation notes](spec/dotnet-implementation.md)
* [Macros](spec/macros.md)
* [Grammar (EBNF)](spec/grammar.ebnf)

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
    return try int.Parse(text) match {
        Ok(let value) => Ok(value)
        Error(_) => Error("not a number")
    }
}
```

Use the introduction for a guided overview and the specification chapters for
precise language rules.

Language proposals and compiler development notes are maintained separately in
the source repository. They are useful to contributors, but do not define the
user-facing language documentation.
