# Raven language reference

Raven is an expression-oriented language for .NET. It combines concise
functions and immutable data with classes, pattern matching, async programming,
and direct access to the .NET ecosystem.

```raven
import System.*

record Temperature(Celsius: double)

func describe(value: Temperature) -> string {
    value.Celsius match {
        ..<0 => "freezing"
        0..<20 => "cold"
        20..<30 => "comfortable"
        _ => "hot"
    }
}

let reading = Temperature(21.5)
Console.WriteLine(describe(reading))
```

This reference explains Raven by feature: what a construct is for, how to use
it, and the rules that matter when code becomes more involved. It serves both
as developer documentation and as Raven's working language specification.

The reference is precise where a distinction affects normal programs, .NET
interoperability, or compiler diagnostics. It does not attempt to enumerate
every compiler implementation detail or every possible invalid program.

## Where to start

If you are new to Raven, begin with [Values, expressions, and
statements](values-and-statements.md),
[Functions](functions.md), and [Unions](unions.md).
Together
they introduce Raven's expression-oriented style, callable values, records,
unions, and the `unit` value.

If you already use C# or another .NET language, start with the [Type
system](type-system.md), [Classes, structs, and
interfaces](classes-and-members.md), and [.NET implementation
notes](dotnet-implementation.md). These pages show where Raven follows the CLR
model and where its source-language conventions differ.

For a formal view of the syntax, see the [non-normative EBNF
grammar](grammar.ebnf). Contextual and semantic rules remain in the feature
articles.

## Language fundamentals

Learn how Raven source is written and organized, how values and statements
behave, and how an executable program starts.

* [Lexical structure](lexical-structure.md)
* [Values, expressions, and statements](values-and-statements.md)
* [Namespaces, imports, and aliases](namespaces-and-imports.md)
* [Top-level code and entry points](top-level-code-and-entry-points.md)
* [Grammar](grammar.md)

## Types

Explore Raven's built-in and .NET types, nullability, tuples, function types,
unions, enums, and delegates. Unions remain a distinct feature because their
closed alternatives, payloads, and exhaustiveness rules form a complete
data-modeling construct rather than merely another annotation syntax.

* [Type system](type-system.md)
* [Unions](unions.md)
* [Enum declarations](enum-declarations.md)
* [Delegate declarations](delegate-declarations.md)

## Expressions

Expressions produce values. These articles cover inference, literals, calls,
collections, tuples, pipelines, construction, and operators.

* [Expressions and type inference](expressions-and-inference.md)
* [Literals and fundamental expressions](fundamental-expressions.md)
* [Collection expressions](collection-expressions.md)
* [Calls](invocations.md)
* [Pipe expressions](pipe-expressions.md)
* [Object creation and copying](object-creation.md)
* [Operators](operators.md)

## Declarations and members

Define local values, reusable functions, and object-oriented types. These pages
also cover generic constraints, overloads, properties, events, inheritance, and
extension members.

* [Local declarations](local-declarations.md)
* [Functions](functions.md)
* [Async functions](async-functions.md)
* [Classes, structs, and interfaces](classes-and-members.md)
* [Type declarations and initialization](type-declarations-and-initialization.md)
* [Properties and events](properties-and-events.md)
* [Inheritance and partial types](inheritance-and-partial-types.md)
* [Parameters, overloading, and operators](parameters-overloading-and-operators.md)
* [Interfaces](interfaces.md)
* [Extension members](extensions.md)

## Pattern matching

Pattern matching tests the shape and contents of a value while optionally
binding its parts. Raven supports constants, ranges, types, sequences,
properties, dictionaries, union cases, and nested combinations of patterns.

* [Pattern matching](pattern-matching.md)
* [Match forms](match-forms.md)
* [Fundamental patterns](fundamental-patterns.md)
* [Sequence and property patterns](sequence-and-property-patterns.md)
* [Deconstruction, member, and union patterns](deconstruction-and-union-patterns.md)
* [Dictionary patterns](dictionary-patterns.md)
* [Match exhaustiveness](match-exhaustiveness.md)

## Control flow and errors

Branch, iterate, return, yield, jump, synchronize, and represent failure either
as an exception or as an explicit carrier value.

* [Control flow](control-flow.md)
* [Conditionals and loops](control-flow-expressions.md)
* [Statements](assignment-and-expression-statements.md)
* [Return and yield](returns-and-yield.md)
* [Jumps and labels](jumps-and-labels.md)
* [Exceptions and structured handling](error-handling.md)
* [Error propagation and carrier types](async-and-error-propagation.md)

## Metaprogramming

Raven macros transform syntax at compile time and can provide domain-specific
language forms while remaining integrated with the compiler.

* [Macros](macros.md)

## Systems programming and .NET interoperability

Use spans, stack allocation, managed references, pointers, unsafe code, and
external declarations when working close to memory or a native boundary. The
implementation notes describe how Raven concepts are represented on .NET.

* [Systems programming and memory efficiency](systems-programming.md)
* [Spans and stack allocation](spans-and-memory.md)
* [Ref structs and ref safety](ref-structs-and-ref-safety.md)
* [Unsafe code and interoperability](unsafe-code-and-interop.md)
* [.NET implementation notes](dotnet-implementation.md)
