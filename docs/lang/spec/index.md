# Raven language reference

This reference is both Raven's developer documentation and its working language
specification. It explains Raven's syntax, behavior, and language rules, with
examples intended to help developers write and understand Raven code.

It is not an exhaustive formal specification of every compiler behavior or edge
case. Individual pages become more precise where a distinction affects normal
programs, interoperability, or diagnostics.

## Language topics

Use these sections to jump into the part of the language you want to explore.

### Language fundamentals

Start with Raven's lexical structure, values and statements, namespaces,
imports, and top-level code.

[Language fundamentals](lexical-structure.md)

### Types

Learn about Raven's type system, unions, enums, delegates, and type-related
language rules.

[Type system](type-system.md)

### Expressions

Learn how expressions are formed and typed, including calls, collections,
tuples, pipes, object creation, and operators.

[Expressions and type inference](expressions-and-inference.md)

### Declarations and members

Functions, local declarations, classes, structs, interfaces, properties,
extensions, inheritance, parameters, and operators.

[Classes, structs, and interfaces](classes-and-members.md)

### Pattern matching

Raven's pattern system, including match forms, deconstruction, property,
sequence, dictionary, and union patterns.

[Pattern matching](pattern-matching.md)

### Control flow

Statements and expressions for branching, matching, iteration, returns,
yielding, jumps, and synchronization.

[Control flow](control-flow.md)

### Error handling

Error handling and Raven's extensible `?` propagation model.

[Error handling](error-handling.md)

### Metaprogramming

Compile-time metaprogramming and DSL construction with Raven macros.

[Macros](macros.md)

### Systems programming

Memory-efficient and low-level programming with spans, stack allocation,
ref structs, ref safety, and unsafe code.

[Systems programming and memory efficiency](systems-programming.md)

### .NET interoperability

Rules and implementation details for interacting with the .NET type system
and runtime.

[.NET implementation notes](dotnet-implementation.md)

### Grammar

A non-normative EBNF description of Raven's grammar.

[Grammar (EBNF)](grammar.ebnf)