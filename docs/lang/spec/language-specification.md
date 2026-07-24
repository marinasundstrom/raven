# Raven language reference

This reference is both Raven's developer documentation and its working language
specification. It explains the most commonly used syntax, behavior, and rules by
feature, with examples intended to help developers write and understand Raven
code.

It is not an exhaustive formal specification of every compiler behavior or edge
case. Individual pages become more precise where a distinction affects normal
programs, interoperability, or diagnostics.

Implementation details describing how Raven projects map to .NET are documented
in [dotnet-implementation.md](dotnet-implementation.md).

An overview of available types, literal semantics, and conversions can be found
in the [type system](type-system.md).

Attached macro syntax and expansion rules are documented in
[macros.md](macros.md).

Recommended source layout and spacing conventions are documented in the [style
guide](../style-guide.md).

## Related reference documents

Use this page for shared conventions and the topic index below. The following
specialized documents complement the feature pages:

* [Type system](type-system.md)
* [Classes, structs, and interfaces](classes-and-members.md)
* [Control flow](control-flow.md)
* [Error handling](error-handling.md)
* [Macros](macros.md)
* [.NET implementation notes](dotnet-implementation.md)
* [Grammar (non-normative EBNF)](grammar.ebnf)

## Document conventions

* Requirements use key words such as “must”, “may”, and “should” when a rule
  needs to be stated precisely. The reference does not attempt to enumerate
  every valid or invalid program.
* Notes and tips highlight rationale, examples, or implementation remarks. They are informative rather than normative.
* Code snippets use the `.rav` file extension and omit surrounding boilerplate unless it is essential to the rule being described.
* When behaviour is intentionally unspecified or still under design, this
  specification calls it out explicitly.
* Callout boxes use a small, consistent emoji set:
  * `ℹ️` **Info** for factual clarification and context.
  * `⚠️` **Warning** for pitfalls or behavior likely to surprise.
  * `❗` **Important** for distinctions that affect correctness or interpretation.
  * `🧭` **Disambiguation** for parser/binder interpretation rules.
  * `❓` **Open Question** for unresolved design choices that need follow-up.

## Code samples

Representative sample programs live in the repository's [sample
directory](https://github.com/marinasundstrom/raven/tree/main/samples) alongside runnable CLI demos.
Each sample intentionally exercises language features such as pattern matching, pipe operators, tuple flow, and .NET interop so
changes to the language can be validated with real code, not just unit tests. The top-level `samples/README.md` explains how to
run them with the Raven CLI.

## Language topics

The pages are organized by feature and developer task so the relevant syntax
and common rules are easy to find.

### Language fundamentals

* [Lexical structure](lexical-structure.md)
* [Values and statements](values-and-statements.md)
* [Namespaces, imports, and aliases](namespaces-and-imports.md)
* [Top-level code and entry points](top-level-code-and-entry-points.md)
* [Grammar (non-normative EBNF)](grammar.ebnf)

### Types

* [Type system](type-system.md)
* [Types and unions](types-and-unions.md)
* [Enum declarations](enum-declarations.md)
* [Delegate declarations](delegate-declarations.md)

### Expressions

* [Expressions and type inference](expressions-and-inference.md)
* [Fundamental expressions](fundamental-expressions.md)
* [Collection expressions](collection-expressions.md)
* [Tuple expressions](tuple-expressions.md)
* [Calls](invocations.md)
* [Pipe expressions](pipe-expressions.md)
* [Object creation](object-creation.md)
* [Object initialization and copying](object-initialization-and-copying.md)
* [Index, range, and bitwise operators](operators-and-object-creation.md)
* [Operator precedence and expression disambiguation](object-oriented-types.md)

### Declarations and members

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

### Metaprogramming

* [Macros](macros.md)

### Pattern matching

* [Pattern matching overview](pattern-matching.md)
* [Dictionary patterns and exhaustiveness](dictionary-patterns-and-exhaustiveness.md)
* [Match forms](match-forms.md)
* [Pattern forms and contexts](pattern-contexts.md)
* [Fundamental patterns](fundamental-patterns.md)
* [Sequence and property patterns](sequence-and-property-patterns.md)
* [Deconstruction, member, and union patterns](deconstruction-and-union-patterns.md)

### Control flow

* [Control flow overview](control-flow.md)
* [Control-flow expressions and statements](control-flow-expressions.md)
* [Lock statements](lock-statements.md)
* [Assignment and expression statements](assignment-and-expression-statements.md)
* [Match statements](match-statements.md)
* [Return and yield](returns-and-yield.md)
* [Jumps and labels](jumps-and-labels.md)

### Error handling

* [Error handling](error-handling.md)
* [Error propagation and carrier types](async-and-error-propagation.md)

### .NET interoperability

* [Unsafe code and interoperability](unsafe-code-and-interop.md)
* [.NET implementation notes](dotnet-implementation.md)
