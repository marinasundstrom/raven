# Language specification

This page is the entry point to the Raven language specification. It defines
the shared conventions used across the spec set and links to the chapter files
that describe the language in detail.

Implementation details describing how Raven projects map to .NET are documented
in [dotnet-implementation.md](dotnet-implementation.md).

An overview of available types, literal semantics, and conversions can be found
in the [type system](type-system.md).

Attached macro syntax and expansion rules are documented in
[macros.md](macros.md).

Recommended source layout and spacing conventions are documented in the [style
guide](../style-guide.md).

## Related reference documents

Use this page for shared conventions and the chapter index below. The following
specialized reference documents complement the feature chapters:

* [Type system](type-system.md)
* [Classes, structs, and interfaces](classes-and-members.md)
* [Control flow](control-flow.md)
* [Error handling](error-handling.md)
* [Macros](macros.md)
* [.NET implementation notes](dotnet-implementation.md)
* [Grammar (non-normative EBNF)](grammar.ebnf)

## Document conventions

* **Normative requirements** use key words such as “must”, “may”, and “should” to describe observable language behaviour.
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

## Specification chapters

* [Lexical structure](lexical-structure.md)
* [Values and statements](values-and-statements.md)
* [Expressions and type inference](expressions-and-inference.md)
* [Async and error propagation expressions](async-and-error-propagation.md)
* [Fundamental expressions](fundamental-expressions.md)
* [Collection expressions](collection-expressions.md)
* [Function invocation](invocations.md)
* [Extensions and traits](extensions.md)
* [Operators and object creation](operators-and-object-creation.md)
* [Control-flow expressions and statements](control-flow-expressions.md)
* [Pattern matching](pattern-matching.md)
* [Namespaces, imports, and aliases](namespaces-and-imports.md)
* [Enum declarations](enum-declarations.md)
* [Top-level code and entry points](top-level-code-and-entry-points.md)
* [Delegate declarations](delegate-declarations.md)
* [Functions](functions.md)
* [Unsafe code and interoperability](unsafe-code-and-interop.md)
* [Local declarations](local-declarations.md)
* [Types and unions](types-and-unions.md)
* [Object-oriented types and operator precedence](object-oriented-types.md)
