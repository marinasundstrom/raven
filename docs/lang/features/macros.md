# Transform code with macros

Raven macros are compile-time programs. A macro receives explicit input,
validates it, and produces ordinary Raven syntax that continues through the
normal compiler and editor pipeline.

Macros are experimental. Their syntax and authoring contracts may change while
the feature is refined.

## Use a macro for an explicit source transformation

Invoke a macro with `!`. The invocation stays visible at the place where the
transformation occurs:

```raven
import Raven.Macros.*

let message = embedFileContent!("assets/message.txt")
```

`embedFileContent!` reads the file during compilation and replaces the
invocation with a string literal. The application does not need to open or
deploy the original file at runtime.

Use a macro when:

- a programmer should opt into a transformation at a specific source location
- invalid input should produce a build diagnostic
- generated code should retain a relationship to the authored invocation
- a compact domain-specific language makes an application clearer

Use an ordinary function when the work belongs at runtime. Use a source
generator when a project-wide input should contribute separate generated files
rather than replace or augment one explicit source site.

## Author a small local macro

A macro can live in the same project while it is being developed:

```raven
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro Double(value: int) {
    let doubled = value * 2
    expand ParseExpression(doubled.ToString())
}

let answer = Double!(21)
```

The macro parameter is evaluated at compile time. `expand` supplies the Raven
syntax that replaces the invocation; the resulting expression is then bound
and compiled normally.

Start with typed parameters. Move to syntax or token-body input only when the
macro needs to inspect authored code or implement a DSL. Report malformed input
as diagnostics instead of throwing.

## Continue learning

- [Authoring Raven macros](../../macro-authoring.md) builds from a local macro
  through diagnostics, DSL bodies, tooling, attached macros, and packaging.
- [Extend a Raven project](../../compiler/extending-projects.md) compares
  macros with analyzers and source generators.
- The [macro sample projects](https://github.com/marinasundstrom/raven/tree/main/samples/projects)
  provide runnable examples of declarations, token DSLs, quotation, embedded
  files, and Blazor integration.
