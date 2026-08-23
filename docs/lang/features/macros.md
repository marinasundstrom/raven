# Extend Raven with macros

Raven macros are explicit compile-time programs. A macro receives input from
one source location, validates or interprets it, and produces ordinary Raven
syntax. The resulting syntax then follows the normal compiler pipeline: it is
bound, diagnosed, emitted, debugged, and presented by editor tooling like
handwritten Raven code.

Their purpose is to simplify authored code and add expressiveness. A small,
domain-meaningful form can expand into more complex Raven behavior, whether the
form looks like a call, an expression or statement header, a token-body DSL, or
a declaration. In effect, a library can build its own DSL while retaining a
seamless boundary with Raven syntax, semantics, diagnostics, and tooling.

The spelling deliberately balances integration with honesty. A resolved alias
such as `component` can be presented like a contextual language keyword, so a
DSL reads naturally beside built-in Raven declarations. The required `!`
remains visible as the stable signal that the construct is a macro invocation
provided by a library rather than syntax fixed into the Raven language.

Macros are experimental. Their syntax and authoring contracts may change while
the feature is refined.

## The simplest macro

A macro with ordinary parameters looks much like a compile-time function:

```raven
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro Double(value: int) {
    expand ParseExpression((value * 2).ToString())
}

let answer = Double!(21)
```

`Double!(21)` explicitly invokes the macro. The compiler binds `21` to the
typed macro parameter, runs the macro during compilation, and replaces the
invocation with the returned expression syntax. `expand` supplies that syntax
and ends the current expansion path.

A macro is not textual substitution. It consumes typed values or structured
source and returns immutable syntax. Invalid input should produce diagnostics
at the authored source location rather than an exception.

## Macros can extend the language naturally

Macros can introduce concise, domain-specific forms wherever their declared
result is valid. For example, a library can build a component declaration DSL
over Blazor without changing Raven's lexer or adding a built-in `component`
declaration:

```raven
import System.Console.*

public component! Greeting(Name: string = "") {
    WriteLine("Rendering Greeting for ${Name}")

    markup! {
        <section class="greeting">
            <h1>Hello {Name}</h1>
        </section>
    }
}
```

Here `component!` expands to ordinary Blazor component declarations, while the
nested `markup!` invocation expands to a render fragment. The two macros own
their interpretations, but the generated program still uses Blazor's normal
component, parameter, rendering, event, CSS, and hosting infrastructure.
`component` is specifically the alias of the function-style declaration macro;
the attached component macro has a separate canonical identity.

This explicit boundary is what lets a DSL feel integrated without making its
private grammar part of Raven. The compiler owns the surrounding invocation,
source locations, expansion category, and semantic integration. The macro owns
the meaning of its declared inputs.

The goal is native language experience, not indistinguishability from the
language core. A reader can understand `component` as declaration vocabulary
while `!` still identifies the extension point.

## Two application kinds

Raven distinguishes where a macro is applied from how it receives input.

- A **freestanding macro** occupies its own source location and expands to the
  syntax category declared by its return annotation. It can produce an
  expression, statement, declaration, or supported list of declarations.
- An **attached macro** occupies an attribute-like position and transforms an
  existing declaration.

```raven
let digest = sha256Digest!("hello")

#[Observable]
public var Name: string
```

A token body, custom lexer, or declaration-shaped carrier adds an input shape
or capability. It does not create another application kind.

## Input shapes communicate intent

The delimiters tell the reader how the invocation supplies input:

| Form | Reading |
| --- | --- |
| `Name!(...)` | Pass these arguments into the macro. |
| `Name! { ... }` | Process this bounded region of content. |
| `Name!(...) { ... }` | Pass configuration and process a trailing region. |
| `Name! Decl(...) { ... }` | Process a declaration-shaped carrier with a name, parameters, and body. |

Ordinary typed and syntax parameters bind from `(...)`. A parameter typed as
`ExpressionSyntax`, for example, receives the authored expression node instead
of executing it. One `IMacroTokenStream` parameter requests the lossless
brace-delimited body. A macro can interpret that body as Raven, as a custom
DSL, or as a mixture of the two.

The declaration-shaped form deliberately resembles an ordinary declaration:

```raven
func Greeting(Name: string) { }
component! Greeting(Name: string) { }
```

In the second line, `Greeting` and its parameters belong to the declaration
being introduced; they are not macro call arguments.

This is a real declaration-shaped carrier. At compilation-unit, namespace, and
type-member boundaries it remains distinct from Raven's expression and
statement invocation forms, including top-level statements. That distinction
lets the compiler and IDE give the carrier declaration behavior such as member
placement and an outline entry under `Greeting`.

## Placement follows the expansion

A macro declares the kind of syntax it produces, and that result determines
where the macro may appear. An expression macro can be used where Raven expects
an expression; a declaration macro can be used at a compatible declaration
boundary. Raven validates the returned syntax before integrating it, so a macro
cannot use expansion to bypass the grammar of its containing scope.

The [macro language reference](../spec/macros.md) defines the exact return
annotations, supported positions, input binding, and expansion validation.

## A spectrum of macro complexity

Not every macro is a language within the language. Prefer the smallest input
model that expresses the job:

1. Typed parameters for compile-time configuration or literals.
2. Syntax parameters when authored Raven code must be inspected or preserved.
3. A token body when the macro owns a bounded region or combines an outer DSL
   with embedded Raven fragments.
4. Custom tokenization, parsing, source mapping, and editor metadata only when
   the DSL has a genuinely different lexical or structural grammar.

The last category can be substantial compiler-like work. A production token
DSL should plan for incomplete input, recovery, precise diagnostics, stable
source spans, highlighting, completion, hover, navigation, and cancellation.
Raven supplies APIs for those responsibilities, but the macro author still
defines the DSL grammar and its lowering.

Raven currently favors this procedural model: macro code receives declared,
possibly typed inputs and explicitly constructs an expansion. There is no
first-class declarative pattern-and-replacement macro syntax. Such a facility
could later be built as a library macro that interprets rules and produces
Raven syntax, but its result would still pass through the same expansion
validation, binding, diagnostics, and type checking as every other macro.

## Names, aliases, and editor presentation

A macro has a canonical name and may publish an alias. Aliases are
case-sensitive and participate in normal namespace and import resolution; they
do not become global lexical keywords. A local value can therefore shadow an
imported alias.

Once an alias invocation is resolved, Raven IDEs contextually present the alias
as a **contextual keyword**. This is especially natural in declaration forms
such as `public component! Greeting(...)`. Canonical macro names retain the
distinct macro classification. The language server and Playground use the same
compiler-owned semantic classification.

This presentation does not reserve the alias or erase the macro boundary. The
alias remains an ordinary, resolvable name, and `!` remains part of every
freestanding invocation spelling.

An identifier-bearing declaration form also contributes one editor-outline
entry for its authored identifier. For example, `component! Greeting(...)`
appears as `Greeting`, with `component!` retained as its detail. The outline
describes the authored declaration carrier rather than duplicating members from
its generated implementation. A member-position macro invocation without an
identifier contributes no synthetic outline name.

## Macros compose

An expansion may contain another macro invocation. This lets a larger DSL
delegate a nested region to a focused macro, as `component!` delegates markup
to `markup!`. Embedded Raven fragments can retain normal completion, hover,
navigation, semantic coloring, and applicable inlay hints in the caller's
scope. For example, an inferred `let x = 42` declaration in a component block
can be presented as `let x: int = 42` without changing the authored source.

## Choose the right extension mechanism

Use a macro when:

- a programmer explicitly opts into a transformation at one source location;
- invalid input should produce a build diagnostic tied to that location;
- generated syntax should retain a relationship to the authored invocation;
- a compact DSL makes the application clearer.

Use an ordinary function when the work belongs at runtime. Use a source
generator when project-wide input should contribute separate generated files
rather than replace or augment one explicit source site.

## Continue learning

- [Authoring Raven macros](../../macro-authoring.md) progresses from a small
  local macro through DSL parsing, diagnostics, editor integration, attached
  macros, and packaging.
- The [macro language specification](../spec/macros.md) defines current syntax,
  placement, binding, resolution, expansion, and composition rules.
- [Extend a Raven project](../../compiler/extending-projects.md) compares
  macros with analyzers and source generators.
- The [macro sample projects](https://github.com/marinasundstrom/raven/tree/main/samples/projects)
  provide runnable examples of declarations, token DSLs, quotation, embedded
  files, and Blazor integration.
