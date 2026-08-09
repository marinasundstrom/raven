# Quote Macro

> 🧩 In progress. The compiler-owned expression quote and expression-hole MVPs
> are implemented.
>
> Statement, member, declaration, compilation-unit, contextual-category, and
> token/identifier/repetition splice support remain proposed.
>
> Current spelling: import `Raven.Macros.*` and invoke `quote! { ... }`, or use
> `Raven.Macros.Quote! { ... }` without an import. Earlier examples retain the
> legacy hash-expression spelling as design history.

## Summary

Raven should provide a compiler-owned `#quote` macro that captures Raven source
syntax and expands inline to the `SyntaxFactory` expression that recreates the
captured syntax tree.

```raven
let declaration: FunctionDeclarationSyntax = #quote {
    func Greet(name: string) -> string {
        return "Hello $name"
    }
}
```

`#quote` treats source code as structured data. If the quoted fragment is
invalid for its expected syntax category, compilation fails with diagnostics
inside the quoted fragment. A successful quote produces verified ordinary
Raven code; the binder and emitter do not need a separate representation for
quoted syntax.

This proposal is related to, but distinct from, `RavenQuoter`. `RavenQuoter` is
a runtime/tooling API that accepts source text or an existing syntax node and
prints factory-construction source. `#quote` is compile-time syntax capture.

It is also analogous to .NET expression-tree conversion. In both cases, the
programmer writes in the host language and the compiler produces an object
representation instead of requiring text parsing or complete manual factory
construction. Expression-tree conversion quotes a supported operation graph
into `Expression<TDelegate>` and discards source syntax. `#quote` quotes Raven
syntax into a `Raven.CodeAnalysis.Syntax` object and preserves tokens and
trivia.

## Purpose and use cases

`#quote` is Raven's syntax-literal facility: authors write a Raven fragment in
its natural form and receive the syntax object representing that fragment.
This is clearer and less error-prone than manually assembling a large
`SyntaxFactory` graph, while preserving more structure than treating code as
an opaque string.

The primary users are:

* procedural macros constructing an expansion from a mostly fixed Raven
  template with a few dynamic syntax holes;
* generators and source-transformation tools producing Raven syntax;
* analyzers, refactorings, and code fixes that need replacement syntax; and
* syntax-oriented tests that need a readable expected tree.

For macro authors, this turns manual syntax construction:

```raven
return SyntaxFactory.InfixOperatorExpression(
    SyntaxKind.AddExpression,
    left,
    SyntaxFactory.PlusToken,
    right
)
```

into a direct description of the expansion:

```raven
return #quote {
    #(left) + #(right)
}
```

The quoted Raven supplies the fixed expansion structure, while holes supply
the syntax computed by the enclosing macro. The result remains an ordinary
syntax tree and continues through Raven's normal binding, diagnostics,
tooling, and code-generation pipeline.

The initial quoted fragment is only a convenient way to construct the starting
tree. Afterward, macro code can traverse it and construct a modified immutable
tree through visitors, replacement APIs, `SyntaxFactory`, or additional quoted
fragments and holes. This gives programmatic transformations the same escape
hatch that expression-tree users have after a compiler creates their initial
operation graph.

This also provides an incremental migration path for the macro library.
Existing macros can continue returning trees assembled with `SyntaxFactory`,
while suitable implementations move to `#quote` without changing their public
invocation syntax or typed expansion contract. The Raven-authored sample
`#add` macro now uses the quoted form above, proving that the intrinsic works
while compiling a macro plugin and that the resulting plugin expands normally
in its consuming application.

Because quote expansion runs during binding, invalid quoted Raven is diagnosed
at compile time inside the authored quote. Macro-specific validation can use
the same diagnostic path. This moves errors that string-based generation or
runtime parsing would discover only during execution into the normal build and
editor feedback cycle.

`#quote` does not execute the quoted expression, capture its runtime values, or
produce a semantically bound representation. It captures syntax. Name
resolution and type checking occur only when the resulting syntax is inserted
into a compilation and bound in that context.

## Implemented expression MVP

The first implementation accepts exactly one Raven expression:

```raven
let syntax = #quote {
    left + right
}
```

The intrinsic is registered by the compiler and needs no macro plugin
reference. It parses the complete body through `ParseExpressionResult`,
rejects native parser diagnostics, trailing tokens, and missing recovery
tokens, and maps user diagnostics to the authored quote body. It then expands
to fully qualified ordinary `SyntaxFactory` construction syntax and preserves
tokens and trivia.

Because the resulting value is a real Raven syntax object, the consuming
project must reference the compiler-matched `Raven.CodeAnalysis` assembly.
Missing that runtime reference produces `QUOTE003`. This requirement is
currently explicit; an SDK-provided macro-project prelude/reference remains
future work.

The MVP does not yet select categories from contextual types, quote statements
or declarations, or perform a separate bind-and-equivalence verification pass
before substitution. The generated factory expression is parsed by the
intrinsic and then goes through ordinary caller binding and emit.

### Expression holes

An expression quote can insert an existing `ExpressionSyntax` value with
`#(expression)`:

```raven
let right = SyntaxFactory.IdentifierName("right")
let syntax = #quote {
    left + #(right)
}
```

The `#` and `(` must be adjacent. The contents are one complete ordinary Raven
expression, so a hole can contain a local reference, member access, invocation,
or other expression. Multiple holes are allowed. The generated expansion uses
the hole expression directly where an `ExpressionSyntax` is required, leaving
ordinary binding to enforce the type contract.

The intrinsic recognizes holes through its standard Raven token stream,
balances nested parentheses, and replaces each hole with an equal-width
parser-only placeholder. This keeps authored parser diagnostics correctly
positioned without adding lexer tokens or changing ordinary Raven parsing.
Malformed hole expressions forward native Raven diagnostics; an empty hole
reports `QUOTE005`.

Inserted syntax keeps its own tokens and trivia. Trivia between an internal
hole and neighboring quoted tokens is retained on those quoted neighbors.
Holes do not yet support token, identifier, list, or repeated insertion.

The runnable `samples/projects/macro-quote` project demonstrates this authoring
pattern with a Raven-authored `twice!` macro declaration. Its
`ExpressionSyntax` parameter receives the caller's authored expression; the
macro quotes an addition expression and splices that syntax into both operands.

## Goals

* Capture Raven syntax without first encoding it as a string.
* Preserve tokens and trivia.
* Report parser diagnostics at their authored locations inside the quote.
* Expand to ordinary Raven `SyntaxFactory` construction code.
* Verify the generated expansion before normal binding continues.
* Infer or validate whether the fragment is an expression, statement,
  declaration, member, or compilation unit.
* Support typed insertion without making splice markers ordinary Raven syntax.
* Avoid requiring a separately distributed quote-macro plugin.

## Non-goals

* General token-tree macro support is not defined by this proposal.
* Runtime parsing of arbitrary strings remains a `SyntaxTree.ParseText` or
  parser API concern.
* This proposal does not make `Raven.CodeAnalysis.Syntax` independently
  version-stable from the compiler.
* Initial splicing does not include token, identifier, list, or repetition
  categories.
* The quote macro does not perform semantic validation of the quoted program.
  It validates syntax and the generated expansion.

## Why “quote”

`SyntaxTree` normally describes an entire parsed document, while a quote may
produce an expression, statement, member, declaration, or complete unit.
`AST` can also imply that tokens and trivia have been discarded. “Quote” is
the conventional metaprogramming term for treating authored code as syntax
data and composes naturally with future “unquote” or “splice” operations.

The public spelling should therefore be `#quote`, even if documentation also
describes it as syntax-tree or AST construction.

## Proposed syntax

The primary form captures a delimited token tree:

```raven
let expression: ExpressionSyntax = #quote {
    left + right
}

let statement: StatementSyntax = #quote {
    Console.WriteLine("Hello")
}

let member: MemberDeclarationSyntax = #quote {
    func Answer() -> int => 42
}
```

The exact delimiter rules depend on the general token-tree macro proposal.
The braces belong to the macro invocation and are not necessarily part of the
captured fragment.

The initial implementation should require a contextual syntax type or an
explicit syntax category when the fragment is ambiguous. The final spelling
for an explicit category remains open; possibilities include:

```raven
#quote(expr) { left + right }
#quote(member) { func Answer() -> int => 42 }
```

or category-specific compiler entry points surfaced through contextual typing
alone.

## Expansion model

For:

```raven
let expression: ExpressionSyntax = #quote {
    left + right
}
```

the compiler conceptually:

1. captures the tokens inside the quote;
2. selects the expression parser from the contextual target type;
3. parses the fragment with source locations mapped to the quote body;
4. rejects parser diagnostics or missing-token recovery;
5. converts the resulting syntax node to Raven `SyntaxFactory` construction
   code;
6. parses the generated Raven expansion;
7. verifies that the generated expression reconstructs an equivalent syntax
   node;
8. substitutes the verified expression at the invocation site; and
9. binds and emits the resulting ordinary Raven code.

The expansion should be observable through the existing macro-expansion APIs
and `rvn dev macros`.

## Verification

Verification has two distinct responsibilities.

### Quoted-fragment verification

The captured fragment must:

* parse under the selected syntax category;
* contain no parser errors;
* contain no missing recovery tokens; and
* produce a node compatible with the contextual syntax type.

Diagnostics use authored spans inside the quote. The `#quote` invocation may
be included as a related location.

### Generated-expansion verification

The produced `SyntaxFactory` source must:

* parse as a Raven expression;
* bind against the syntax API available to the macro compilation;
* have a result convertible to the requested syntax type; and
* reconstruct an equivalent syntax tree, including token kinds, values, and
  trivia.

A failure in generated-expansion verification is a compiler defect, not an
ordinary user parse error. The diagnostic should say that quote expansion
failed and retain the original quote location for investigation.

## Compiler ownership and versioning

`#quote` should be a compiler-provided intrinsic macro rather than a normal
plugin assembly. Compiler ownership provides:

* guaranteed availability;
* access to the compiler's exact parser and syntax model;
* no plugin discovery or independently versioned quote package;
* source-accurate parser diagnostics; and
* a stable language-level spelling.

Compiler ownership does not remove the type-identity issue. A value whose type
is `ExpressionSyntax` or `MemberDeclarationSyntax` still belongs to
`Raven.CodeAnalysis`. The initial feature should therefore be available in
macro projects, which already compile against the compiler-matched macro and
syntax API.

The compiler/MSBuild integration should provide that reference from the active
Raven SDK rather than requiring macro authors to choose an unrelated package
version. Macro load contexts must unify the macro assembly's
`Raven.CodeAnalysis` reference with the compiler's own assembly, following the
existing `MacroReference` direction.

If `#quote` is later allowed in ordinary runtime projects, those projects must
carry a compatible runtime `Raven.CodeAnalysis` reference because the quoted
value is a real syntax object. Baking the macro into the compiler cannot make
that runtime dependency disappear.

An implicit macro-project prelude may expose `Raven.CodeAnalysis.Syntax` names,
but it should be convenience rather than a second syntax type system.

## Required macro infrastructure

Implementation should wait for these general macro capabilities:

1. **Delimited token-tree input**
   Freestanding macros must capture a block without requiring the block to
   parse first as an ordinary Raven argument expression.

2. **Category-directed fragment parsers**
   Macro infrastructure must expose expression, statement, member,
   declaration, and compilation-unit parsing with authored source locations.

3. **Expansion substitution**
   Freestanding expansion results must replace invocations during normal
   binding and code generation, not only appear in inspection APIs.

4. **Source mapping**
   Diagnostics from captured and expanded syntax must map back to the quote
   body and invocation.

5. **Compiler-provided macro registration**
   The registry must distinguish intrinsic macros from project/plugin macros
   while preserving normal resolution, diagnostics, caching, and tooling.

6. **Contextual result categories**
   The compiler must communicate the required syntax category or contextual
   syntax type to the intrinsic.

7. **Expansion validation**
   Generated syntax must be parsed and bound before it enters the ordinary
   compilation.

8. **Deterministic caching**
   Quote expansion should be cached by captured syntax identity, contextual
   category, parse options, and compiler/syntax API version.

## Relationship to `RavenQuoter`

The current `RavenQuoter` should remain a public runtime and tooling utility.
Its Raven output mode is useful for diagnostics, examples, and the first
implementation of factory-source generation.

The intrinsic should reuse a structured quoter service where practical, but
must not implement quoting by rendering the captured fragment to a string and
calling the public text API. It already has a parsed syntax node and should
preserve that identity and its source mapping.

Longer term, the shared implementation may be separated into:

* syntax-node-to-factory-expression construction;
* Raven/C# source rendering for tooling; and
* compiler expansion verification.

## Expression splice and future categories

Expression quotes support explicit insertion of expression syntax:

```raven
let operand = SyntaxFactory.IdentifierName("answer")

let expression = #quote {
    #(operand) + 1
}
```

This first form accepts an ordinary Raven expression whose result must be
convertible to `ExpressionSyntax`. Token, identifier, statement, member, list,
and repetition splices still require explicit categories, hygiene rules, and
source-mapping policy. Those concerns remain deferred.

## Implementation sequence

1. Complete general token-tree capture for freestanding macros.
2. Integrate freestanding macro substitution into binding and code generation.
3. Add fragment-parser and source-mapping services to macro infrastructure.
4. Add compiler-provided intrinsic macro registration.
5. Implement `#quote` for one unambiguous category, preferably expressions.
6. Add contextual category selection for statements and declarations.
7. Add generated-expansion parsing, binding, and equivalence verification.
8. Expose expansion through developer tooling and language-service features.
9. Add expression holes; consider the remaining typed splice categories in a
   separate proposal.

## Open questions

* The first implementation supports only expression quotes.
* Is contextual typing sufficient, or is an explicit category syntax required?
* Should trivia be preserved by default, or should quotes offer a normalized
  mode?
* Which syntax base type should an uncontextualized quote produce?
* Should ordinary application projects be allowed to use `#quote`, or should
  the first version be restricted to macro projects?
* What is the exact compatibility policy between the Raven SDK, compiler,
  macro contracts, and `Raven.CodeAnalysis` runtime assembly?
