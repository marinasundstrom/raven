# Quote Macro

> 🧩 Proposal. Not implemented.
>
> This feature depends on token-tree macro input and on macro expansions being
> integrated into normal binding and code generation. It should not be
> implemented as a special case before that infrastructure exists.

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

## Goals

* Capture Raven syntax without first encoding it as a string.
* Preserve tokens and trivia.
* Report parser diagnostics at their authored locations inside the quote.
* Expand to ordinary Raven `SyntaxFactory` construction code.
* Verify the generated expansion before normal binding continues.
* Infer or validate whether the fragment is an expression, statement,
  declaration, member, or compilation unit.
* Provide a foundation for future unquote/splice support.
* Avoid requiring a separately distributed quote-macro plugin.

## Non-goals

* General token-tree macro support is not defined by this proposal.
* Runtime parsing of arbitrary strings remains a `SyntaxTree.ParseText` or
  parser API concern.
* This proposal does not make `Raven.CodeAnalysis.Syntax` independently
  version-stable from the compiler.
* Initial implementation does not include interpolation, unquoting, or
  repetition.
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

## Future unquote and splice

Once quoting is stable, Raven may add explicit insertion of values into quoted
syntax:

```raven
let name = SyntaxFactory.Identifier("Answer")

let member = #quote {
    func #($name)() -> int => 42
}
```

This requires typed splice categories, hygiene rules, repetition behavior,
and source mapping. Those concerns are intentionally deferred.

## Implementation sequence

1. Complete general token-tree capture for freestanding macros.
2. Integrate freestanding macro substitution into binding and code generation.
3. Add fragment-parser and source-mapping services to macro infrastructure.
4. Add compiler-provided intrinsic macro registration.
5. Implement `#quote` for one unambiguous category, preferably expressions.
6. Add contextual category selection for statements and declarations.
7. Add generated-expansion parsing, binding, and equivalence verification.
8. Expose expansion through developer tooling and language-service features.
9. Consider unquote/splice in a separate proposal.

## Open questions

* Should the first implementation support only expression quotes?
* Is contextual typing sufficient, or is an explicit category syntax required?
* Should trivia be preserved by default, or should quotes offer a normalized
  mode?
* Which syntax base type should an uncontextualized quote produce?
* Should ordinary application projects be allowed to use `#quote`, or should
  the first version be restricted to macro projects?
* What is the exact compatibility policy between the Raven SDK, compiler,
  macro contracts, and `Raven.CodeAnalysis` runtime assembly?
