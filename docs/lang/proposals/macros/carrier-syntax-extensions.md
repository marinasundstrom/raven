# Macro carrier syntax shapes

Status: **Proposed**

This proposal adds predictable source shapes to Raven's macro carrier model.
Its two immediate additions are richer declaration-shaped carriers and an
expression-header carrier resembling the outer shape of Raven control-flow
constructs. It covers source grammar, syntax-tree shape, carrier selection, and
the macro-facing representation of that syntax. It does not define expansion
behavior, generated-symbol semantics, or bootstrapping.

The current macro API is experimental. Source and API compatibility are not
requirements for this work. If the existing nodes or normalized invocation
model prevent a coherent carrier hierarchy, they should be replaced rather
than extended with an increasing number of mutually exclusive nullable slots.

## Terminology

The existing terminology remains in use:

* a **freestanding macro** is an application kind;
* an **invocation carrier** supplies call-like input;
* a **declaration-shaped carrier** supplies a Raven-shaped declaration header;
* a **token body** is the lossless brace-delimited region owned by the macro;
* the **grammar position** is where the complete macro application appears,
  such as an expression, statement, type, pattern, or member position.

A token body, declaration header, or unparenthesized expression is a carrier
shape. It does not create another macro application kind.

## Motivation

Raven should let libraries offer DSLs with an outer shape appropriate to the
job instead of forcing every extension through function-call syntax or one raw
token body. Together, the carrier family can cover a useful spectrum:

```raven
sql!(queryText)

markup! {
    <h1>Hello</h1>
}

retry! operation {
    retry policy
}

component! Counter<T>(initial: T)
    : ComponentBase, IRenderable<T>
    where T: Number
{
    component body
}
```

This gives Raven libraries the expressive surface needed to compete with
languages that have strong DSL and syntax-extension models. A DSL can look
call-like, control-flow-shaped, declaration-shaped, or wholly private inside a
bounded body without adding a new reserved keyword or fixed production to the
Raven grammar.

The required `!` keeps that extensibility honest. The carrier shell remains
recognizably macro-provided, while every Raven-shaped component inside the
shell retains its ordinary Raven grammar and tooling expectations.

## Goals

* Let macros occupy and represent many useful Raven syntax shapes without
  teaching the parser the identity of individual macros.
* Make each carrier shape explicit in the syntax tree and public API.
* Enrich declaration-shaped carriers with Raven's ordinary generic, parameter,
  base-list, return-type, constraint, and clause syntax.
* Distinguish type arguments supplied to a macro from type parameters declared
  by a declaration-shaped carrier.
* Add a concise expression-header carrier for any macro name or alias:

  ```raven
  process! operation

  process! operation {
      continuation
  }
  ```

* Preserve Raven's normal meaning wherever a carrier uses Raven-shaped syntax.
* Parse incomplete and unresolved carriers deterministically without loading a
  macro implementation.
* Keep the model open to additional carriers without redesigning every
  application-position node.

## Non-goals

This proposal does not specify:

* how expansions execute or which output categories they produce;
* how a generated declaration contributes symbols;
* how an authored declaration header must correspond to generated syntax;
* a bootstrap implementation of macro declarations.

The proposal identifies the syntax contract a macro author selects, but leaves
execution and output validation to the broader macro model.

## Core rules

### Carrier choice belongs to the macro contract

A macro author decides which source carriers the macro accepts. A macro may be
call-like, expression-taking, token-body-based, declaration-shaped, or may
deliberately accept several carriers.

That choice does not let the author redefine a selected carrier's grammar. If
the macro accepts a base list, it receives Raven base-type syntax. If it accepts
an expression-header carrier, it receives Raven expression syntax.

### Raven-shaped syntax keeps Raven meaning

In this declaration carrier:

```raven
component! Counter<T>(initial: T)
    : ComponentBase(initial), IRenderable<T>
    where T: Number
{
    render initial
}
```

the parser represents:

* `<T>` as `TypeParameterListSyntax`;
* `(initial: T)` as `ParameterListSyntax`;
* `ComponentBase(initial)` and `IRenderable<T>` as base-type syntax;
* `where T: Number` as `TypeParameterConstraintClauseSyntax`;
* `{ ... }` as `MacroTokenTreeSyntax`.

The macro cannot reinterpret the base list or constraint clause as arbitrary
tokens. Private grammar belongs in an explicitly represented custom clause or
token body.

### Parsing does not depend on macro resolution

The source position and tokens select a syntax carrier before the macro name is
resolved. Resolution may later reject a carrier that the selected macro does
not accept, but it does not change the authored parse.

## Syntax architecture

Macro applications must inherit from the syntax category of their grammar
position. An expression macro node cannot also inherit from `TypeSyntax` or
`MemberDeclarationSyntax`. The position-specific outer nodes should therefore
share a carrier child and a public common contract:

```text
IMacroApplicationSyntax
    SyntaxNode Syntax
    NameSyntax Name
    SyntaxToken ExclamationToken
    MacroCarrierSyntax Carrier
```

The target position nodes are:

```text
FreestandingMacroExpressionSyntax : ExpressionSyntax
FreestandingMacroStatementSyntax : StatementSyntax
FreestandingMacroTypeSyntax : TypeSyntax
FreestandingMacroPatternSyntax : PatternSyntax
FreestandingMacroMemberDeclarationSyntax : MemberDeclarationSyntax
```

The shared carrier model prevents each position node from independently
inventing argument, body, and declaration fields. Position support may land in
slices, but the public model should account for all five positions from the
start so later slices do not require another API redesign.

### Positions and carrier shapes

Application position and carrier shape are separate axes. Supporting all useful
forms does not require accepting every carrier in every position:

| Carrier | Expression | Statement | Type | Pattern | Member/declaration |
| --- | --- | --- | --- | --- | --- |
| Parenthesized | Yes | Yes | Yes | Yes | Yes |
| Token tree | Yes | Yes | Yes | Yes | Yes |
| Expression header | Yes | Yes | No | No | No |
| Declaration-shaped | No | No | No | No | Yes |

The expression-header carrier is deliberately limited to the expression and
statement paradigm. It provides the control-flow-like shell discussed in this
proposal; it is not a way to smuggle an expression header into type, pattern,
or declaration grammar.

Declaration-shaped carriers occur at member or declaration boundaries. If
Raven later gains another ordinary declaration position, such as a broader
local-declaration facility, that position can reuse the same carrier without
changing its header syntax.

At compilation-unit boundaries where Raven permits both global statements and
members, carrier lookahead retains a deterministic syntactic preference:
identifier-bearing declaration headers select the declaration carrier, while
ordinary invocation carriers follow the global-statement path.

### Carrier hierarchy

The carrier is a discriminated syntax hierarchy rather than a collection of
nullable fields:

```text
MacroCarrierSyntax
    ParenthesizedMacroCarrierSyntax
        ArgumentListSyntax ArgumentList
        MacroTokenTreeSyntax? TokenTree

    ExpressionHeaderMacroCarrierSyntax
        ExpressionSyntax Expression
        MacroTokenTreeSyntax? TokenTree

    TokenTreeMacroCarrierSyntax
        MacroTokenTreeSyntax TokenTree

    DeclarationMacroCarrierSyntax
        MacroDeclarationHeaderSyntax Header
        MacroTokenTreeSyntax? TokenTree
```

This directly represents the currently useful combinations:

```raven
foo!()
foo!(x, y)
query!(dialect: "sql") { ... }
process! operation
process! operation { ... }
markup! { ... }
component! Counter<T> { ... }
```

New shapes, such as a future sequence carrier, can add another
`MacroCarrierSyntax` subtype without adding fields to every application node.

## Descriptive grammar

Names in this grammar refer to existing Raven syntax categories where one
already exists.

```ebnf
FreestandingMacroApplication
    ::= MacroName '!' MacroCarrier

MacroName
    ::= Name
     |  KeywordAsMacroName

MacroCarrier
    ::= ParenthesizedCarrier
     |  ExpressionHeaderCarrier
     |  TokenTreeCarrier
     |  DeclarationCarrier

ParenthesizedCarrier
    ::= ArgumentList TokenBody?

ExpressionHeaderCarrier
    ::= Expression TokenBody?

TokenTreeCarrier
    ::= TokenBody

DeclarationCarrier
    ::= MacroDeclarationHeader TokenBody?

MacroDeclarationHeader
    ::= Identifier
        TypeParameterList?
        ParameterList?
        DeclarationTail
        MacroDeclarationClause*

DeclarationTail
    ::= BaseList TypeParameterConstraintClause* PermitsClause?
     |  ArrowTypeClause TypeParameterConstraintClause*
     |  TypeParameterConstraintClause* PermitsClause?

TokenBody
    ::= '{' MacroBodyToken '}'
```

`KeywordAsMacroName` permits a Raven keyword token to act as a macro name when
it is immediately followed by `!`. The explicit marker makes `match!`, `if!`,
or another keyword-like alias distinguishable from the corresponding built-in
construct before macro resolution. The authored token text remains the macro
name; this rule does not make the keyword an identifier elsewhere.

The tail alternatives prevent a base list or `permits` clause from being
combined with a callable return annotation. No ordinary Raven declaration has
that combination, so a general macro carrier should not create it.

The parameter list is structurally neutral. In a form without a distinguishing
suffix, the parser does not decide whether it resembles primary-constructor
parameters or callable parameters:

```raven
form! Example<T>(value: T) {
}
```

The selected macro contract may later accept or reject the shape. The
individual parameters remain ordinary `ParameterSyntax` nodes.

## Type arguments and type parameters

Generic syntax has two distinct positions and fixed meanings.

### Macro type arguments

A generic name before `!` specializes the macro application:

```raven
parse<int>!("42")
repeat<string>! value
```

The generic portion is part of `MacroName` and is represented by the
`TypeArgumentListSyntax` of a `GenericNameSyntax`.

### Declared type parameters

A type-parameter list after the declaration identifier introduces parameters
of the declaration-shaped carrier:

```raven
component! Counter<T>(initial: T) {
}
```

It is represented by `TypeParameterListSyntax`, not type-argument syntax.

Both positions may occur without ambiguity:

```raven
component<Blazor>! Counter<T>(initial: T)
    : ComponentBase, IRenderable<T>
    where T: Number
{
}
```

`Blazor` is a macro type argument. `T` is a declared type parameter. A macro
author may accept either or both forms, but cannot change the category selected
by the source position.

## Declaration-shaped carriers

The declaration carrier owns a reusable structured header:

```text
MacroDeclarationHeaderSyntax
    Identifier
    TypeParameterList?
    ParameterList?
    Suffix?
    ConstraintClauses
    PermitsClause?
    MacroClauses
```

The suffix should itself be discriminated:

```text
MacroDeclarationSuffixSyntax
    MacroBaseListSuffixSyntax
        BaseListSyntax BaseList

    MacroReturnTypeSuffixSyntax
        ArrowTypeClauseSyntax ReturnType
```

The header uses existing Raven nodes:

| Source | Syntax category |
| --- | --- |
| `<T>` after the declared name | `TypeParameterListSyntax` |
| `(value: T)` | `ParameterListSyntax` |
| `: Base, IFoo<T>` | `BaseListSyntax` and `BaseTypeSyntax` |
| `-> Result<T>` | `ArrowTypeClauseSyntax` and `TypeSyntax` |
| `where T: Entity` | `TypeParameterConstraintClauseSyntax` |
| `permits A, B` | `PermitsClauseSyntax` and `TypeSyntax` |

This represents type-like forms:

```raven
service! Repository<T>(source: DataSource)
    : RepositoryBase(source), IRepository<T>
    where T: Entity
{
}
```

and callable forms:

```raven
consumer! Handle<T>(message: T) -> Task<Result>
    where T: Message
{
}
```

Attributes and modifiers belong to the position-specific member application
node, as they do for other `MemberDeclarationSyntax` nodes. The declaration
header remains reusable and independent of its enclosing member position.

## Macro-defined declaration clauses

Standard Raven clauses continue to use standard syntax nodes. A `where` clause
cannot be claimed by a macro and reinterpreted with a private grammar.

A separate `MacroDeclarationClauseSyntax` represents a clause whose leading
contextual word belongs to the macro vocabulary:

```text
MacroDeclarationClauseSyntax
    ClauseKeyword
    PayloadToken
    TerminatorToken
```

For example:

```raven
service! Repository<T>
    where T: Entity
    lifetime scoped
    serializer by RepositorySerializer<T>
{
}
```

The initial authored node preserves each custom payload losslessly. Its
boundary uses Raven's normal declaration-continuation and terminator rules:

* a top-level line terminator ends the clause unless normal continuation rules
  keep the payload open;
* balanced parentheses, brackets, and nested braces remain within the payload;
* the declaration token body begins at the first top-level opening brace after
  the completed header;
* a blank line always ends an incomplete clause.

The macro-facing syntax contract may project a clause payload as an identifier,
type, expression, pattern, parameter list, or another declared Raven syntax
category. That projection cannot reinterpret a successfully parsed standard
header component. Arbitrary DSL content requires an explicitly delimited
payload.

The generic clause boundary needs focused parser prototypes before this part
advances beyond proposal status. In particular, recovery must remain
predictable when a clause and the following token body are both incomplete.

## Expression-header carriers

This is one carrier shape alongside parenthesized, token-tree, and
declaration-shaped carriers. It is not a general replacement for those forms.

Any macro name or alias may take exactly one ordinary Raven expression as a
header without an argument list:

```raven
process! operation
process! await operation()
process! value + offset
```

The general source shape is:

```text
<macro-name-or-alias> ! <ExpressionSyntax> TokenBody?
```

An identifier is not a separate carrier kind; it is an
`IdentifierNameSyntax`, and therefore the smallest expression case:

```raven
process! operation
```

This is `ExpressionHeaderMacroCarrierSyntax`, not an `ArgumentListSyntax` with
omitted parentheses. It cannot contain multiple or named arguments.
Parenthesized invocation remains available for explicit argument-list behavior
or boundaries:

```raven
process!(operation)
configure!(mode: .Safe, retries: 3)
```

No macro name, including `do`, is special to this grammar. Carrier recognition
is uniform for canonical names and aliases.

The surface shape deliberately resembles Raven constructs with an expression
header followed by a body:

```raven
if condition {
    handleTrueCase()
}

while condition {
    continueWork()
}

guard! condition {
    reportFailure
}
```

The resemblance is structural, not semantic. `guard!` is still a macro alias,
and its braces contain a macro token body rather than necessarily a Raven
`BlockSyntax`.

The complete application may occupy either a statement or expression grammar
position when that position supports a macro application:

```raven
guard! condition {
    reportFailure
}

let result = transform! source {
    mapping rules
}
```

The outer position-specific syntax node records whether the application appears
as a statement or expression. The expression-header carrier only describes the
shape following `!`.

### Recreating match as an acceptance test

The expression-header carrier should be rich enough for a library macro to
recreate the prefix match statement and expression shell:

```raven
let label = match! value {
    0 => "zero"
    _ => "other"
}

match! value {
    0 => { Console.WriteLine("zero") }
    _ => { Console.WriteLine("other") }
}
```

In both forms, `value` is the carrier's `ExpressionSyntax` header and the
braces are its token body. The outer application position tells the macro
whether it appears as an expression or statement. The macro may parse each arm
itself while projecting the arm patterns, guards, result expressions, and
statement blocks through Raven's ordinary fragment categories.

The implementation may construct Raven `MatchExpressionSyntax` or
`MatchStatementSyntax`, or lower to other ordinary Raven syntax. The important
syntax test is that the carrier does not need a built-in `match` production to
preserve the same outer expression-plus-body shape. The required `!` continues
to disclose that `match!` is macro-provided.

### Recognition

The parser recognizes an expression-header carrier when:

1. a `NameSyntax` is immediately followed by `!`;
2. no line break occurs between `!` and the first expression token;
3. the next token begins an expression but is not `(` or `{`;
4. the containing grammar position permits an expression-header carrier; and
5. declaration-carrier lookahead has not already selected a declaration at a
   declaration boundary.

To preserve Raven's postfix `!` expression, an operator-leading header is not
recognized as an expression-header carrier. It must be parenthesized:

```raven
process!(-value)
process!(+offset)
```

This keeps an existing expression such as `value! + offset` from changing its
parse.

### Expression followed by a token body

The expression parser uses the same `stopOnOpenBrace` behavior already used by
Raven constructs whose expression is followed by a block:

```raven
process! operation {
    continuation
}

process! value + offset {
    continuation
}
```

The first top-level `{` terminates the expression and begins the macro token
body. No line break is permitted between the expression and that opening brace,
matching the existing argument-plus-body carrier.

Opening braces consumed inside a nested Raven expression remain part of the
expression. A top-level brace that could otherwise form an object initializer
or block expression is deliberately interpreted as the macro token body.
Parentheses provide the explicit alternative:

```raven
// `Widget { Value = 1 }` is the expression; the second braces are the body.
process! (Widget { Value = 1 }) {
    continuation
}
```

An immediate brace retains the token-only interpretation:

```raven
process! {
    tokens
}
```

It is not an unparenthesized block-expression argument. Use
`process!({ ... })` when a block expression itself is the input.

### Declaration-boundary ambiguity

At a declaration boundary, an identifier-bearing form whose remaining tokens
fit a declaration header keeps the declaration-shaped interpretation. This is
true with or without a token body:

```raven
component! Counter {
}

marker! GeneratedMember
```

A top-level or member-position expression-header carrier with the same shape
must use parentheses:

```raven
process!(operation)

process!(operation) {
    continuation
}
```

Within an ordinary expression or statement position,
`process! operation { ... }` uses the expression-header carrier. This
preference is syntactic and independent of which macros are in scope.

## Line breaks and termination

The following adjacency rules apply:

* no line break is permitted between the macro name and `!`;
* no line break is permitted between `!` and an expression header;
* no line break is permitted between a parenthesized or expression-header
  carrier and a trailing token body;
* without a trailing body, an expression-header carrier ends according to
  Raven's normal expression and statement-terminator rules;
* line breaks inside a nested or normally continued expression retain their
  ordinary Raven behavior.

These rules reject accidental capture such as:

```raven
process!
nextStatement()
```

## Macro-facing model and API

The public model should preserve the carrier distinction instead of
normalizing every form into optional `ArgumentList` and `TokenTree` properties.

At minimum, the macro-facing API should expose:

```text
MacroApplication
    Syntax: IMacroApplicationSyntax
    Name: NameSyntax
    Carrier: MacroCarrierSyntax
    Position: MacroApplicationPosition

MacroApplicationPosition
    Expression
    Statement
    Type
    Pattern
    Member
```

Carrier-specific code uses the carrier subtype:

```raven
match context.Application.Carrier {
    ParenthesizedMacroCarrierSyntax(args, body) => ...
    ExpressionHeaderMacroCarrierSyntax(expression, body) => ...
    TokenTreeMacroCarrierSyntax(body) => ...
    DeclarationMacroCarrierSyntax(header, body) => ...
}
```

The macro definition descriptor should declare an explicit set of accepted
carrier kinds. Declaration carriers may additionally describe which standard
header pieces and custom clauses they accept. This lets an author decide the
form while Raven continues to decide the grammar of that form.

### Authoring contract

The normalized authoring model should describe forms as position-and-carrier
pairs rather than deriving the entire invocation surface indirectly from a
context parameter:

```text
MacroDefinitionDescriptor
    ApplicationKind
    Forms: MacroApplicationFormDescriptor[]

MacroApplicationFormDescriptor
    Positions: MacroApplicationPosition[]
    Carrier: MacroCarrierDescriptor
    Parameters
    BodyRequirement
```

Each form remains strongly typed:

* a parenthesized form maps its `ArgumentListSyntax` to ordinary declared macro
  parameters;
* an expression-header form supplies exactly one `ExpressionSyntax` input and
  may independently require or allow a token body;
* a token-tree form supplies its bounded token body;
* a declaration form supplies `MacroDeclarationHeaderSyntax` and may expose
  its standard header pieces through typed convenience APIs.

The compact authoring syntax needs an explicit way to select a non-default
carrier. The exact spelling is left open, but the model should support a form
equivalent to:

```raven
macro Retry(
    operation: ExpressionSyntax,
    body: IMacroTokenStream
) -> StatementSyntax
    carrier expressionHeader
{
    ...
}
```

Here `carrier expressionHeader` changes the authored source envelope from
`Retry!(operation) { ... }` to `Retry! operation { ... }`. It does not change
the fact that `operation` is Raven `ExpressionSyntax`.

A macro name or alias may publish several forms. Each form should retain its
own typed entry point or overload so simple macro implementations do not need
to inspect an untyped union of every possible carrier. Candidate selection
first filters by authored position and carrier kind, then applies the normal
parameter-shape rules for that form.

The existing internal `FreestandingMacroInvocation` normalization and public
contexts should be changed if they erase carrier-specific structure. In
particular:

* a declaration carrier must not be represented as an invocation with a null
  argument list;
* an expression-header carrier must not be represented as a missing argument
  list;
* token-body access should be available uniformly on every carrier that may
  compose with a body;
* contexts should expose the complete application syntax and carrier rather
  than requiring consumers to switch among unrelated invocation properties.

Compatibility shims are optional and should not constrain the syntax model.

### Future declaration-authoring route

For the current phase, `macro` remains compiler-owned declaration syntax. This
proposal does not replace it with a `macro!` macro.

The normalized descriptor and carrier API should nevertheless avoid assuming
that every definition originated as `MacroDeclarationSyntax`. A future
declaration-shaped macro could produce the same `MacroDefinitionDescriptor`
from a structured declaration header and clauses. Reaching that point should
require another authoring front end, not another redesign of application
positions, carrier syntax, or the public execution API.

This is a forward-compatibility requirement on the model, not an implementation
slice in this proposal.

## Parser recovery

Once the parser consumes the macro name and `!` and selects a carrier, it
should preserve that carrier during recovery:

* `process!` at end of line produces a missing expression or carrier diagnostic
  without consuming the next statement;
* `process! expression {` produces an expression-header carrier with a token
  tree whose close brace is missing;
* an unterminated type-parameter, parameter, base, return, or constraint list
  remains represented by its ordinary Raven syntax node and missing tokens;
* an incomplete custom clause remains a `MacroDeclarationClauseSyntax` and
  does not absorb the following declaration after a blank line;
* a declaration carrier with no body remains structurally available even when
  a later contract requires one.

## Implementation slices

This is compiler infrastructure work in `Raven.CodeAnalysis`, not a library
macro implemented on top of the current carrier API. The owning layers are the
syntax model and parser first, followed by the normalized macro descriptor and
public authoring API. Expansion integration and editor tooling then consume
those compiler-owned facts.

The model can advance in independently reviewable slices:

1. Introduce `MacroCarrierSyntax` and migrate the existing parenthesized,
   token-tree, and declaration carriers to explicit subtypes.
2. Introduce the shared macro-application contract and replace normalized APIs
   that erase carrier shape.
3. Add `ExpressionHeaderMacroCarrierSyntax` using top-level-open-brace
   termination.
4. Add `MacroDeclarationHeaderSyntax` with type parameters, parameter lists,
   base-list or return-type suffixes, constraints, permits, and an optional
   token body.
5. Add ambiguity and recovery tests for postfix `!`, declaration carriers,
   object initializers, block expressions, multiline expressions, and missing
   delimiters.
6. Prototype `MacroDeclarationClauseSyntax` and settle its lossless boundary
   rules before exposing it as a stable public syntax shape.
7. Add statement, type, pattern, and member position nodes against the shared
   carrier contract, with focused examples for each supported pairing.
8. Extend compact and class-authored macro descriptors so authors explicitly
   select accepted position-and-carrier forms without losing strong typing.
9. Update the grammar, syntax factories, pretty-printer, classifier, TextMate
   grammar, and macro tooling alongside the implemented slices.

Changes to `Model.xml` require regeneration through the repository's normal
syntax-generator build path.

## Summary

* Macros select from explicit carrier shapes instead of one call-like shape
  with optional data.
* Position-specific application nodes share a reusable carrier hierarchy.
* Generic syntax before `!` supplies macro type arguments.
* Generic syntax after a declaration identifier declares type parameters.
* Parenthesized arguments and an expression header are distinct carriers.
* An identifier after `!` is the ordinary identifier-expression case, not a
  special name carrier.
* A top-level opening brace after an expression header starts the macro token
  body; parentheses retain braces inside the expression.
* Declaration-shaped syntax always keeps its ordinary Raven syntax category.
* Custom clauses cannot take ownership of standard `where`, base-list,
  return-type, or permits syntax.
* Breaking the experimental syntax and macro APIs is acceptable when necessary
  to preserve carrier shape and keep the model extensible.
