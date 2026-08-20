# Macros

Status: **Experimental**

This chapter specifies Raven's implemented macro application and declaration
model. Future forms are labeled explicitly and are not accepted source syntax.
The [authoring guide](../../macro-authoring.md) documents compiler APIs and
progressive implementation techniques.

## 1. Definition

A macro is an explicitly invoked compile-time program that consumes declared
inputs and produces ordinary Raven syntax or declaration contributions.
Expansion is procedural and syntax-based, not textual substitution.

The source file is parsed before macro resolution. The compiler then resolves
the macro, binds its inputs, executes it in the compile-time partition,
validates its output against the authored grammar position, and binds the
resulting Raven syntax normally.

Four dimensions are independent:

| Dimension | Examples |
| --- | --- |
| Application kind | freestanding; attached |
| Input | typed values; syntax nodes; token body; declaration carrier; context |
| Invocation target | expression; statement; member list |
| Optional capabilities | tokens; fragments; symbols; source origins |

A token body, declaration carrier, custom lexer, or editor provider does not
create another macro application kind.

## 2. Application kinds

### 2.1 Freestanding macros

A freestanding macro occupies an independent source location. Its invocation
uses a macro name followed by `!` and one of the supported carriers:

```raven
Name!(arguments)

Name! {
    body
}

Name!(arguments) {
    body
}

Name! Decl(parameters) {
    body
}
```

The first three forms are invocation carriers. The last is a
declaration-shaped carrier.

There must be no line break between the name and `!`, or between `!` (or its
argument list) and an opening body brace. This keeps the form distinct from an
ordinary postfix `!` expression.

### 2.2 Attached macros

An attached macro occupies an attribute-like position directly before a
declaration:

```raven
#[Observable]
public var Name: string
```

It may take positional or named arguments:

```raven
#[Observable("NameChanged")]
#[Observable(Name: "NameChanged", Notify: true)]
```

Attached macro attributes follow declaration-attribute placement rules. No
blank line may separate the macro attribute from its declaration. Ordinary and
macro attribute lists may be mixed in the same declaration prelude.

Exactly one macro declaration parameter marked with contextual `on` selects
attached application and its accepted declaration syntax:

```raven
macro Observable(
    enabled: bool = true,
    on property: PropertyDeclarationSyntax
) {
    if enabled {
        replace Rewrite(property)
        introduce CreateBackingField(property)
    }
}
```

The `on` parameter is compiler-supplied. It is not written in the attached
macro's argument list.

## 3. Declaring macros

Compact macro declarations are allowed at compilation-unit and namespace
member scope:

```raven
macro Double(value: int) -> ExpressionSyntax {
    expand ParseExpression((value * 2).ToString())
}
```

`macro` is contextual. At a declaration boundary it introduces a macro when
followed by its declaration name; it remains an ordinary identifier in other
contexts.

A declaration may contain attributes, modifiers, generic parameters, ordinary
parameters, constraints, a syntax return annotation, and a block or expression
body. A compact macro is compiled into Raven's compile-time partition and is
not emitted as an ordinary runtime function. Semantic APIs expose it as an
`IMacroDeclarationSymbol`, not as an `IMethodSymbol`.

Macro declarations are synchronous. `async` and `await` are not supported in a
macro declaration.

An ordinary class implementing `IMacroDefinition` may declare the same
method-shaped contract through its designated `Expand` method. Compact and
class-authored definitions normalize to the same symbol, descriptor, registry,
binding, execution, and tooling model. The lower-level provider and executor
interfaces are documented in the [authoring guide](../../macro-authoring.md).

## 4. Input binding and carriers

### 4.1 Parenthesized arguments

Ordinary value parameters and syntax parameters bind from `(...)` in
declaration order. Positional and named arguments, optional defaults, and the
compiler's supported constant conversions apply.

```raven
macro AddOffset(offset: int, value: ExpressionSyntax) {
    expand ParseExpression(value.ToString() + " + " + offset.ToString())
}

let result = AddOffset!(2, value)
```

For an ordinary value parameter, the compiler supplies a representable
compile-time constant. It does not execute an arbitrary caller expression.
For a syntax parameter such as `ExpressionSyntax`, the compiler supplies the
authored syntax node instead of its runtime value. Syntax parameters cannot
declare defaults.

Injected context parameters do not appear at the invocation site.
`FreestandingMacroContext` supplies argument-style services,
`TokenTreeMacroContext` supplies token-body services, and
`AttachedMacroContext` supplies attached-declaration services.

### 4.2 Brace-delimited bodies

One `IMacroTokenStream` parameter requests a required, lossless `{...}` body:

```raven
macro Query(dialect: string, body: IMacroTokenStream) {
    expand LowerQuery(dialect, body)
}

let rows = Query!("sql") {
    from user in users
    select user.Name
}
```

The body parameter is compiler-supplied and is not included in `(...)`. A macro
declaration may have at most one `IMacroTokenStream` parameter. It cannot have
a default value or be combined with an attached `on` parameter.

The body is the source of truth. The macro may read Raven-backed tokens,
provide a custom token stream, parse ordinary Raven fragments, or apply a
private grammar. These interpretations remain scoped to the invocation and do
not add global Raven token or syntax kinds.

`TokenTreeMacroContext` alone requests services; it does not imply a body. The
presence of `IMacroTokenStream` is what requires `{...}`.

### 4.3 Declaration-shaped carriers

A declaration-shaped carrier is a freestanding macro invocation at a
declaration boundary:

```raven
public component! Greeting(Name: string = "") {
    markup! { <h1>Hello {Name}</h1> }
}
```

The carrier preserves its modifiers, macro name, declared identifier,
declaration parameter list, and body as structured syntax. A parameter of type
`FreestandingMacroDeclarationSyntax` requests the complete carrier. A separate
`IMacroTokenStream` parameter requests its body:

```raven
macro FunctionComponent(
    declaration: FreestandingMacroDeclarationSyntax,
    body: IMacroTokenStream,
    context: TokenTreeMacroContext
) -> MemberDeclarationSyntax {
    expand LowerComponent(declaration, body, context)
}
```

In `component! Greeting(Name: string)`, the parentheses contain parameters of
the declaration being introduced. They are not caller arguments to
`FunctionComponent`.

Declaration-shaped carriers are parsed at compilation-unit, namespace, and
type-member boundaries. Resolution must select a macro descriptor that accepts
the declaration carrier. Its result must be valid in the containing
declaration list.

### 4.4 Future sequence carrier

`Name![item1, item2]` is reserved design direction and is not implemented.
Its intended reading is a variable number of homogeneous items that the macro
processes to produce a result. The application-model proposal associates it
with a compiler-owned `MacroList<T>` input: ordinary `T` would request value
conversion for each item, while a syntax type would preserve each authored
item node.

This form is not an alternate spelling for one collection argument.
`Collect!([1, 2, 3])` passes one ordinary collection expression through a
parenthesized parameter. A future `Collect![1, 2, 3]` would bind three
independent macro items.

No meaning is currently assigned to `Name![...] { ... }` or to a form combining
all three envelopes. The grammar and execution model should remain open to
such combinations if a concrete use case gives them a clear reading.

## 5. Invocation targets and output

For a freestanding macro, the syntax return annotation declares its permitted
grammar target. It does not declare the runtime value type of an expanded
expression.

| Annotation | Target |
| --- | --- |
| omitted | expression |
| `ExpressionSyntax` | expression |
| `StatementSyntax` | statement |
| `ExpressionSyntax | StatementSyntax` | expression or statement |
| `SyntaxNode` | every supported single-node target |
| `MemberDeclarationSyntax` or subtype | one compatible declaration member |
| `SyntaxList<TMember>` | zero or more compatible declaration members |

A bare raw-body invocation used as the whole statement selects statement
placement. Parentheses explicitly retain expression placement. Call-style
macros follow ordinary expression-statement rules.

At file and namespace scope, an invocation-shaped `Name! { ... }` is initially
retained in a global-statement envelope because its output cannot be known
before semantic resolution. A declaration result supplies file or namespace
members; a statement result retains statement behavior. In a type body the
parser uses a member carrier directly.

The compiler validates the complete result atomically against the actual
target. A category mismatch reports `RAVM022` and does not insert the invalid
node. A list result preserves source order. An explicitly empty member list
removes the carrier.

Generated declarations participate in ordinary declaration lookup, binding,
expanded documents, and emission.

## 6. Contributions and diagnostics

Macro bodies use contextual contribution constructs:

- `expand value` supplies a freestanding expansion and immediately returns
  from the current execution path;
- `replace declaration` sets the replacement for an attached declaration and
  permits execution to continue;
- `introduce memberOrMembers` appends attached members in execution order;
- `fragment region` contributes an ordinary Raven fragment for editor tooling;
- `token info` contributes classified or symbol-bearing DSL token metadata.

These constructs also have expression forms where their control-flow behavior
permits it. `replace` and `introduce` accumulate until the body ends. A later
`replace` supersedes an earlier replacement.

Expected invalid input must be reported as diagnostics. Context APIs accumulate
diagnostics independently of the expansion, so diagnostics survive both
`expand` and normal body fall-through. Parser recovery, incomplete regions,
and diagnostics are preferred to throwing. An exception represents a macro or
provider defect and must not destabilize unrelated semantic queries.

## 7. Resolution, names, and aliases

A macro has a case-sensitive canonical name and may declare a case-sensitive
alias with `MacroAlias`. Macro lookup uses the invocation's namespace and
imports. A canonical fully qualified name remains available independently of a
wildcard-imported alias.

Aliases do not become lexical keywords. They resolve contextually through the
macro registry and can be shadowed by an ordinary local name. After successful
resolution, IDEs present the alias token with the contextual-keyword semantic
classification. Canonical macro names retain the macro classification. This
presentation rule applies equally to invocation and declaration-shaped forms
and is shared by language-server clients and the Playground.

Completion, signature help, hover, navigation, diagnostics, and expansion must
consume the same compiler-owned descriptor and resolution result. Language
services must not maintain an independent macro registry.

An identifier-bearing declaration-shaped carrier contributes one document
outline entry whose name and selection range come from its authored identifier.
The macro invocation spelling may be presented as entry detail. Generated
members are not duplicated beneath that authored entry, and a member-position
macro invocation without an identifier does not contribute an outline entry.

## 8. Expansion and composition

Macro expansion is part of semantic compilation, not a lexical preprocessor
pass. Expansion results retain authored-source relationships for diagnostics,
semantic services, expanded views, and executable source origins.

An expansion may contain another macro invocation. Nested invocations resolve
using the namespace and imports at their authored outer source location. When
they appear inside an ordinary Raven fragment reported by an outer macro, they
inherit the lexical bindings visible at that position. Recursive fragment
lookup selects the most specific nested region and is bounded for recovery.

When several attached macros apply to one declaration, Raven runs them in
source order. Each macro receives the original target and the current
replacement shape. Introduced members from all macros precede the effective
declaration, the last replacement wins, and peer declarations follow it;
contribution order is otherwise preserved.

Parent declaration macros see the parent's parsed member shape rather than
assuming that child declarations have already been rewritten. Macro
cooperation should therefore use explicit contracts instead of relying on
incidental traversal order.

## 9. Token DSLs and embedded Raven

The default macro token stream uses Raven's lexer while retaining body-relative
and absolute authored spans. A macro may overlay body-scoped keywords without
changing the underlying Raven token kind. A macro with a different lexical
grammar may provide a custom stream with provider-owned raw kinds.

A macro can identify expression, statement, type, pattern, member, compilation
unit, or block fragments inside its private DSL. Fragment metadata exposes the
category, authored span, optional expected type, and optional introduced locals
without exposing the DSL's private parse tree.

Declaration-shaped macros may project typed header parameters into a reported
block fragment. A nested macro within that block then observes those parameters
through normal semantic lookup. Token-symbol associations similarly let a DSL
token, such as a component tag or attribute, reuse ordinary Raven hover and
go-to-definition presentation.

Token, fragment, classification, and symbol contributions are optional
capabilities. Failure in one capability degrades that metadata request without
invalidating unrelated binding or semantic queries. Providers must honor
cancellation.

## 10. Local and packaged macros

A macro declared in the same project belongs to a compiler-owned local macro
partition. Its implementation is compiled and activated before consumer
binding, excluded from runtime emit, and included in diagnostics and language
services. Supporting types may be marked with `LocalMacro` when a source file
contains both compile-time and runtime declarations.

A reusable macro project marks its output with `RavenCompilerPlugin`. Consumers
use an ordinary project, assembly, or package reference. The marked provider is
activated as a compiler plugin rather than treated as an application runtime
reference. Raven does not scan unmarked runtime references for macro types.

Macro source may depend on metadata and other macro plugins, but the local
compile-time partition cannot depend on consumer declarations. This keeps the
partition dependency graph acyclic.

The active macro registry belongs to an immutable compilation snapshot. A
change to macro source, macro references, relevant compiler options, or
dependencies produces a new registry and invalidates dependent expansions.

## 11. Standard macros

The `Raven.Macros` library currently includes:

- `quote! { expression }`, which captures one Raven expression as immutable
  syntax and supports `#(expression)` syntax holes;
- `compile<TDelegate>! { expression }`, which quotes and compiles a delegate at
  runtime;
- `embedFileContent!(path)`, which embeds UTF-8 file content and records the
  file as an incremental compilation input;
- `sha256Digest!(literal)`, which computes a compile-time SHA-256 digest.

Their lowercase aliases enter scope through `import Raven.Macros.*`. Canonical
names such as `Raven.Macros.Quote!` remain available without the wildcard
import. These macros use the same declaration, registry, resolution,
diagnostic, and expansion model as other macro libraries.

Expression quotation currently accepts exactly one expression. Statement,
member, declaration, token, identifier, list, and repetition quote forms are
not implemented.

## 12. Current boundaries

The implemented model supports typed constant and syntax inputs, token bodies,
declaration-shaped carriers, expression and statement targets, single and
list-valued declaration expansion, attached declaration transforms, nested
expansion, source-located diagnostics, and span-based editor integration.

The following remain future design work:

- the `[...]` sequence carrier and `MacroList<T>`;
- type and pattern invocation targets;
- typed syntax facades that preserve both semantic type information and
  authored syntax;
- broader custom scope models beyond the current fragment locals and
  declaration parameters;
- a general public structured-DSL tree contract.

These boundaries do not prevent a macro from maintaining a private parser or
syntax tree. They describe what the Raven compiler currently recognizes and
shares across macro, semantic, and language-service boundaries.
