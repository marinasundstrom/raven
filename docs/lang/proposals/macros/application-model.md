# Macro application model

Status: **MVP, member-list, and declaration-carrier slices implemented;
sequence input remains proposed**

This proposal defines where macros can be applied, what a macro declaration
must communicate, and how the compiler validates expansion. It does not change
compiler behavior by itself.

The proposed structured declaration headers and expression-header invocation
form are specified separately in
[Macro carrier syntax shapes](carrier-syntax-extensions.md).

The stable method-shaped declaration model and erased provider dispatch
boundary are defined separately in [Macro ABI](abi.md). This document owns the
application positions and normalized semantic facts that the ABI carries; it
does not preserve the current generated adapter or parameter-object layout.

Raven macros are procedural macros with one of two application kinds:
**Freestanding**, meaning the macro appears independently at any declared
grammar position, or **Attached**, meaning it occupies an attribute-like
position on an existing declaration.

The intended user experience is native Raven at the boundary and macro-owned
language inside a token body. Declaration and invocation carriers establish a
normal Raven name, signature, source location, and grammar position. The braces
then open a bounded region whose contents may follow any DSL grammar. Optional
capabilities reconnect that region to Raven fragments, semantic symbols, or an
embedded editor language without requiring the private grammar itself to become
part of Raven's parser.

The central rule is that application position, input representation, output
syntax, and optional capabilities are independent dimensions. Token bodies,
editor metadata, and custom DSL structure are not separate macro kinds.

## Future bootstrap direction

The compiler-owned `macro` declaration is a bootstrap implementation, not
necessarily the permanent implementation boundary. A future Raven distribution
could define the declaration's meaning and generated adapter through the public
macro API itself, in the same spirit as a macro system implementing one of its
own higher-level authoring forms. Doing so would provide a demanding proof that
the API can express real language features and would let macro authoring evolve
without coupling every change to the compiler core.

That direction still requires a small parser-owned carrier. Raven must preserve
the function-shaped header, body, and extensible declaration clauses as lossless
syntax before any macro implementation can interpret them. The current uniform
`capability by Handler` clauses intentionally keep that boundary shallow: they
are declarative header data whose meaning can later move out of the compiler,
not complex statements embedded in expansion control flow. Any bootstrap design
must also define ordering, diagnostics before macro activation, and how the
macro that implements `macro` itself is made available.

The parser-side carrier should eventually standardize a declaration clause
list shared with constructs such as generic `where` constraints. Its stable
shape is a declaration header, followed by zero or more constraint and
extension clauses, followed by the body. Every clause needs a leading
contextual keyword, a bounded payload, consistent newline or terminator rules,
and a distinct syntax node. A macro-defined declaration can then consume
structured clauses predictably instead of reparsing arbitrary header text.

For a freestanding macro, the declared **return type decides its
invocation target**. It is not an ordinary runtime return-type annotation:
`ExpressionSyntax` makes the macro freestanding in expression targets,
`StatementSyntax` makes it freestanding in statement targets, and a union declares
several targets. `expand` must then produce syntax valid for the actual target.

## Goals

* Keep the simplest expression macro concise.
* Let one macro support several grammatical positions deliberately.
* Keep single-position class APIs strongly typed.
* Provide an advanced multi-position API without unsafe insertion.
* Let the parser recognize carriers without loading plugins.
* Make diagnostics, completion, hover, and expansion agree on positions.

## Independent dimensions

| Dimension | Examples |
| --- | --- |
| Application | freestanding; attached to a declaration |
| Input | constants; syntax nodes; token body; compiler context |
| Return type / invocation target | expression; statement; member; type; pattern |
| Cardinality | one node; a list in a list-valued grammar position |
| Contributions | expand; replace; introduce members; introduce peers |
| Capabilities | tokens; fragments; hover; completion; navigation |

The normalized contract records these separately. A macro does not become a
new kind merely because it uses a token body or supplies hover metadata.

## Canonical source syntax

This document owns the proposed source syntax used by the other macro design
documents. Examples elsewhere should use these forms rather than infer a
different call shape from a context type or reinterpret the return annotation.

```raven
macro Double(value: int) -> ExpressionSyntax {
    expand ParseExpression((value * 2).ToString())
}

macro Query(
    dialect: string = "raven",
    body: IMacroTokenStream,
    context: TokenTreeMacroContext
) -> ExpressionSyntax {
    expand LowerQuery(dialect, body, context)
}

macro Markup(context: TokenTreeMacroContext) -> ExpressionSyntax
    highlighting by ClassifyMarkupToken
    fragments by GetMarkupFragments
    completion by MarkupCompletion.GetItems
    projection by ProjectMarkup
{
    expand LowerMarkup(context)
}

macro Observable(enabled: bool = true,
    on property: PropertyDeclarationSyntax) {
    if enabled {
        replace Rewrite(property)
    }
    introduce CreateBackingField(property)
}

macro AnyOf(items: MacroList<ExpressionSyntax>) -> ExpressionSyntax {
    expand LowerDisjunction(items)
}
```

The declaration rules are:

* `macro Name<T>(...) -> OutputSyntax` reuses Raven's ordinary declaration
  name, generic parameter, parameter, constraint, and block spellings.
* Ordinary value and syntax-input parameters are candidates for `(...)` and
  bind with normal positional, named, optional, and conversion rules. This
  includes ordinary collection types such as `List<T>`; their type alone does
  not select a different envelope.
* A parameter of type `MacroList<T>` explicitly requests the alternative
  `[...]` envelope. The compiler binds each comma-separated item independently
  as `T`. An ordinary `T` receives a converted compile-time value, while a
  syntax type such as `ExpressionSyntax` receives the authored item node.
  `MacroList<T>` is an immutable compiler-owned sequence that retains item
  locations.
* A parameter of type `IMacroTokenStream` explicitly requests the lossless
  `{ ... }` body after an optional `(...)` group. A macro context parameter
  only requests compiler services; it never implies a body by itself.
* Exactly one contextual `on` parameter selects attached application. An
  attached declaration has no freestanding output annotation.
* For a freestanding macro, the return annotation is always an ordinary Raven
  syntax type or supported syntax list. It declares output category and
  invocation positions, not the runtime type later inferred for an expanded
  expression. Omitting it is shorthand for `-> ExpressionSyntax`.
* `expand` terminates a freestanding expansion. `replace` and `introduce`
  accumulate attached contributions, which are finalized on body fall-through.
* A declaration-level capability clause has the uniform form
  `capability by Handler`. It projects an existing optional macro interface onto
  the generated adapter and forwards the interface member to an ordinary
  namespace function or qualified static function. It does not introduce a
  statement or a nested service scope in the expansion body.
* `keywords`, `tokens`, `tokenKinds`, `highlighting`, `fragments`, `symbols`,
  `completion`, and `projection` are the initial capability keywords. Each may
  appear at most once and requires token-tree input.

The delimiters communicate author intent:

| Carrier | Reading | Binding model |
| --- | --- | --- |
| `Name!(...)` | “Pass these arguments into the macro.” | A fixed parameter signature with ordinary positional, named, optional, and conversion rules. |
| `Name![...]` | “Process this variable number of items and produce a result.” | One homogeneous sequence whose element type decides value conversion versus syntax capture. |
| `Name! { ... }` | “Process this bounded region of content.” | One lossless region that may be raw, tokenized, or privately structured by the macro. |

The square-bracket form is therefore not an alternate spelling for passing one
collection value. `Collect!([1, 2, 3])` passes one ordinary collection
expression through a `(...)` parameter; `Collect![1, 2, 3]` supplies three
independently bound macro items. Likewise, the brace form is not an object or
block value passed as an argument. It establishes the source region owned by
the macro's content processor.

Those declarations correspond to these call-site carriers:

| Declaration shape | Invocation shape |
| --- | --- |
| caller parameters only | `Double!(21)` |
| one `MacroList<T>` | `AnyOf![ready, retries > 0]` |
| `IMacroTokenStream` and no caller parameters | `Query! { ... }` |
| caller parameters plus `IMacroTokenStream` | `Query!(dialect: "sql") { ... }` |
| one `on` parameter | `#[Observable]` or `#[Observable(enabled: false)]` |

The parameter envelope is `Name!(...)`, optionally followed by a region as
`Name!(...) { ... }`; either group may be omitted when its role is absent. The
initial sequence slice proposes `Name![...]` alone. The declaration and ABI
model do not make it permanently exclusive, but `Name![...] { ... }` and
`Name!(...)[...] { ... }` have no proposed meaning. They remain unassigned
design space until a motivating case gives them a comparably clear reading.
A `MacroList<T>` parameter may always
request injected context services. The initial list grammar accepts only
comma-separated Raven expressions with an optional trailing comma. Spreads,
comprehensions, and dictionary entries remain ordinary expression features
inside a parenthesized argument and are not list-envelope items.

Macro identifiers are case-sensitive and an invocation uses the resolved
declaration name exactly. A separately declared alias may provide another
spelling, such as the standard library's lowercase `query` alias; casing is not
implicitly rewritten by the macro grammar.

## Freestanding positions

### Expression

An expression macro occupies an expression slot and produces exactly one
`ExpressionSyntax`:

```raven
macro Sql(body: IMacroTokenStream,
    context: TokenTreeMacroContext) -> ExpressionSyntax {
    expand LowerQuery(body, context)
}

let rows = Sql! { select * from users }
```

The proposed default for an omitted return type is `ExpressionSyntax`, making
the macro freestanding only in expression targets.

### Statement

A statement macro occupies one statement slot and produces one
`StatementSyntax`. A block represents several runtime statements:

```raven
macro Trace(body: IMacroTokenStream,
    context: TokenTreeMacroContext) -> StatementSyntax {
    expand BuildTraceBlock(body, context)
}

func Run() {
    Trace! { operation() }
}
```

### Expression or statement

A multi-position macro declares its closed output set:

```raven
macro Evaluate(body: IMacroTokenStream,
    context: TokenTreeMacroContext)
    -> ExpressionSyntax | StatementSyntax {
    match context.Position {
        .Expression => expand BuildExpression(context)
        .Statement => expand BuildStatement(context)
    }
}
```

The source declaration uses Raven's union notation for static checking. The
advanced plugin ABI may carry the value as `SyntaxNode`; the driver validates
it against the actual invocation carrier before insertion.

### Flexible single-node output

`SyntaxNode` is the explicit wildcard for a macro that intentionally supports
every single-node freestanding position known to the compiler:

```raven
macro Forward(body: IMacroTokenStream,
    context: TokenTreeMacroContext) -> SyntaxNode {
    expand BuildFor(context.Position, body, context)
}
```

This is an expert escape hatch. It makes the macro eligible in expression,
statement, single-member, type, and pattern positions, so its implementation
must inspect `context.Position` and the driver must validate every result.

`SyntaxNode` does not include attached application, replacement,
introductions, peer contributions, or list-valued member expansion. Those have
different input or cardinality contracts. A precise union such as
`ExpressionSyntax | StatementSyntax` remains canonical whenever the intended
positions are known.

In the macro model, `SyntaxNode` is Raven's equivalent of an **untyped macro
output**: the concrete syntax category has been erased. It remains an
immutable, structured Raven syntax node with a kind, children, tokens, spans,
and provenance—not dynamic data or raw text. The invocation carrier supplies
the required category, and the driver performs the checked cast by diagnosing
a mismatch rather than throwing.

The return-type-to-target projection is therefore:

| Return type | Declared invocation targets |
| --- | --- |
| omitted | expression |
| `ExpressionSyntax` | expression |
| `ExpressionSyntax \| StatementSyntax` | expression and statement |
| `SyntaxNode` | every supported single-node freestanding position |

### Member

A member macro occupies a namespace-member or type-member list position:

```raven
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro Properties(body: IMacroTokenStream,
    context: TokenTreeMacroContext)
    -> SyntaxList<MemberDeclarationSyntax> {
    let properties = List<MemberDeclarationSyntax>([
        BuildIdProperty(context),
        BuildNameProperty(context)
    ])
    expand properties
}
```

Member positions naturally need zero-or-more output. Raven uses the existing
immutable `SyntaxList<TMember>` compiler API as that source-level contract; it
does not add a macro-only `T*` type spelling or a new keyword. `TMember` must be
`MemberDeclarationSyntax` or one of its syntax subtypes. An empty list removes
the invocation carrier, and a nonempty list preserves source order.

The normalized expansion result stores the output as
`ImmutableArray<MemberDeclarationSyntax>`. The generated adapter copies the
source `SyntaxList<TMember>` into that result, preserving each node and its
provenance. `CompilationUnitSyntax` is not accepted as an accidental list
container.

`SyntaxList<TMember>` declares the namespace-member and type-member target set.
The actual carrier records which of those positions was authored. Before any
member is inserted, the compiler validates every returned node against that
position. Validation is atomic: an invalid member produces diagnostics and the
entire list is discarded, so binding, language services, and emission never
observe a partial expansion. A macro that supports both positions can inspect
`context.Position` when its output differs between them.

List-valued member output is a distinct cardinality contract. It is not
included by `SyntaxNode`, cannot be mixed into a single-node return union, and
does not imply attached replacement or introduction.

### Declaration-form carrier

Raven extends freestanding macros with a declaration-oriented invocation
carrier for constructs that introduce declarations. This is an invocation form,
not a third application kind:

```raven
func Foo(x: int) {
}

component! Foo(x: int) {
}

public component! Header(title: string) {
}

Actor! OrderProcessor(mailbox: .bounded(100)) {
    receive order: SubmitOrder {
        process(order)
    }
}
```

The first two declarations are intentionally parallel. `component!` occupies
the declaration-introducer position that `func` occupies in an ordinary
function declaration. `Foo` occupies the declaration-name position,
`(x: int)` is a declaration parameter list rather than a macro argument list,
and `{...}` is the declaration body. The `!` preserves an explicit macro
boundary while Raven retains ownership of that surrounding structural grammar.

Modifiers occupy ordinary modifier positions. The macro must not recover the
modifiers, declared name, parameters, or body by scanning raw tokens. These are
separate structured carrier inputs even when the macro chooses to expose the
body itself as a token stream or private DSL region.

The compiler should expose the carrier as structured inputs associated with the
same canonical `Expand` method and descriptor used by every other freestanding
macro. Conceptually the descriptor records the authored modifiers, declared
name, declaration parameter list, body, and actual grammar position. A
different declaration-oriented macro may deliberately choose an invocation-
argument payload instead, as the `Actor!` sketch does; that carrier shape must
be declared rather than guessed from parentheses. Compact `macro` syntax needs
concise parameter roles for these structured declaration parts, and an ordinary
`IMacroDefinition` class must be able to express the equivalent signature close
to the ABI.

This means parentheses have a carrier-relative interpretation:

| Carrier | Parentheses contain |
| --- | --- |
| `Query!(dialect: "sql") { ... }` | macro invocation arguments |
| `component! Foo(x: int) { ... }` | parameters of the declaration being introduced |

The parser can distinguish these forms without loading a plugin because the
declaration carrier has a declared name between `!` and `(` and occurs at a
declaration boundary. Semantic resolution then verifies that the macro's
descriptor accepts that declaration shape.

Placement follows the existing output-compatibility rule. A declaration-form
macro may appear only where its declared result is structurally compatible
with the expected syntax category. A macro returning
`ClassDeclarationSyntax`, for example, is valid in a compatible member list but
not in a function body that expects a statement. A list-valued result may
introduce multiple declarations only where the carrier occupies a compatible
declaration list. The compiler validates the complete result atomically.

This direction permits framework forms such as `component!`, `Actor!`,
`service!`, or `workflow!` without promoting their names to Raven keywords or
allowing plugins to replace Raven's grammar. A body may still host a macro-owned
DSL through the existing token, fragment, completion, hover, and navigation
capabilities.

The first implemented projection exposes the complete carrier as
`FreestandingMacroDeclarationSyntax`. A separate `IMacroTokenStream` parameter
projects its body, so a macro may consume both the structured declaration and
the raw region without rescanning the header. More granular typed carrier
facades remain future work. They must extend the current freestanding
descriptor and expansion pipeline rather than create a parallel discovery,
ABI, or execution path.

### Freestanding invocation envelopes

Freestanding macros select one of two explicit invocation families:

```raven
Values![1, 2, 3]

Arguments!(a, b)

Body! {
    // body
}

Combined!(a, b) {
    // body
}
```

These names are illustrative; they do not propose built-in or standard macros.
Method-like `(...)` arguments, a variable-cardinality `[...]` sequence, and a
structured or macro-owned `{...}` region are separate input roles. The
currently motivated composition is `(...)` plus `{...}`: at a call site it
reads as configured processing of a trailing region, and in a declaration
carrier `component! Foo(x: int) { ... }` mirrors ordinary declaration grammar.
`Name![...] { ... }` has neither rationale today. The grammar and ABI avoid
making it impossible, but the proposal does not assign it semantics merely for
symmetry. The declaration-form carrier composes the parameter and region roles
with declaration modifiers and a name instead of creating a separate macro
kind.

An envelope does not introduce a delimiter-specific execution ABI. Raven owns
recognition and balancing of its delimiters, while the descriptor records each
parameter's binding role and the canonical `Expand` signature receives the
projected syntax or values. `MacroList<T>` selects the list family. Future
typed syntax facades may express that a list item is an expression of a
particular type while still preserving its authored syntax;
`MacroList<ExpressionSyntax>` remains the category-only form.

The parser and language services must recognize the envelope without loading or
executing the macro. Completion, signature help, diagnostics, navigation, and
incremental invalidation then consume the same registered descriptor used by
ordinary and declaration-form Freestanding invocations. The list envelope and
`MacroList<T>` binding source are a breaking design direction and are not
implemented yet.

### Type

A type macro occupies a type slot and produces one `TypeSyntax`:

```raven
macro QueryRow(body: IMacroTokenStream,
    context: TokenTreeMacroContext) -> TypeSyntax
```

Type expansion affects signatures and incremental declaration binding. It is
part of the complete design but follows expression, statement, and member
support in implementation order.

### Pattern

A pattern macro occupies a pattern slot and produces one `PatternSyntax`:

```raven
macro MessageShape(body: IMacroTokenStream,
    context: TokenTreeMacroContext) -> PatternSyntax
```

Expansion occurs before binding so introduced variables, exhaustiveness,
narrowing, hover, and rename use ordinary Raven semantics.

## Attached application

An attached macro declares a compiler-supplied target parameter with `on`:

```raven
macro Component(on target: ClassDeclarationSyntax) {
    replace ImplementComponent(target)
    introduce CreateSupportMembers(target)
}
```

The modifier identifies the parameter role; its name has no semantic meaning.
Its type decides where the macro can be attached, just as a freestanding macro's
return type decides where it can be invoked:

```raven
macro Serializable(
    mode: SerializationMode,
    on target: ClassDeclarationSyntax | RecordDeclarationSyntax
) {
    replace AddSerialization(target, mode)
}
```

`on target: SyntaxNode` is category-untyped and accepts every attachable single
syntax node. It remains distinct from an ordinary invocation parameter
`node: SyntaxNode`; attachment is never inferred from a name or type alone.

Attached macros operate on an existing declaration. Their contributions are
replacement, introduced members, introduced peers, diagnostics, and editor
metadata. They have no freestanding return target. Combining an `on` parameter
with a freestanding syntax return type is invalid.

Potential targets include type, method/function, property, field, event,
constructor, accessor, and parameter. File, namespace, module, and assembly
targets require separate justification.

## Parameter binding

Macro parameters have explicit compiler roles. Binding partitions them before
mapping invocation arguments:

| Role | Declared form | Supplied by |
| --- | --- | --- |
| Value | `mode: Mode` | positional or named invocation argument |
| Syntax input | `expression: ExpressionSyntax` | authored invocation syntax |
| List input | `items: MacroList<T>` | comma-separated `[...]` items |
| Context | a recognized macro context type | compiler |
| Token stream/body | a recognized token-body type | compiler |
| Attached target | `on target: TargetSyntax` | compiler |

Only ordinary value and syntax-input parameters participate in positional and
named `(...)` argument mapping. A list input consumes the `[...]` payload, and
a token-body input consumes the `{...}` region. Compiler-supplied parameters
never consume any of those inputs and cannot be named by the caller.

### Carrier shape versus input projection

The carrier determines which authored regions exist; the resolved macro
signature determines how each region is projected. Parsing does not decide
whether an argument is a value or syntax, and it does not load a macro provider.

For example, one invocation can mix projections deliberately:

```raven
macro Repeat(count: int, value: ExpressionSyntax) -> ExpressionSyntax {
    // count is a converted compile-time value.
    // value is the source-backed expression node authored by the caller.
}

Repeat!(3, CalculateValue())
```

Binding `count` performs the supported compile-time evaluation and conversion
to `int`. Binding `value` preserves the `ExpressionSyntax`, its trivia,
location, and source identity without evaluating it. A different candidate may
declare a different projection for the same argument position; overload or
candidate selection must validate the complete signature before executing any
macro.

The same rule applies to sequence items. `MacroList<int>` converts every
`[...]` item to an `int`, while `MacroList<ExpressionSyntax>` preserves every
item as authored expression syntax. A conversion or syntax-category failure is
diagnosed at the responsible item and prevents execution with a partial input.

Typed syntax facades extend the syntax projection rather than replace it:

```raven
ExpressionSyntax<T>        // implemented
LiteralExpressionSyntax<T> // future fixed-node facade
```

`ExpressionSyntax<T>` preserves the ordinary source-backed expression and its
actual bound type while carrying a compiler-verified constraint that the result
is compatible with `T`. A future `LiteralExpressionSyntax<T>` would additionally
constrain the syntax shape. These are macro input facades over ordinary
immutable syntax, not new nodes inserted into Raven's syntax tree and not
evaluated runtime values.

A dynamic context remains the escape hatch when a macro deliberately wants to
interpret raw argument syntax itself. The normal strongly typed path should be
signature-directed so binding, diagnostics, signature help, hover, caching,
and execution all agree on the chosen projection.

### Context is opt-in

A macro context is not mandatory syntax and is not implicitly bound into every
macro body. The minimal macro consists only of caller inputs and an output
target:

```raven
macro Double(value: int) -> ExpressionSyntax {
    let doubled = value * 2
    expand ParseExpression(doubled.ToString())
}
```

The author declares a context parameter only when the implementation needs
advanced compiler services:

```raven
macro Query(body: IMacroTokenStream,
    context: TokenTreeMacroContext) -> ExpressionSyntax {
    let stream = context.CreateTokenStream()
    expand ParseAndLower(stream)
}
```

Likewise, a simple attached macro needs only its target:

```raven
macro Component(on target: ClassDeclarationSyntax) {
    replace ImplementComponent(target)
}
```

The compiler may maintain internal invocation state to execute any macro, but
that implementation detail does not create a source-level parameter, local, or
binding. Context construction and its semantic services should remain lazy
where practical.

### Syntax inputs and expanded semantic types

A syntax-typed parameter receives the authored argument as source-backed syntax
without evaluating it:

```raven
macro evaluate(expr: ExpressionSyntax) -> ExpressionSyntax {
    expand Transform(expr)
}

let x = evaluate!(2 + 3)
```

`expr` is the `ExpressionSyntax` for `2 + 3`, including its authored spans and
trivia. It consumes one ordinary invocation argument and therefore participates
in positional and named argument binding. The macro can inspect or transform
the node without requesting a context. If it needs symbols or types, it opts in
to an appropriate context parameter and asks the compiler semantic APIs about
that source-backed node.

A more specific existing syntax-node type constrains the authored shape. For
example, `LiteralExpressionSyntax` accepts a literal node but not an infix
expression. This is a normal checked syntax conversion; a mismatch is diagnosed
before the macro executes. The design does not require a new node such as a
numeric-expression node merely to describe semantic numeric compatibility.

Plain `ExpressionSyntax` describes the **syntax category**, not the semantic
type of the expanded expression. After expansion, Raven binds the ordinary
returned expression in its invocation context. Hover and downstream type
checking use that bound type rather than `ExpressionSyntax`.

The first typed-contract slice additionally supports `ExpressionSyntax<T>` at
macro input and output boundaries:

```raven
macro RequireInt(value: ExpressionSyntax<int>) -> ExpressionSyntax<int> {
    expand value.Syntax
}

macro ProduceInt() -> ExpressionSyntax<int> {
    expand SyntaxFactory.ParseExpression("42")
}

let value: int = RequireInt!(ProduceInt!())
```

For input, the compiler binds the authored expression, requires an implicit
conversion to `T`, and only then executes the macro. The macro receives a
macro-infrastructure facade containing the original source-backed `Syntax` node
and its actual bound `Type`; the expression is not evaluated. For output, the
macro still expands to an ordinary expression node. Raven binds that expansion
and requires an implicit conversion to the promised `T` before exposing the
result to the consuming expression context.

The facade is not a syntax node, does not appear in ordinary Raven syntax
trees, and does not extend the generated syntax-node hierarchy. Plain
`ExpressionSyntax` remains the category-only form. Class-authored providers
express the same output promise through `IMacroDefinition.ExpressionResultType`
while continuing to return an ordinary expression or expansion result.

The result contract therefore has three independent axes:

* the application position and output syntax category, such as expression;
* an optional semantic result-type constraint, such as `T` in
  `ExpressionSyntax<T>`; and
* an optional fixed syntax-node shape, such as the literal shape in
  a future `LiteralExpressionSyntax<T>` or another dedicated facade over an
  existing expression node kind.

Returning plain `ExpressionSyntax` continues to mean “an expression of any
semantic type and any expression node kind.” A typed wrapper narrows the
semantic type without creating a new grammar position. A fixed-node wrapper
narrows the existing syntax hierarchy without letting a macro invent new Raven
syntax kinds. The normalized macro descriptor should retain these constraints
separately so resolution, expansion validation, hover, and contextual typing
can agree on the promise.

Generic constraint inference and fixed-node facades remain future work.
Semantic promises stay separate from grammatical invocation targets and are
checked without consulting replacement-aware public semantic queries, avoiding
expansion binding cycles.

### Binding order

1. Classify roles from explicit syntax (`on`) and recognized compiler API
   types.
2. Validate the declaration shape before registering the macro.
3. Select a macro whose invocation or attachment target matches the carrier.
4. Map positional arguments to user-supplied parameters in declaration order.
5. Map named arguments; diagnose unknown, duplicate, or already-bound names.
6. Bind syntax inputs as source-backed syntax without evaluating them.
7. Evaluate and convert value inputs using compile-time constant rules.
8. Apply declaration-time defaults to missing optional value parameters.
9. Inject context, token-body, actual-position, and attached-target values.
10. If required binding failed, report all accumulated diagnostics and do not
    execute the macro.

Binding produces one immutable input snapshot. Expansion, hover, signature
help, and completion consume the same normalized descriptors so tooling cannot
disagree with execution.

### Declaration constraints

* An attached macro has exactly one `on` parameter.
* Its type is an attachable syntax type, a union of those types, or
  `SyntaxNode`.
* A freestanding macro has no `on` parameter.
* At most one parameter supplies each compiler context/body role unless a
  future API explicitly defines otherwise.
* No context role is required merely because a declaration is a macro.
* Compiler-supplied parameters cannot have defaults.
* Syntax-input defaults remain unsupported until their provenance semantics
  are defined.
* Value defaults are declaration-time constants convertible to their parameter
  type.
* Generic substitution happens before role classification and conversion.

For an attached invocation, attribute arguments bind only to user-supplied
parameters:

```raven
#[Serializable(.Compact)]
class Customer { }
```

`.Compact` binds to `mode`; the compiler injects the
`ClassDeclarationSyntax` into `target`.

## Actual invocation position

Every freestanding context exposes the compiler-determined position:

```raven
context.Position
```

Conceptually:

```raven
union MacroInvocationPosition {
    case Expression
    case Statement
    case NamespaceMember
    case TypeMember
    case Type
    case Pattern
}
```

This describes grammar, not target typing. It exists before execution and is
stable for expansion caching.

## Parsing and carriers

The parser recognizes compiler-owned invocation carriers without resolving a
macro definition. Each retains the same macro name, arguments, token body, and
source spans. Custom DSL nodes remain derived macro-owned structure rather
than Raven grammar nodes.

When `Name! { ... }` occupies an entire statement, the proposed deterministic
rule is a statement carrier. Parentheses force expression placement:

```raven
Name! { ... }       // statement position
(Name! { ... })     // expression position
```

Parsing must not load plugins. Resolution later diagnoses a declaration that
does not permit the carrier's position.

## Expansion validation

The driver follows one category-safe path:

1. Determine the carrier's actual position.
2. Resolve a macro whose declared positions include it.
3. Create a context containing that position.
4. Execute and collect diagnostics and contributions.
5. Validate the returned node or node list against the carrier.
6. Diagnose a mismatch; never cast and throw.
7. Register provenance and continue ordinary binding and lowering.

A union-typed multi-position macro remains category-typed at the source level
even if the normalized ABI transports its result as `SyntaxNode`. A declaration
written directly as `-> SyntaxNode` is category-untyped by design. Its supported
set remains inspectable as “all single-node freestanding positions,” and every
result is validated against the actual carrier.

## Normalized compiler model

The compiler model should represent the independent dimensions above directly.
The following names are proposed API shapes rather than a compatibility promise,
but the separation and invariants are design requirements.

### Application kind

`MacroKind` must stop encoding both application and output grammar. Replace its
current `AttachedDeclaration` and `Freestanding` cases with the
application-only distinction:

```csharp
public enum MacroApplicationKind
{
    Freestanding,
    Attached,
}
```

A freestanding macro's grammar positions are separate metadata. They are projected
from the declared return type and represented internally as flags so lookup does
not repeatedly inspect type syntax:

```csharp
[Flags]
public enum MacroInvocationTargets
{
    None = 0,
    Expression = 1 << 0,
    Statement = 1 << 1,
    NamespaceMember = 1 << 2,
    TypeMember = 1 << 3,
    Type = 1 << 4,
    Pattern = 1 << 5,
    AllSingleNode = Expression | Statement | NamespaceMember |
        TypeMember | Type | Pattern,
}
```

`AllSingleNode` is an alias for the currently supported flags, not an unrelated
seventh target. Adding a new single-node carrier deliberately updates the alias
and the validation table. List-valued outputs use a separate result cardinality
contract and are never smuggled through this flag set.

### Attached target

The public symbol model must not expose a second hand-maintained `MacroTarget`
classification as the source of truth. An attached declaration instead exposes
its compiler-supplied parameter and bound type:

```csharp
IParameterSymbol? AttachmentTargetParameter { get; }
ITypeSymbol? AttachmentTargetType { get; }
```

The type can be a concrete syntax type, a union of attachable syntax types, or
`SyntaxNode`. The compiler may derive a private bit set for registry indexing,
but it must be produced from this type by one shared projection routine. Binding,
completion, hover, diagnostics, and execution must consume that same projection.

### Parameter bindings

Parameter roles describe who supplies a value. The parameter type describes
what kind of value it is. This avoids adding one role for every syntax category
or context class:

```csharp
public enum MacroParameterSource
{
    Value,
    SyntaxInput,
    SequenceInput,
    Context,
    TokenBody,
    AttachedTarget,
}
```

For example, `ExpressionSyntax` and `TypeSyntax` parameters both have the
`SyntaxInput` role; their bound types retain the category distinction. Likewise,
recognized context types use the `Context` role rather than creating a new role
for each context implementation. `MacroList<T>` has the `SequenceInput` role; its
element type applies the same value-versus-syntax distinction to each item.

Every `Expand` parameter has one immutable binding:

```csharp
public sealed class MacroParameterBinding
{
    public IParameterSymbol Parameter { get; }
    public MacroParameterSource Source { get; }
    public int DeclarationOrdinal { get; }
    public int? InvocationArgumentOrdinal { get; }
}
```

`InvocationArgumentOrdinal` exists only for caller-supplied value and syntax
inputs in `(...)`. List-input and compiler-supplied roles retain declaration
order for diagnostics and display, but do not create holes in positional
argument binding. Consequently,
`AcceptsArguments` becomes a derived fact—whether any binding accepts a user
argument—not a capability separately declared by a provider interface.

Explicit syntax wins over type recognition. `on` always produces
`AttachedTarget`; a recognized compiler context or token-body type produces its
respective role; `MacroList<T>` produces `SequenceInput`; a syntax-node type
produces `SyntaxInput`; every other type, including `List<T>`, is `Value`.
Invalid combinations receive declaration diagnostics and are not registered as
executable macros.

### Macro symbols

The common symbol API should expose normalized facts regardless of whether a
macro was authored with Raven syntax or a plugin class:

```csharp
public interface IMacroSymbol : ISymbol
{
    INamedTypeSymbol DefinitionType { get; }
    IMethodSymbol ExpandMethod { get; }
    MacroApplicationKind ApplicationKind { get; }
    MacroInvocationTargets InvocationTargets { get; }
    IParameterSymbol? AttachmentTargetParameter { get; }
    ITypeSymbol? AttachmentTargetType { get; }
    ImmutableArray<MacroParameterBinding> ParameterBindings { get; }
}
```

For an attached macro, `InvocationTargets` is `None` and the attachment
properties are present. For a freestanding macro, the inverse holds. Return type,
parameters, generic parameters, and constraints project from `DefinitionType`
and `ExpandMethod`; the macro symbol does not own copies. These are validated
states rather than combinations consumers must guess how to interpret.
Raven-authored and class-authored macros project into this same symbol shape
before registration.

### Execution inputs and context

Argument binding produces one immutable `MacroInvocationInput` containing the
canonical parameter-binding/value pairs plus the authored carrier and actual
grammar position. The compiler then injects requested context, token body, and
attached target values. Execution never rebinds invocation arguments
independently.

The compiler may always maintain private execution state, but a macro context
object is created and exposed only when a context parameter asks for it. Its
semantic services should initialize lazily. This preserves the minimal
authoring experience without forcing the driver to maintain separate execution
pipelines for macros with and without an explicit context.

### Expansion and contribution results

The expression-specific `FreestandingMacroExpansionResult.Expression` is not
the normalized result boundary. The MVP freestanding expansion carries one
category-erased node:

```csharp
public sealed class FreestandingMacroExpansionResult
{
    public SyntaxNode? Node { get; }
    public ExpressionSyntax? Expression { get; }
    public StatementSyntax? Statement { get; }
    public ImmutableArray<MemberDeclarationSyntax> Members { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }
    // Provenance, dependencies, fragments, and token metadata are retained.
}
```

`Expression` and `Statement` are typed projections over `Node`, while
`FromExpression`, `FromStatement`, and `FromNode` preserve convenient creation.
The single-node `Node` and list-valued `Members` payloads are mutually
exclusive. The driver validates the node category or every member against the
actual carrier and reports diagnostics instead of casting or throwing. Empty
member output is represented by an explicitly selected member-list result, not
by guessing from the absence of `Node`; the selected output cardinality remains
available even when the list has no elements.

Attached execution produces a contribution result containing replacements,
introduced members or peers, diagnostics, provenance, and editor metadata. It
does not fake those contributions as a freestanding syntax return. `expand` is
therefore terminal only for freestanding macros; `replace` and `introduce`
accumulate attached contributions until body completion.

### Registry and lookup

The registry indexes a normalized descriptor by canonical name, application
kind, and projected target. Aliases point to that descriptor rather than
creating divergent copies. Lookup follows this order:

1. identify the compiler-owned carrier and actual target;
2. find visible macros with the requested canonical name or alias;
3. filter by application kind and projected target;
4. bind only caller-supplied parameters;
5. diagnose no match or ambiguity without executing a provider; and
6. execute, validate, and retain the result for the compilation snapshot.

Completion, signature help, hover, definition, and diagnostics query the same
descriptor set. Language-server code must not reconstruct macro applicability
from syntax or provider runtime types.

### Lowering Raven-authored declarations

`macro` declarations project to the nominal definition type and designated
`Expand` method specified by the [Macro ABI](abi.md). Compiler-supplied context,
body, and attached-target values remain parameters of that canonical method but
do not occupy invocation argument positions.

The current implementation lowers through private category-specific adapters
and generated parameter objects. Those shapes are transitional and may be
removed. The target execution boundary is the ABI's immutable invocation
snapshot and erased executor; source symbols, plugin symbols, and language
services depend on the canonical definition and method rather than generated
adapter layout.

### Invalid states and recovery

Declaration binding accumulates diagnostics and produces a non-executable
descriptor when possible. It must not throw for incomplete types, missing
parameters, duplicate compiler roles, or contradictory application metadata.
In particular:

* an attached macro has exactly one `AttachedTarget` parameter and no freestanding
  return target;
* a freestanding macro has no `AttachedTarget` parameter and at least one projected
  invocation target;
* at most one parameter supplies each compiler-owned context or body role;
* unsupported syntax categories and open-ended unions are diagnosed;
* unresolved types remain error types in the symbol snapshot; and
* malformed invocations retain a carrier and diagnostics so editor queries can
  continue against a consistent compilation snapshot.

This normalized invalid state is important for the language server: hover,
completion, semantic tokens, and diagnostics must observe the same partial
symbol rather than triggering different recovery paths.

## Class-authored APIs

An ordinary Raven class implementing `IMacroDefinition` is a first-class
authoring form. The interface marks the nominal definition but does not decide
the `Expand` signature. Discovery and registration project the class generic
parameters and its one designated method into the same canonical macro symbol
used for dedicated declarations. The compiler lowers that definition to the
erased executor from [Macro ABI](abi.md):

```csharp
public interface IMacroExecutor
{
    MacroExecutionResult Expand(MacroExecutionContext context);
}
```

The exported manifest associates the generated executor with the same
canonical nominal definition and `Expand` signature seen by Raven binding and tooling. The
execution context carries the constructed symbolic signature, bound arguments,
injected inputs, actual position, and lazy compiler services. The normalized
result retains diagnostics, dependencies, source maps, single-node results,
and list results. The driver validates all returned syntax against the
declaration metadata and actual carrier.

## Tooling

The declared position set belongs to macro symbol and registry metadata. It
drives completion visibility, signature help, hover, invalid-position
diagnostics, navigation, and expansion routing. The language server presents
compiler-owned facts and does not infer positions independently.

## Quotation is separate

`quote!` currently appears in expression position because its expansion is an
expression that constructs a syntax object. The syntax category inside its
body is not determined by that invocation position.

Quote-body category selection—explicit or safely target-contextual—must be
designed separately after the general application model is fixed. Macro
placement must not be distorted to solve quotation.

## Proposed decisions

1. A freestanding macro's return type declares its allowed invocation targets.
2. An omitted annotation defaults to `ExpressionSyntax`.
3. A union annotation is the canonical precise multi-position declaration.
4. `SyntaxNode` explicitly means all single-node freestanding positions and is
   the advanced wildcard, not a synonym for attached or list-valued expansion.
5. Actual position is compiler-owned context, not a macro argument.
6. Single-position APIs are typed; the advanced ABI carries `SyntaxNode` and
   is validated by the driver.
7. Whole-statement syntax selects a statement carrier; parentheses force an
   expression carrier.
8. Member-list output uses `SyntaxList<TMember>` in Raven source and an
   immutable member array in the normalized result.
9. Token bodies and editor services remain capabilities, not macro kinds.
10. Attached targets are compiler-supplied `on` parameters whose syntax type
    declares the attachment target.
11. Quote-body category selection remains independent.
12. Application kind and grammar target are separate compiler concepts.
13. Parameter roles describe value suppliers; parameter types describe syntax
    and context categories.
14. Attached applicability is derived from the typed `on` parameter, not a
    parallel public target enum.
15. All macro origins normalize to one symbol, descriptor, binding, registry,
    execution, and tooling model.
16. The stable declaration ABI is one nominal definition type with one
    designated `Expand` method; compiled execution uses the erased executor
    boundary described in [Macro ABI](abi.md).

## Implementation sequence

1. Introduce normalized application-kind, invocation-target, and parameter-role
   models and project existing expression and attached macros into them without
   changing accepted source.
2. Move registry lookup, symbols, argument binding, and language services to
   normalized descriptors; derive `AcceptsArguments` and attached target indexes.
3. Replace the legacy macro target clause with an `on` modifier on ordinary
   parameters, regenerate syntax APIs, and migrate compiler-owned macros and
   samples in the same compatibility-breaking slice.
4. Project return types into invocation targets and diagnose unsupported,
   contradictory, or unresolved category declarations.
5. Generalize the expansion result and driver validation while retaining typed
   expression factories and current expression behavior.
6. Add a statement carrier, position-aware resolution, expansion, diagnostics,
   malformed-input recovery, and editor tests.
7. Unify typed and multi-position class APIs behind the validated driver path.
8. Add member carriers using the documented `SyntaxList<TMember>` source
   contract and immutable normalized result ABI.
9. Add type and pattern carriers after declaration binding and incremental
   invalidation impact is covered.
10. Design quote-body categories on top of the stable application model.

Every slice includes malformed-input and incremental-language-server tests; an
incomplete invocation must remain a valid recoverable compiler state.
