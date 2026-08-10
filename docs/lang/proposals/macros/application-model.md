# Macro application model

Status: **MVP implemented; member-list extension in progress**

This proposal defines where macros can be applied, what a macro declaration
must communicate, and how the compiler validates expansion. It does not change
compiler behavior by itself.

The central rule is that application position, input representation, output
syntax, and optional capabilities are independent dimensions. Token bodies,
editor metadata, and custom DSL structure are not separate macro kinds.

For an invocable macro, the declared **return type decides its
invocation target**. It is not an ordinary runtime return-type annotation:
`ExpressionSyntax` makes the macro invocable in expression targets,
`StatementSyntax` makes it invocable in statement targets, and a union declares
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
| Application | invocable; attached to a declaration |
| Input | constants; syntax nodes; token body; compiler context |
| Return type / invocation target | expression; statement; member; type; pattern |
| Cardinality | one node; a list in a list-valued grammar position |
| Contributions | expand; replace; introduce members; introduce peers |
| Capabilities | tokens; fragments; hover; completion; navigation |

The normalized contract records these separately. A macro does not become a
new kind merely because it uses a token body or supplies hover metadata.

## Invocable positions

### Expression

An expression macro occupies an expression slot and produces exactly one
`ExpressionSyntax`:

```raven
macro Sql(context: TokenTreeMacroContext) -> ExpressionSyntax {
    expand LowerQuery(context)
}

let rows = Sql! { select * from users }
```

The proposed default for an omitted return type is `ExpressionSyntax`, making
the macro invocable only in expression targets.

### Statement

A statement macro occupies one statement slot and produces one
`StatementSyntax`. A block represents several runtime statements:

```raven
macro Trace(context: TokenTreeMacroContext) -> StatementSyntax {
    expand BuildTraceBlock(context)
}

func Run() {
    Trace! { operation() }
}
```

### Expression or statement

A multi-position macro declares its closed output set:

```raven
macro Evaluate(context: TokenTreeMacroContext)
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
every single-node invocable position known to the compiler:

```raven
macro Forward(context: TokenTreeMacroContext) -> SyntaxNode {
    expand BuildFor(context.Position, context)
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
| `SyntaxNode` | every supported single-node invocable position |

### Member

A member macro occupies a namespace-member or type-member list position:

```raven
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro Properties(context: TokenTreeMacroContext)
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

### Type

A type macro occupies a type slot and produces one `TypeSyntax`:

```raven
macro QueryRow(context: TokenTreeMacroContext) -> TypeSyntax
```

Type expansion affects signatures and incremental declaration binding. It is
part of the complete design but follows expression, statement, and member
support in implementation order.

### Pattern

A pattern macro occupies a pattern slot and produces one `PatternSyntax`:

```raven
macro MessageShape(context: TokenTreeMacroContext) -> PatternSyntax
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
Its type decides where the macro can be attached, just as an invocable macro's
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
metadata. They have no invocable return target. Combining an `on` parameter
with an invocable syntax return type is invalid.

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
| Context | a recognized macro context type | compiler |
| Token stream/body | a recognized token-body type | compiler |
| Attached target | `on target: TargetSyntax` | compiler |

Only value and syntax-input parameters participate in positional and named
argument mapping. Compiler-supplied parameters never consume an argument slot
and cannot be named by the caller.

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
macro Query(context: TokenTreeMacroContext) -> ExpressionSyntax {
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

The macro return annotation describes the **syntax category**, not the runtime
or semantic type of the expanded expression. After expansion, Raven binds the
ordinary returned expression in its invocation context. In the example, `x` is
inferred as the normal numeric type of `2 + 3`; hover and downstream type
checking use that bound type rather than `ExpressionSyntax`.

The MVP does not introduce `ExpressionSyntax<T>` or another parallel type system
for syntax objects. Contextual typing already validates the expansion where the
invocation appears:

```raven
let x: double = evaluate!(2 + 3)
```

Typed syntax wrappers remain a post-MVP design decision and belong to a special
macro-infrastructure layer. They are not syntax nodes, do not appear in ordinary
Raven syntax trees, and do not extend the generated syntax-node hierarchy. They
wrap existing immutable syntax nodes together with a compiler-verified semantic
constraint. Ordinary syntax-node parameters and returns remain supported as the
category-only, or “untyped,” forms.

Illustrative future shapes are:

```raven
ExpressionSyntax<T>        // Any expression whose resulting type is compatible with T.
LiteralExpressionSyntax<T> // A literal syntax node whose resulting type is compatible with T.

macro Double(expr: ExpressionSyntax<double>) -> ExpressionSyntax<double> {
    // ...
}
```

`ExpressionSyntax<T>` constrains only the semantic result while
`LiteralExpressionSyntax<T>` constrains both existing syntax shape and semantic
type. Each wrapper retains access to its underlying ordinary node. The compiler
would bind and verify the input before execution, then unwrap, bind, and verify
the expansion after execution, mapping diagnostics through provenance. The
macro cannot assert or bypass either check.

The final wrapper API is deliberately undecided. Semantic promises must remain
separate from grammatical invocation targets, work without creating binding
cycles, and degrade to stable error-typed inputs and results during incomplete
editing.

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
* An invocable macro has no `on` parameter.
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

Every invocable context exposes the compiler-determined position:

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
set remains inspectable as “all single-node invocable positions,” and every
result is validated against the actual carrier.

## Normalized compiler model

The compiler model should represent the independent dimensions above directly.
The following names are proposed API shapes rather than a compatibility promise,
but the separation and invariants are design requirements.

### Application kind

`MacroKind` must stop encoding both application and output grammar. Replace its
current `AttachedDeclaration` and `Invocable` cases with the
application-only distinction:

```csharp
public enum MacroApplicationKind
{
    Invocable,
    Attached,
}
```

An invocable macro's grammar positions are separate metadata. They are projected
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

### Parameter descriptors

Parameter roles describe who supplies a value. The parameter type describes
what kind of value it is. This avoids adding one role for every syntax category
or context class:

```csharp
public enum MacroParameterRole
{
    None,
    Value,
    SyntaxInput,
    Context,
    TokenBody,
    AttachedTarget,
}
```

For example, `ExpressionSyntax` and `TypeSyntax` parameters both have the
`SyntaxInput` role; their bound types retain the category distinction. Likewise,
recognized context types use the `Context` role rather than creating a new role
for each context implementation.

Every macro is normalized to immutable parameter descriptors:

```csharp
public sealed class MacroParameterDescriptor
{
    public IParameterSymbol Parameter { get; }
    public MacroParameterRole Role { get; }
    public int DeclarationOrdinal { get; }
    public int? InvocationArgumentOrdinal { get; }
    public bool HasDefaultValue { get; }
    public object? DefaultValue { get; }
}
```

`InvocationArgumentOrdinal` exists only for caller-supplied value and syntax
inputs. Compiler-supplied roles retain declaration order for diagnostics and
display, but do not create holes in positional argument binding. Consequently,
`AcceptsArguments` becomes a derived fact—whether any descriptor accepts a user
argument—not a capability separately declared by a provider interface.

Explicit syntax wins over type recognition. `on` always produces
`AttachedTarget`; a recognized compiler context or token-body type produces its
respective role; a syntax-node type produces `SyntaxInput`; every other type is
`Value`. Invalid combinations receive declaration diagnostics and are not
registered as executable macros.

### Macro symbols

The common symbol API should expose normalized facts regardless of whether a
macro was authored with Raven syntax or a plugin class:

```csharp
public interface IMacroSymbol : ISymbol
{
    MacroApplicationKind ApplicationKind { get; }
    MacroInvocationTargets InvocationTargets { get; }
    ITypeSymbol? ReturnType { get; }
    IParameterSymbol? AttachmentTargetParameter { get; }
    ITypeSymbol? AttachmentTargetType { get; }
    ImmutableArray<MacroParameterDescriptor> Parameters { get; }
}
```

For an attached macro, `InvocationTargets` is `None`, `ReturnType` is absent,
and the attachment properties are present. For an invocable macro, the inverse
holds. These are validated states rather than combinations consumers must guess
how to interpret. Raven-authored and class-authored macros project into this
same symbol shape before registration.

### Execution inputs and context

Argument binding produces one immutable `MacroInvocationInput` containing the
normalized descriptor/value pairs plus the authored carrier and actual grammar
position. The compiler then injects requested context, token body, and attached
target values. Execution never rebinds invocation arguments independently.

The compiler may always maintain private execution state, but a macro context
object is created and exposed only when a context parameter asks for it. Its
semantic services should initialize lazily. This preserves the minimal
authoring experience without forcing the driver to maintain separate execution
pipelines for macros with and without an explicit context.

### Expansion and contribution results

The expression-specific `InvocableMacroExpansionResult.Expression` is not
the normalized result boundary. The MVP invocable expansion carries one
category-erased node:

```csharp
public sealed class InvocableMacroExpansionResult
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
does not fake those contributions as an invocable syntax return. `expand` is
therefore terminal only for invocable macros; `replace` and `introduce`
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

`macro` declarations lower to private adapters after semantic normalization.
The generated parameter object contains only caller-supplied parameters. The
adapter receives compiler-supplied context, body, and attached-target values
through the normalized execution input and passes them to the declaration body
at their declared parameter positions.

This adapter is deliberately allowed to be more complex than the source form.
Its shape is not a public language contract, and the implementation may replace
the current category-specific interfaces. Source symbols, plugin symbols, and
language services must depend on normalized metadata rather than inspecting the
generated adapter.

### Invalid states and recovery

Declaration binding accumulates diagnostics and produces a non-executable
descriptor when possible. It must not throw for incomplete types, missing
parameters, duplicate compiler roles, or contradictory application metadata.
In particular:

* an attached macro has exactly one `AttachedTarget` parameter and no invocable
  return target;
* an invocable macro has no `AttachedTarget` parameter and at least one projected
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

The simple API remains typed:

```csharp
public interface ISyntaxMacro<TSyntax> where TSyntax : SyntaxNode
{
    TSyntax Expand(SyntaxMacroContext context);
}
```

The advanced API supports a precise position set or the explicit wildcard:

```csharp
public interface ISyntaxMacro
{
    MacroExpansionPositions SupportedPositions { get; }
    SyntaxNode Expand(SyntaxMacroContext context);
}
```

The final result carrier also retains diagnostics, dependencies, source maps,
and list results. These interfaces illustrate typing, not final names.
Raven-authored declarations lower to the same normalized metadata and adapter
contract while retaining a much simpler source form.

For a union declaration, `SupportedPositions` contains exactly the projected
cases. For `-> SyntaxNode`, it contains `AllSingleNode`; it never silently
includes attached or list-valued operations.

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

1. An invocable macro's return type declares its allowed invocation targets.
2. An omitted annotation defaults to `ExpressionSyntax`.
3. A union annotation is the canonical precise multi-position declaration.
4. `SyntaxNode` explicitly means all single-node invocable positions and is
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
