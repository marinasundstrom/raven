# Macro application model

Status: **design proposal**

This proposal defines where macros can be applied, what a macro declaration
must communicate, and how the compiler validates expansion. It does not change
compiler behavior by itself.

The central rule is that application position, input representation, output
syntax, and optional capabilities are independent dimensions. Token bodies,
editor metadata, and custom DSL structure are not separate macro kinds.

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
| Output position | expression; statement; member; type; pattern |
| Cardinality | one node; a list in a list-valued grammar position |
| Contributions | expand; replace; introduce members; introduce peers |
| Capabilities | tokens; fragments; hover; completion; navigation |

The normalized contract records these separately. A macro does not become a
new kind merely because it uses a token body or supplies hover metadata.

## Freestanding positions

### Expression

An expression macro occupies an expression slot and produces exactly one
`ExpressionSyntax`:

```raven
macro Sql(context: TokenTreeMacroContext) -> ExpressionSyntax {
    expand LowerQuery(context)
}

let rows = Sql! { select * from users }
```

The proposed default for an omitted output annotation is `ExpressionSyntax`.

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

### Member

A member macro occupies a namespace-member or type-member list position:

```raven
macro Properties(context: TokenTreeMacroContext) -> MemberDeclarationSyntax* {
    expand BuildProperties(context)
}
```

Member positions naturally need zero-or-more output. The `T*` notation is
illustrative; the final list spelling remains a syntax decision.
`CompilationUnitSyntax` must not be used as an accidental list container.
Namespace-member and type-member positions may remain distinct because not
every declaration is legal in both.

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

An attached macro declares its target through `on`:

```raven
macro Component() on Type {
    replace ImplementComponent(target)
    introduce CreateSupportMembers(target)
}
```

Attached macros operate on an existing declaration. Their contributions are
replacement, introduced members, introduced peers, diagnostics, and editor
metadata. The target already constrains replacement syntax, so freestanding
output annotations do not replace `on`.

Potential targets include type, method/function, property, field, event,
constructor, accessor, and parameter. File, namespace, module, and assembly
targets require separate justification.

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

A multi-position macro is “untyped” only at the normalized ABI boundary. Its
allowed result set remains declared, closed, inspectable, and validated.

## Class-authored APIs

The simple API remains typed:

```csharp
public interface ISyntaxMacro<TSyntax> where TSyntax : SyntaxNode
{
    TSyntax Expand(SyntaxMacroContext context);
}
```

The advanced API supports several positions:

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

1. A freestanding output annotation declares allowed application positions.
2. An omitted annotation defaults to `ExpressionSyntax`.
3. A union annotation declares a multi-position macro.
4. Actual position is compiler-owned context, not a macro argument.
5. Single-position APIs are typed; the advanced ABI carries `SyntaxNode` and
   is validated by the driver.
6. Whole-statement syntax selects a statement carrier; parentheses force an
   expression carrier.
7. Member-list output receives a real list contract.
8. Token bodies and editor services remain capabilities, not macro kinds.
9. Attached targets remain declared with `on`.
10. Quote-body category selection remains independent.

## Implementation sequence

1. Add position and declared-position metadata without changing expression
   expansion.
2. Project macro return annotations into positions and diagnose unsupported
   syntax types.
3. Add a statement carrier, position-aware resolution, expansion, diagnostics,
   and editor tests.
4. Unify typed and multi-position class APIs behind one validated driver path.
5. Add member carriers after deciding list output syntax and ABI.
6. Add type and pattern carriers after declaration and binding impact is
   covered.
7. Migrate samples and compiler-owned macros where explicit annotations help.
8. Design quote-body categories on top of the stable application model.

Every slice includes malformed-input and incremental-language-server tests; an
incomplete invocation must remain a valid recoverable compiler state.
