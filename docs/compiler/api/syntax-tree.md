# Syntax Tree API

The syntax tree API exposes the immutable structure produced by the parser. It
follows the familiar Roslyn split between a compact "green" tree and a richer
"red" tree so callers can efficiently inspect and transform source code without
sacrificing reuse.

---

## Creating syntax trees

The entry point is `SyntaxTree.ParseText`, which accepts raw text or a
`SourceText` instance. Parsing returns a `SyntaxTree` whose root is a
`CompilationUnitSyntax` node:

```raven
import Raven.CodeAnalysis.Syntax.*

let tree = SyntaxTree.ParseText(source)
let root = tree.GetRoot()
```

`SyntaxTree.Create` performs the inverse: it wraps an existing red root (for
example, one produced by a rewriter) back into a tree so it can participate in a
`Compilation`.

Each tree retains a `SourceText` snapshot. Parsing keeps the supplied text;
`Create` reconstructs the text once from the red root. `GetText` and `TryGetText`
return that snapshot.

## Syntax annotations

Adding annotations to a syntax node preserves its diagnostics, children and
previously attached annotations. New annotations follow existing annotations in
insertion order; the original immutable node remains unchanged. An empty green
node annotation update returns the same node.

## Separated syntax lists

`SeparatedSyntaxList<TNode>.Count` counts nodes. `GetWithSeparators()` enumerates
all alternating node and separator slots; it may end with a trailing separator.
`SeparatorCount` includes that trailing separator, and `GetSeparator(index)`
accepts its index. Negative indices and indices beyond the stored separator slots
throw `IndexOutOfRangeException`, including index zero on an empty/default list.

`IsSoundSeparatedList(expectedSeparatorKind)` validates the complete slot
sequence: every even slot must contain a `TNode`, and every odd slot must contain
a token of the requested kind. Empty lists and trailing separators are valid
representations. This structural check does not imply that every language grammar
production permits a trailing separator.

## Navigating nodes and tokens

Red `SyntaxNode` and `SyntaxToken` instances expose parents, spans, and helpers
such as `DescendantNodes`/`DescendantTokens`. The `SyntaxTree` surface adds
convenience methods for locating nodes: `GetNodesInSpan` returns the innermost
nodes that cover a `TextSpan`, `GetNodeForSpan` gives the first match, and
`GetNodeToReplace` normalizes zero-width insertions to the containing node.
`GetLocation` turns a span into a `Location` tied to the tree for diagnostics.
Tree-level `GetDiagnostics` overloads aggregate parser diagnostics for the whole
file, a particular node/token, or a span.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L46-L139】

Macro declarations retain declaration-level service clauses as structured
syntax. `MacroDeclarationSyntax.CapabilityClauses` is an ordered
`SyntaxList<MacroCapabilityClauseSyntax>`. Each clause exposes its contextual
capability keyword, the `by` keyword, and the handler expression, so syntax
tools never need to recover `completion by CompleteDsl` or similar clauses from
raw text.

For inspection and tooling, syntax trees can be printed using the `PrintSyntaxTree`
extension showcased in the API README. That routine walks the red tree and emits
kind names, spans, and trivia so callers can visualize structure.

While trivia (comments, whitespace, and directives) is preserved on tokens for
round-tripping and tooling scenarios, semantic analysis only cares about the
well-formedness of the tree. As long as the parser produces a structurally
correct syntax tree, the binder can reason about declarations and expressions;
trivia influences parsing decisions but carries no meaning for the compiler
itself.

## Formatting and trivia

`SyntaxNode.FindTrivia(position)` checks both leading and trailing trivia. A
position within token text or outside the node returns the default trivia value.
Token trailing-trivia spans begin after both leading trivia and token text; their
positions use the same source coordinates as node and token spans.

`SyntaxFactory` creates raw structured nodes. Unless you explicitly attach
trivia yourself, factory-built nodes generally have minimal whitespace and are
not intended to be human-formatted out of the box.

Use `NormalizeWhitespace()` when you want a fully normalized presentation of a
node or subtree:

```raven
let formatted = rawNode.NormalizeWhitespace()
```

For targeted formatting, Raven also exposes Roslyn-style formatting markers:

- `Formatter.Annotation` marks nodes or tokens that should be formatted.
- `SyntaxAnnotation.ElasticAnnotation` marks trivia as formatter-managed.
- `SyntaxFactory.ElasticSpace`, `SyntaxFactory.ElasticLineFeed`, and related
  helpers create elastic trivia directly.

`Formatter.Format(root)` formats annotated regions and rewrites elastic trivia
into normal spaces/newlines while preserving unannotated concrete trivia
outside those regions.

## SyntaxFactory overloads

Public red `SyntaxFactory` overloads are intended to be valid-by-construction.
For nodes that opt into explicit factory definitions, the generated public API
only exposes the overloads declared in `Syntax/Factories.xml` instead of a raw
full-slot helper.

`Factories.xml` can also attach documentation metadata to an overload:

- `Summary="..."` adds XML documentation to the canonical factory overload.
- `Alias="..."` adds a second descriptive factory name for the same overload.

Aliases are only convenience names. They do not define a different syntax kind
or shape, and the generated XML docs call out that the canonical factory name is
still preferred unless the alias is clearer at the call site.

## Closed syntax hierarchies

The public syntax-node hierarchy is published as a set of closed families.
This includes the `SyntaxNode` root, structured trivia, `StatementSyntax`,
`ExpressionSyntax`, `PatternSyntax`, and intermediate categories such as types,
names, members, methods, collection elements, and variable designations. Raven
reconstructs their permitted direct subtypes from `ClosedHierarchyAttribute`
metadata, so a `match` over the syntax API participates in ordinary
exhaustiveness analysis:

```raven
func Describe(statement: StatementSyntax) -> string {
    return match statement {
        ReturnStatementSyntax => "return"
        ExpressionStatementSyntax => "expression"
        _ => "another statement"
    }
}
```

Until the platform supplies this well-known contract, each producing assembly
may define its own `System.Runtime.CompilerServices.ClosedHierarchyAttribute`.
Raven recognizes the metadata name and payload, so consumers do not require
attribute type identity with `Raven.CodeAnalysis`.

Remove the discard arm and Raven reports the unhandled syntax cases. Tooling can
use the same missing-case information to offer the cases that may be added.
Visitors remain preferable when a consumer intentionally wants new syntax kinds
to follow default traversal behavior.

The node generator derives each permitted subtype list from `Syntax/Model.xml`.
Top-level `Hierarchy` entries describe public syntax families declared outside
ordinary generated node entries, including the root and structured trivia.
Recovery-only nodes are included explicitly; for example,
`ExpressionSyntax.Missing` is a real expression shape that exhaustive consumers
must handle or route through a discard arm.

This contract intentionally makes exhaustive consumers sensitive to compiler API
evolution. Adding a syntax node extends the generated closed hierarchy, so code
compiled against the newer API must handle the new case or retain a catch-all.
That feedback is useful for compiler and macro infrastructure that must make a
deliberate decision about every syntax shape.

## Visitors and rewriters

The compiler ships source-generated visitor bases that align with Roslyn's
patterns. `SyntaxVisitor` and `SyntaxVisitor<T>` offer traversal hooks for every
node type, while `SyntaxRewriter` enables immutable transformations by producing
updated nodes. The generator maintains these APIs so consumers can override only
the members they care about and rely on default traversal for the rest.【F:docs/compiler/architecture/visitors-and-rewriters.md†L1-L40】

The base nullable-token visitor preserves an absent token and dispatches each
present value, including a present default token, through `VisitToken(SyntaxToken)`.
The base trivia visitor preserves its input. Derived rewriters can override these
hooks deliberately; ordinary list traversal does not erase whitespace or comments
merely because the base trivia implementation ran. These syntax contracts are
shared before target-specific lowering.

## Incremental updates

`WithChangedText` rebuilds a tree from edited source and shares unchanged green
nodes where possible. The API exposes text changes via `GetChanges`, allowing
callers to diff old and new trees or integrate with workspace features.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L68-L93】【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L141-L178】

When used with `Compilation.With...` helpers, new syntax trees can be swapped in
without mutating existing compilations. This workflow underpins the compiler's
incremental model and keeps tree operations predictable for IDE scenarios.

### Green runtime null-input contracts

Internal green-node construction accepts a null slot array as an empty node.
Token construction accepts omitted/null trivia lists, and the enumerable
`WithLeadingTrivia` / `WithTrailingTrivia` helpers treat null as empty trivia.
These inputs carry nullable annotations in the C# bootstrap and Raven translation.
A green list's item array remains required: null still throws
`ArgumentNullException` with parameter name `items`. The explicit null test in
its base initializer preserves that guard while avoiding conditional access on a
statically non-null input in Raven.

The red token's internal green-node reference is nullable because `default(SyntaxToken)`
has no green node. Empty/default token properties and update helpers retain their
existing behavior. Green trivia-list construction accepts a null array as empty,
and its checked `GetSlot` override returns a non-null trivia or throws for an
out-of-range index. These annotations are shared by the C# and Raven sources.
Trivia-list construction computes the width once for both width properties.

Green syntax lists preserve nullable child slots, including through `With`. Their
constructor still requires the array itself. Empty slots contribute no width and
are skipped by child traversal; indexed slot access can return null. Storage,
constructor-element, and indexer annotations reflect this in both compiler sources.

Structured directive trivia and skipped-token trivia require their first child
slot. Their typed accessors and `With` reconstruction paths explicitly assert
that invariant before casting the nullable general-slot result, in both sources.

Green-list span calculation finds the first and last non-empty slot before
trimming trivia. Lists with only empty slots have empty spans at their supplied
position, matching their zero width.

Typed node, token and separated lists require populated element slots; their
accessors explicitly assert that invariant after indexing the general nullable
green list. Green rewriter copy buffers permit null entries so existing empty
slots can be retained when copying a prefix during a rewrite.

### Missing-expression visitor dispatch

A recovery expression's green node dispatches to `DefaultVisit` exactly once,
matching its red counterpart. Both generic and non-generic visitors can inspect
that leaf without redispatching through `Visit`; a generic default result is
returned unchanged. Recovery-node child buffers retain the nullable green-slot
contract, and an unattached red recovery node accepts a nullable parent.

## Bootstrap source organization

`SyntaxTree.cs` owns text/root storage, construction from an existing root,
diagnostic attachment, locations and navigation. `SyntaxTree.Parsing.cs` owns
parsing entry points, incremental replacement and fallback policy. Both are parts
of the same C# type; existing APIs and incremental behavior are preserved.

The Raven translation follows the same boundary. Syntax nodes, tree references,
source locations and parser entry points belong to the Raven syntax assembly.
Semantic symbol contracts still come from the C# bootstrap; this syntax milestone
does not provide the complete Raven-hosted compiler. Validation evidence and
remaining compiler work are recorded in the port progress log.


A final conditional reparsed as a block value retains abrupt transfers in its
branches. In particular, explicit returns inside a statement-form match arm
remain returns from the enclosing function when the final conditional becomes
an expression node.


`ArrowTypeClauseSyntax` stores children in source order: `ArrowToken`,
`AttributeLists`, then `Type`. Full factories, constructors and `Update` follow
that order in both generated C# and Raven APIs. A clause such as `-> [A] string`
round-trips without moving its attributes before the arrow; attribute and type
positions follow the same order.


Attribute-list recovery preserves available source and synthesizes a missing
closing bracket while reporting the expected `]` diagnostic.


For an attached macro prefix such as `#[A]`, `AttributeListSyntax.HashToken`
owns the optional `#` before `OpenBracketToken`. `AttributeSyntax` contains the
name and optional argument list; it no longer owns a hash slot. Generated full
list factories/constructors/Update methods take the hash token first, while
ordinary-list convenience overloads supply an absent token. `IsMacroAttribute`
retains the existing convention of identifying the first attribute in a prefixed
list. Completion and preview selection include the list prefix in the invocation
head; token spans and round-tripped source follow physical source order.


Macro-declaration lookahead recognizes `extern` alongside the other declaration
modifiers, including after `async`. Compilation-unit and namespace parsers retain
those modifiers on `MacroDeclarationSyntax`; an extern declaration terminated by
`;` has no synthesized block or expression body. These are parser-shape guarantees,
not a claim that every modifier combination is semantically valid.


Missing tokens retain `IsMissing` when copied with leading/trailing trivia, raw
kind, diagnostics or annotations. Missing state is independent of width: a
present end-of-file token remains present even though its width is zero. Enum
member recovery retains an unexpected token as skipped trivia on a missing
identifier, reports the unexpected token, and advances instead of retrying the
same input indefinitely.


Misplaced import and alias directives retain their original source order as
skipped tokens inside an incomplete member. They continue to report ordering
diagnostics and are excluded from the valid `Imports` / `Aliases` lists. This
recovery applies to compilation units, braced namespaces and file-scoped
namespaces, preserving `ToFullString()` and positions even for invalid input.


Global import block recovery retains unexpected tokens as skipped trivia on the
following import keyword or closing brace, including a missing closing brace.
The parser reports unexpected-token diagnostics instead of silently dropping
source text. The internal single-member entry point returns null for standalone
imports and aliases; recovery of whole compilation units does not turn those
fragments into accepted member declarations.

Extension declaration recovery retains unexpected member tokens in incomplete
members and unexpected accessor tokens as skipped trivia on the missing accessor
keyword. Both the C# bootstrap and Raven implementation preserve the original
source through `ToFullString()` while reporting diagnostics. This parser contract
is shared across targets; it does not depend on runtime encoding.


Pattern operand parsing leaves outer-level `and`, `or` and `|` tokens for the
pattern parser. Comparison operands, range bounds and identifier constants use
this boundary; nested parenthesized expressions retain ordinary operators.
The resulting tree preserves `BinaryPatternSyntax` conjunction/alternative
nodes instead of consuming their operators into an operand expression. Both
C# and Raven pattern parser implementations use the same expression-parser
contract.


Required pattern delimiters produce `CharacterExpected` diagnostics when missing,
while retaining zero-width missing tokens and the available source. This applies
to positional, sequence, dictionary, member/nominal argument lists and nested
variable designations. Speculative property-clause parsing continues to rewind
its temporary diagnostics if the clause is rejected.


Explicit-interface indexers retain the qualified `self` member in their
`ExplicitInterfaceSpecifier`, followed by the bracketed parameter list. The
reserved `self` token is admitted in this member-name position without allowing
it as an ordinary type identifier. Accessor-list recovery in C# and Raven retains
unexpected tokens as skipped trivia on the missing accessor keyword, preserving
source text and diagnostics.

String interpolation scanning now belongs to the internal static
`StringInterpolationParser` helper. The C# expression parser calls this helper;
the Raven translation preserves its scanning, token/trivia, missing-token and
escape-decoding behavior. Embedded expressions use the translated `LanguageParser`; component parity
tests now execute the source-owned grammar cycle. The reduced component still
uses bootstrap lexer, contexts and green/red syntax runtime.

Expression parsing in statement headers stops speculative parenthesized casts
at a line break after the closing parenthesis. This preserves the boundary
between a condition and its non-block body without changing same-line casts.

Tuple, collection and array-expression recovery reports `CharacterExpected`
when a required closing delimiter is absent. The tree retains the corresponding
missing token and all available source text. The C# and Raven expression
parsers use the same recovery behavior.

The internal `LanguageParser` dispatches specialized `NameSyntax` and
`IdentifierNameSyntax` requests before the general expression family.
A qualified name requested as `NameSyntax` therefore remains a
`QualifiedNameSyntax`, rather than becoming a member-access expression.
`ParseSyntaxWithDiagnostics` returns its diagnostics and consumed position in
the immutable `ParseResult` record struct; concurrent calls create independent
lexers and parser contexts.

Internal node-only `ParseSyntax` results retain context diagnostics on their
returned green root. `ParseStatement` uses the same result-producing path and
honors `consumeFullText`: trailing input is diagnosed when requested, while
fragment parsing remains available when it is false. The result-producing API
keeps diagnostics separately in `ParseResult`; callers must not merge the same
list twice. Public detached red-node diagnostics retain source context as
described below.

The Raven port now implements the public `SyntaxFactory.ParseExpression`,
`ParseStatement`, `ParseType`, `ParsePattern`, `ParseCompilationUnit` and
`ParseMemberDeclaration` overloads in `SyntaxFactory.Parsing.rvn`. String overloads
forward through `SourceText`; fragment overloads retain their input offset.
Member parsing accepts exactly one non-global declaration with no imports,
aliases or compilation-unit attributes, matching the bootstrap implementation.
The shared C# parity project exercises these entry points against the selected
implementation and asserts syntax-runtime assembly ownership. Component harness
results and full assembly results are distinguished in the port progress log.

Syntax-tree printing counts the root at depth zero. `PrinterOptions.MaxDepth`
limits descendant levels; both writer output and string representations use
that same starting depth. Starting at the configured maximum incorrectly hid
all children for finite limits and overflowed the default unlimited counter.

Trivia and structured-trivia traversal observe the same maximum depth. The Raven
port supplies PrinterOptions and PrettySyntaxTreePrinter, including extension
entry points, list grouping, spans/locations, colors, annotations and diagnostics.
The permanent parity suite uses the selected implementation's syntax nodes;
reduced printer component checks also remain useful during stabilization.

`SyntaxFactory.ParseCompilationUnit` now returns the root of `SyntaxTree.ParseText`
for the supplied source and options. Its root and descendants retain their source
owner, and syntax diagnostics remain available with locations in that same tree.
The string overload uses the same path. `ParseMemberDeclaration` consequently
retains the containing tree for an accepted single member. Fragment parsing (expression, statement, type and pattern) retains source context
with its diagnostics, as described below, while leaving the returned nodes detached.

Diagnostic span filtering includes zero-width diagnostic markers at either
boundary of the requested span, including missing-token diagnostics at EOF.
Nonempty diagnostic spans retain ordinary overlap filtering. This is a diagnostic
query rule; general TextSpan.IntersectsWith semantics are unchanged.

Detached fragment diagnostics retain immutable source context in DiagnosticInfo.
Node-only parser entry points attach context-bearing diagnostic copies to the green
root. GetDiagnostics converts these into external-file locations using the original
source coordinates, including a nonzero parse offset; SourceTree remains null.
Nonrecursive node queries return diagnostics directly attached to that green node.
Diagnostics without source context retain Location.None. Full-tree queries continue
using their owning SyntaxTree, and incremental reparsing continues to return its
separate ParseResult diagnostic list.

Missing closing braces in type declarations now produce CharacterExpected while
retaining the missing token and original source text.


The Raven syntax runtime also includes `RavenQuoter`, `RavenQuoterOptions` and
`RavenQuoterOutputLanguage`. Quotation preserves factory selection, trivia,
source overrides, formatting options and C#/Raven output choices. Raven wrappers
place opening braces on the declaration line. The reflection-based factory cache
remains private; semantic symbol contracts still come from the C# bootstrap.

## Pattern and logical-expression boundaries

After `is`, pattern combinators remain inside `IsPatternExpressionSyntax`.
Enclosing `&&` and `||` form `InfixOperatorExpressionSyntax` nodes whose left
operand is the completed pattern test. Constant-pattern lookahead accepts these
terminators; expression parsing within comparison/range patterns leaves logical
operators for the enclosing parser. Parenthesized operands retain their ordinary
expression behavior. The C# and Raven parser implementations share this contract.
