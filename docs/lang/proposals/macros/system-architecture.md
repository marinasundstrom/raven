# Complete macro system architecture

> **Status:** proposal. This document describes the intended end state and is
> not a compatibility contract. Raven is experimental; existing macro APIs,
> interfaces, syntax, and implementation details may be replaced when the
> resulting model is simpler or fits the compiler better.

This proposal consolidates the intended Raven macro system: invocation,
expansion, syntax construction, source provenance, hygiene, diagnostics,
semantic access, editor services, packaging, isolation, caching, and future
Playground support. The current implementation is tracked separately in the
[macro implementation plan](implementation-plan.md), and detailed editor
behavior is discussed in
[Macro and DSL developer experience](developer-experience.md).

## Summary

Raven macros should be explicit, compiler-owned transformations from a bounded
source region to ordinary Raven syntax. They should support both conventional
host-language macros and token-oriented DSLs without forcing those DSLs to
pretend to be Raven syntax.

The design has three cooperating projections of a macro invocation:

1. **The authored carrier** is ordinary Raven syntax that preserves the macro
   name, arguments, delimiters, and lossless body.
2. **The input snapshot** exposes provider-classified tokens and spans for
   embedded Raven fragments. It is sufficient for diagnostics and editor
   routing but does not expose the macro's private parse tree.
3. **The expansion** is ordinary immutable Raven syntax, annotated with
   provenance that maps generated elements back to authored DSL tokens,
   fragments, or the invocation as a whole.

Macro authors receive a syntax toolbox rather than a single prescribed
construction technique:

* parse source-backed Raven fragments by category;
* quote Raven syntax and splice existing syntax into it;
* construct or transform nodes with `SyntaxFactory`, visitors, and rewriters;
* create hygienic names and explicitly select call-site or definition-site
  lookup when needed; and
* attach source origins and report diagnostics through compiler-owned APIs.

This combines useful ideas from Nim, Rust, Swift, Scala, Elixir, and Roslyn,
but the combination is specific to Raven's architecture. In particular, a
macro-owned DSL representation is not inserted into Raven's syntax or bound
trees, and the language server never becomes a second macro host.

## Motivation

The HTML/Blazor prototype demonstrates the immediate use case:

```raven
#[Component]
class Greeting(name: string) {
    func Render() -> RenderFragment =>
        Html! {
            <h1>Hello {name}</h1>
        }
}
```

The component and parameter macros remove Blazor boilerplate. `Html!` owns an
HTML-shaped region, delegates `{name}` to Raven expression parsing, and lowers
the result to normal Blazor calls. A component tag such as
`<Greeting name="Raven" />` becomes an ordinary Blazor component operation.
None of those conveniences should create a parallel UI runtime or restrict
access to the underlying Blazor model.

The same infrastructure should also cover host-shaped macros, query DSLs,
regular expressions, routes, serialization descriptions, and future library
experiments. Building a separate compiler feature for each embedded language
would not scale. Treating every body as a string would lose source identity,
diagnostics, completion, and safe construction.

## Goals

The complete system should provide:

* explicit attached and invocable application forms;
* lossless raw bodies for arbitrary token DSLs;
* typed value, syntax, token-body, and symbolic-type inputs;
* expression, statement, declaration, member, type, and pattern outputs;
* immutable ordinary Raven syntax as the expansion boundary;
* parsing, quotation, splicing, factories, visitors, and rewriters;
* predictable hygiene with explicit escape hatches;
* authored source locations for parser, macro, binder, and emit diagnostics;
* optional semantic inspection without giving plugins mutable compiler state;
* token classification and embedded-fragment spans for editor services;
* completion in incomplete DSL bodies and zero-width recovery positions;
* deterministic, cancellable, cacheable execution with tracked inputs;
* compiler-plugin packaging that works without a workspace;
* expansion inspection and a path to Playground visualization; and
* a deliberately small path for macros that need only arguments and a quoted
  result.

## Non-goals

The design does not:

* turn macros into textual preprocessing;
* make arbitrary macro parser nodes part of Raven's public syntax hierarchy;
* require every macro to implement editor capabilities;
* give the language server authority to load or execute plugins independently;
* provide a new runtime framework when expansion can target an existing .NET
  framework such as Blazor;
* guarantee that every compiler API is callable during expansion;
* allow untracked ambient file, network, environment, clock, or process input;
* promise compatibility with the current experimental macro API; or
* require Playground integration before a macro library is independently
  distributable.

## Architectural invariants

1. **The authored tree is the diagnostic source of truth.** Generated trees
   may carry origins, but tools report locations in authored source whenever a
   meaningful origin exists.
2. **The file parses without loading a macro.** The Raven parser recognizes
   invocation carriers and balances raw bodies; it does not need the DSL
   grammar.
3. **Expansion is compiler-owned.** `Compilation` and `SemanticModel` resolve,
   execute, validate, and cache macros. A workspace is optional.
4. **Expansion produces ordinary Raven syntax.** Binding, operations, lowering,
   and emit do not need macro-specific variants of every generated construct.
5. **Private DSL structure stays private.** The shared tooling boundary is a
   lossless token-and-span snapshot, not an extensible Raven AST hierarchy.
6. **One snapshot means one answer.** Expansion, diagnostics, completion, and
   semantic tokens are computed against a stable compilation/document version.
7. **Failure is data.** Expected errors are diagnostics; plugin exceptions are
   isolated macro failures and do not crash the compiler or editor.
8. **Capabilities are optional and composable.** A simple macro need not become
   a lexer, parser, analyzer, or language-server plugin.
9. **Inputs are explicit and cacheable.** External resources must be requested
   through tracked compiler services.
10. **The compiler validates the result.** A macro cannot bypass Raven parsing,
    binding, type checking, or normal code generation rules.

## System model

```text
authored Raven source
        |
        v
compiler-known invocation carrier
        |
        +------> resolve descriptor and bind typed arguments
        |
        v
optional input analysis (tokens, fragment spans, diagnostics)
        |                                      |
        |                                      +--> compiler-owned editor snapshot
        v
expand with parse / quote / factory / semantic services
        |
        v
validate category + attach source provenance
        |
        v
ordinary Raven syntax -> binding -> operations -> lowering -> emit
```

Input analysis and expansion are distinct compiler operations, but only
expansion belongs to the basic authoring contract. Input analysis is an
optional editor capability and must tolerate incomplete input. When present,
both operations use the same provider lexer and coordinate system. The compiler
may optimize repeated work internally; the first public API should not expose a
provider-state lifecycle merely to avoid parsing twice.

## Invocation and declaration model

Raven should retain two broad invocation families:

```raven
#[Component]
class Counter { ... }

let value = answer!(Default: 42)

let names = query!(Dialect: "raven") {
    from user in users
    where user.IsActive
    select user.Name
}
```

Attached macros transform or contribute to a declaration. Invocable macros
expand at a grammar position. Delimited token-body macros preserve their body
without ordinary Raven lexing.

A declaration describes independent dimensions rather than encoding them in
one large macro-kind enumeration:

| Dimension | Examples |
| --- | --- |
| Attachment | invocable, attached to type/property/method |
| Input | constants, `ExpressionSyntax`, raw token body, symbolic types |
| Output | expression, statement, member, declaration, type, pattern |
| Contribution | replace, introduce members, introduce peers |
| Capabilities | tokens, fragments, completion, hover, navigation |

The current `macro` direction remains a plausible concise source form:

```raven
macro Html(body: IMacroTokenBody) -> RenderFragment {
    expand LowerHtml(body)
}

macro Component(on target: BaseTypeDeclarationSyntax) {
    replace ImplementComponent(target)
}
```

The declaration should lower to the same normalized compiler contract as a C#
or Raven class-authored plugin. The source syntax must not freeze the binary
adapter ABI.

The complete placement and output contract is specified in the
[macro application model](application-model.md). Application position,
token-body input, and tooling capabilities are independent. A macro supporting
both expression and statement positions declares a closed output set and
receives the actual carrier position through compiler-owned context.

For invocable macros, the source-level return type is the target declaration:
it determines the grammar positions in which the macro can be invoked. A union
return type declares several targets; it does not merely describe an
after-the-fact expansion value.

## Proposed compiler API

The names below are design-level API, not a commitment to preserve the current
types. Before stability, Raven should prefer replacing an awkward contract to
maintaining adapters for experiments.

### Simplicity budget

The common authoring path has a strict budget:

* one category-specific interface or one `macro` declaration;
* one `Expand` method;
* one context containing the authored input and compiler services; and
* one category-checked result factory.

A macro that replaces an expression should not need a descriptor class, input
analysis phase, custom lexer, capability registry, private-state wrapper, or
workspace integration. Those concepts are either derived by the compiler or
opted into only when the macro needs them.

For example, the Raven-authored shape should be approximately:

```raven
macro twice(value: ExpressionSyntax) -> ExpressionSyntax {
    expand quote! {
        $(value) + $(value)
    }
}
```

The C# interface can require an ordinary result-factory call, but should not
require ceremony unrelated to the transformation. The exact quote/splice
spelling is still open.

### Compiler-normalized descriptor

```csharp
public sealed record MacroDescriptor(
    string Namespace,
    string Name,
    string? Alias,
    MacroInvocationKind InvocationKind,
    MacroOutputKind OutputKind,
    ImmutableArray<MacroTarget> Targets,
    ImmutableArray<MacroParameterDescriptor> Parameters,
    MacroCapability Capabilities);
```

The descriptor is immutable, inspectable without executing expansion, and is
the source for lookup, diagnostics, completion, and signature help. It is a
compiler model, not required boilerplate. The compiler derives it from a
category-specific interface and its typed parameter schema, or directly from a
`macro` signature. Macro kind is not repeated as provider properties that
can disagree.

### Basic definition contract

Class-authored macros implement one category-specific expansion contract:

```csharp
public interface IExpressionMacro : IMacroDefinition
{
    MacroExpansionResult Expand(ExpressionMacroContext context);
}
```

Attached, statement, member, type, and pattern variants supply the appropriate
context and statically constrain the result category. Typed parameters use a
generic interface or compiler-generated adapter:

```csharp
public interface IExpressionMacro<TParameters> : IMacroDefinition
{
    MacroExpansionResult Expand(
        ExpressionMacroContext<TParameters> context);
}
```

Multi-position class-authored macros use an advanced contract that declares
their supported positions and returns a `SyntaxNode` through the normalized
result carrier. A Raven union return annotation projects to an exact position
set. `-> SyntaxNode` projects to the explicit “all single-node invocable
positions” wildcard. The wildcard excludes attached contributions and
list-valued member expansion, and the driver validates the concrete node for
every invocation.

This proposal calls `SyntaxNode` output *untyped* only in the syntax-category
sense. It erases `ExpressionSyntax`, `StatementSyntax`, and similar categories;
it does not erase syntax structure or permit raw dynamic values.

Attached macros use the corresponding input-side rule:
`on target: TargetSyntax` marks one compiler-supplied target parameter, and its
syntax type or union declares the attachment targets. `on target: SyntaxNode`
is the category-untyped attached form. Ordinary syntax parameters without `on`
remain caller-supplied inputs.

The normalized parameter schema records a binding role for every parameter:
value, syntax input, context, token body/stream, or attached target. Positional
and named arguments bind only value and syntax-input roles. The compiler
injects all other roles after argument binding, and completion, signature help,
and execution consume this same schema.

Context is opt-in. A Raven-authored macro with only ordinary inputs and an
output return target does not declare or bind a context parameter. The compiler
supplies a context only when a recognized context type is present; any hidden
driver state remains an adapter implementation detail rather than authoring
boilerplate.

The compiler derives names, targets, parameters, and output categories from the
implemented role and declaration metadata. Optional features use separate
capability interfaces so they do not enlarge `Expand` or burden macros that do
not need them:

```csharp
public interface IMacroInputProvider
{
    MacroInputSnapshot AnalyzeInput(MacroInputContext context);
}

public interface IMacroCompletionProvider { /* optional */ }
public interface IMacroHoverProvider { /* optional */ }
```

An implementation may share its own pure lexer/parser helpers between
`AnalyzeInput` and `Expand`. A provider-state exchange API should be added only
if profiling proves duplicate parsing material and a safe compiler-owned cache
cannot solve it internally.

### Input context

```csharp
public abstract class MacroInputContext
{
    public MacroInvocationSyntax Invocation { get; }
    public SourceText BodyText { get; }
    public TextSpan BodySpan { get; }
    public ParseOptions ParseOptions { get; }
    public CancellationToken CancellationToken { get; }

    public MacroDiagnostic CreateDiagnostic(
        TextSpan bodyRelativeSpan,
        DiagnosticDescriptor descriptor,
        params object?[] arguments);
}
```

All body APIs use body-relative spans. The context performs the checked mapping
to the authored syntax tree. Invalid spans produce an API error attributed to
the provider, never a diagnostic at an unrelated source location.

### Optional input analysis and editor snapshot

```csharp
public sealed record MacroInputSnapshot(
    ImmutableArray<MacroInputToken> Tokens,
    ImmutableArray<MacroFragmentSpan> Fragments,
    ImmutableArray<Diagnostic> Diagnostics);

public readonly record struct MacroInputToken(
    TextSpan Span,
    MacroRawKind RawKind,
    MacroTokenClassification Classification,
    MacroSemanticRole? SemanticRole);

public readonly record struct MacroFragmentSpan(
    TextSpan Span,
    RavenFragmentKind Kind,
    MacroFragmentState State,
    MacroScopeBridge? ScopeBridge);
```

`MacroRawKind` combines provider identity with a provider-owned integer. Equal
integers from different providers are unrelated. Classification is an editor
presentation such as keyword, tag, attribute, string, number, operator, type,
property, or component. It is not a new Raven `SyntaxKind`.

Fragment state distinguishes a present fragment from an expected or recovery
slot. A zero-width span can therefore say “a Raven expression is expected
here,” which is essential for completion before valid syntax exists.

The implemented scope-bridge MVP is smaller than the sketched
`MacroScopeBridge`: a `MacroFragmentRegion` carries immutable
`MacroFragmentLocal` name/type pairs. A query macro creates a typed range
variable from an authored source-expression span with
`CreateSequenceElementLocal` and attaches it only to its `where` and `select`
regions. The language server consumes the resulting ordinary local symbols; it
does not synthesize fake source or reverse engineer the final expansion.

The provider returns this snapshot only when it implements the optional input
capability. The authoritative public query remains compiler-owned:

```csharp
MacroInputSnapshot? SemanticModel.GetMacroInputSnapshot(
    MacroInvocationSyntax invocation,
    CancellationToken cancellationToken = default);
```

It may prepare lazily and cache the answer. A null result means the resolved
macro publishes no input metadata, not that callers should guess from its raw
body or expansion.

### Expansion context

```csharp
public abstract class MacroExpansionContext
{
    public Compilation Compilation { get; }
    public SemanticModel SemanticModel { get; }
    public MacroInvocationSyntax Invocation { get; }
    public MacroInputSnapshot Input { get; }
    public CancellationToken CancellationToken { get; }

    public MacroFragmentParser Fragments { get; }
    public MacroSyntaxBuilder Syntax { get; }
    public MacroOriginBuilder Origins { get; }
    public MacroResourceProvider Resources { get; }
}
```

The semantic model belongs to the current stable compilation snapshot. Queries
must follow the normal public Raven semantic APIs. Macro-specific lookup should
be added to `Raven.CodeAnalysis` rather than reconstructed in a plugin or LSP.

The compiler must guard against re-entrant requests for the expansion currently
being produced. A diagnostic is preferable to deadlock, recursion, or a
partially bound answer.

### Raven-facing result shapes and bootstrap layering

The compiler-wide policy is documented in
[Desired Compiler API result shapes after bootstrap](../../../compiler/api/result-shapes.md).
Macro APIs should apply that policy rather than define a separate result model.

The long-term Raven-authored API should look like Raven rather than merely
transliterating nullable C# contracts. Once the compiler bootstrap can consume
these types without introducing a `Raven.Core` build cycle, use:

* `Option<T>` when the only distinction is presence or absence;
* `Result<T, TError>` for one expected success/failure boundary;
* a purpose-built union when callers must handle several meaningful outcomes;
  and
* nested or payload unions when an advanced result needs structured failure or
  continuation data.

These shapes remain ordinary .NET ABI types and are consumable from C#. They
make exhaustive Raven pattern matching the primary authoring experience while
preserving normal interop. Exceptions remain appropriate for violated API
invariants, cancellation, and unexpected macro-host failures—not expected
input rejection.

Bootstrap dependencies are an implementation constraint, not the desired API
model. Transitional compiler-layer methods may therefore return nullable
references or result records. Raven-authored facades can project those shapes
to unions first; after bootstrapping permits it, the owning APIs should migrate
without retaining parallel nullable contracts solely for compatibility.

Not every diagnostic-bearing parser is a binary `Result`: recovered syntax and
diagnostics can coexist. Such APIs should use a purpose-built result or union
whose cases preserve recovery data rather than discarding it to force an
`Ok`/`Error` split.

### Expansion result

```csharp
public sealed record MacroExpansionResult(
    SyntaxNode? Replacement,
    ImmutableArray<MemberDeclarationSyntax> IntroducedMembers,
    ImmutableArray<MemberDeclarationSyntax> PeerDeclarations,
    ImmutableArray<Diagnostic> Diagnostics,
    ImmutableArray<MacroResourceDependency> Dependencies);
```

Category-specific factories should be the normal authoring surface:

```csharp
MacroExpansion.Expression(expression, diagnostics);
MacroExpansion.Replace(declaration, introducedMembers, diagnostics);
MacroExpansion.Introduce(members, diagnostics);
MacroExpansion.Failure(diagnostics);
```

The compiler validates that every output matches the descriptor and target.
Malformed, foreign-tree, or context-incompatible syntax becomes a macro
implementation diagnostic. Normal Raven syntax and semantic errors remain
ordinary compiler diagnostics mapped through provenance.

## Syntax-construction toolbox

No one construction mechanism fits every macro. Raven should provide all of
the following layers and teach authors to use the highest convenient layer.

### Source-backed fragment parsing

The fragment service parses a complete selected region and retains authored
locations:

```csharp
MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult(TextSpan span);
MacroSyntaxParseResult<StatementSyntax> ParseStatementResult(TextSpan span);
MacroSyntaxParseResult<TypeSyntax> ParseTypeResult(TextSpan span);
MacroSyntaxParseResult<PatternSyntax> ParsePatternResult(TextSpan span);
MacroSyntaxParseResult<MemberDeclarationSyntax> ParseMemberDeclarationResult(TextSpan span);
MacroSyntaxParseResult<CompilationUnitSyntax> ParseCompilationUnitResult(TextSpan span);
```

Every result contains recovered syntax, immutable native parser diagnostics,
`HasErrors`, and the actual body-relative syntax span. Complete-fragment APIs
reject unexplained trailing input.
Convenience methods may return syntax directly, but examples should use the
diagnostic-bearing result whenever authored input can be invalid.

The default token stream is also a parser cursor:

```csharp
MacroSyntaxParseResult<ExpressionSyntax> ParseExpression();
MacroSyntaxParseResult<StatementSyntax> ParseStatement();
MacroSyntaxParseResult<TypeSyntax> ParseType();
MacroSyntaxParseResult<PatternSyntax> ParsePattern();
```

These cursor methods begin at the current token, let Raven's grammar determine
the construct boundary, return its body-relative span, and advance through the
consumed tokens. Explicit-span parsing remains the advanced escape hatch when
the outer DSL owns a boundary that Raven grammar cannot infer unambiguously.
Custom token providers retain the same cursor surface because the compiler
wraps their `IMacroTokenStream` in the context-bound `MacroTokenStream`.

Parsing an arbitrary generated string is also useful and should be supported:

```csharp
MacroParseResult<ExpressionSyntax> ParseGeneratedExpression(
    SourceText text,
    SyntaxOrigin origin);
```

It is a fallback, not the default. Source-backed parsing preserves identity and
diagnostic locations naturally; generated strings require an explicit origin
and cannot provide token-level mapping unless the macro supplies it.

### Quote and splice

Quotation is the concise normal path for fixed Raven structure:

```raven
let result: ExpressionSyntax = quote! {
    builder.AddContent($(sequence), $(content))
}
```

Quotes are category-aware syntax literals. Splices insert syntax of the
required category rather than interpolating text. Future categories should
include expressions, statements, types, patterns, members, declarations, and
lists/repetitions.

Quote verification happens when the macro implementation is compiled. The
expanded result is checked again in the consumer context because spliced
syntax and available symbols can differ. Trivia and origins on spliced nodes
are preserved unless the macro explicitly normalizes them.

### Factories and transformations

`SyntaxFactory`, immutable replacement APIs, visitors, and rewriters remain
the precise layer. They are necessary for computed structures, transformations
of existing declarations, and cases that quotation would make less clear.

Raven should retain Roslyn-like immutable, full-fidelity syntax APIs. Factory
construction and quote construction must produce equivalent valid Raven nodes;
quotation is convenience, not a privileged representation.

### Inspection helpers

The toolbox should eventually include:

* structural and source printers;
* a “factory form” printer similar in purpose to `RavenQuoter`;
* node-kind assertions that return diagnostics in production paths
  (`MacroContext.RequireSyntax<TSyntax>` now provides the first such helper);
* `GetDeclaredSymbol`, `GetSymbolInfo`, `GetTypeInfo`, and operations for
  source-backed Raven fragments;
* helpers to inspect declarations, attributes, parameters, and generic
  constraints; and
* an expanded-source view with links back to authored origins.

These are tools over Raven syntax and semantics. They do not require Raven to
standardize an HTML, SQL, or other DSL tree.

## Hygiene and name resolution

Hygiene should prevent accidental capture while allowing intentional
integration with caller code. Raven needs to distinguish at least four name
origins:

1. **Authored/spliced names** retain their call-site meaning.
2. **Generated bindings** receive a fresh compiler identity by default, even
   if their display text matches a caller local.
3. **Definition-site references** intentionally resolve against symbols made
   available by the macro package or its declared runtime support.
4. **Explicit call-site names** intentionally ask the caller's scope to resolve
   a constructed identifier.

`MacroContext.CreateUniqueName` now provides deterministic textual collision
avoidance for generated bindings. It reserves every identifier in the authored
invocation document and every name previously allocated by that context. It is
deliberately not presented as complete hygiene: proposed
`CreateDefinitionSiteReference` and `CreateCallSiteIdentifier` helpers still
need semantic identities and explicit lookup rules. Constructing a reference
from a string must not silently select whichever scope happens to bind it.

The precise default resolution of bare identifiers written inside a quote is
still a design decision. Before declaration and statement quotes become stable,
Raven must specify binding/reference pairing, shadowing, member introduction,
and how a quoted runtime helper is made available to the consumer.

## Source provenance

A single invocation-wide location is insufficient for a convincing DSL. In
the HTML example:

* generated `OpenComponent<Greeting>` should originate at `Greeting`;
* generated `AddAttribute(..., "name", ...)` should originate at `name`; and
* generated content for `{count + 1}` should originate at that expression.

Generated nodes and tokens should accept one of these origin forms:

```csharp
SyntaxOrigin.Authored(TextSpan bodyRelativeSpan)
SyntaxOrigin.Generated(Location invocationAnchor)
SyntaxOrigin.Composite(ImmutableArray<SyntaxOrigin> parts)
SyntaxOrigin.Inherited(SyntaxNode sourceNode)
```

The compiler owns the resulting source map. Quoted tokens inherit the quote
location, spliced syntax retains its existing origin, and factory-built syntax
uses the origin supplied by the macro. When a later diagnostic is reported on
generated syntax, mapping selects the narrowest authored origin, then a
composite authored origin, then the invocation anchor.

Origins are also used for expanded-view navigation and debugging. They must not
change semantic identity or be treated as ordinary trivia.

## Diagnostics and failure behavior

There are four diagnostic lanes:

| Lane | Example | Location |
| --- | --- | --- |
| Carrier/compiler | unknown macro or invalid target | invocation/attribute |
| Native fragment parser | malformed `{count + }` | embedded fragment span |
| Macro-authored | mismatched HTML end tag | responsible DSL token/span |
| Generated Raven | unknown component or wrong parameter type | mapped generated origin |

Expected invalid input is returned as diagnostics. A provider exception,
invalid span, null result, incompatible syntax tree, or contract violation is
reported as a macro implementation failure with the invocation as its fallback
location. Cancellation is propagated and never converted into an error.

Diagnostics are immutable and tied to the document snapshot that produced
them. The compiler normalizes ordering and deduplicates identical diagnostics.

## Expansion order and composition

The compiler should use a documented fixed-point process:

1. parse carrier syntax without loading plugins;
2. resolve the macro and bind its typed arguments;
3. prepare its body and collect input diagnostics;
4. expand the outer invocation;
5. validate and insert its ordinary Raven syntax;
6. discover macro invocations introduced by that result; and
7. repeat within configured depth and work limits.

Outer-first expansion allows an outer macro to consume or transform nested
macro carrier syntax deliberately. Attached macros on one declaration compose
in authored order through an explicit replacement/contribution pipeline. Each
step sees the prior step's declared result, while the original declaration
remains available through context for diagnostics and intentional inspection.

Cycles, repeated equivalent expansions, category mismatches, and depth/work
limit exhaustion produce diagnostics. Partial expansion must not leak into
code generation.

## Determinism, resources, caching, and isolation

A macro expansion cache key should account for:

* provider identity, binary/version identity, and descriptor;
* invocation source, arguments, parse options, and relevant imports;
* semantic identities actually requested through compiler services;
* tracked resource content identities; and
* the compiler/language version and relevant compilation options.

Macros request files or other future resources through `Resources`. The
compiler records dependencies and invalidates the cache when they change.
Ambient current directory, arbitrary environment variables, wall-clock time,
randomness, network access, and untracked process execution are outside the
deterministic contract.

The end-state host should isolate reusable macro plugins from the compiler
process where practical, with cancellation, time/memory limits, protocol
versioning, and crash containment. Same-project and Playground macros may use a
lighter development host initially, but must preserve the same observable
contract. Isolation is an implementation choice; deterministic inputs and
compiler-owned results are language semantics.

## Editor and language-service behavior

The language server asks `Raven.CodeAnalysis` for macro information. It does
not discover assemblies, instantiate providers, parse DSLs, or infer semantics
from generated source.

The compiler-owned snapshot enables:

* semantic token classification for macro-owned tokens;
* ordinary Raven completion, hover, signature help, and navigation inside
  declared Raven fragment spans;
* macro-owned completion in DSL positions;
* expected-token completion at zero-width recovery slots;
* source-accurate diagnostics;
* navigation between authored and expanded syntax; and
* consistent answers across build, editor, and Playground hosts.

Optional input analysis must be fast, incremental where useful, and safe on
incomplete input. Foreground completion may preempt stale background
classification or diagnostics. Compiler caching owns semantic truth; the LSP
may cache only the rendered result for a document version.

Custom completion is a later capability, not a prerequisite for useful DSL
tooling. Classified tokens plus Raven fragment spans already provide meaningful
highlighting and host-language completion for the HTML prototype.

## Packaging, standard library, and Playground

Reusable macros are compiler-plugin assets referenced through ordinary project
or package references. Runtime support required by generated code is an
explicit normal dependency, separate from implementation-only compiler-plugin
dependencies.

The standard macro library should remain small. It may provide foundational
facilities such as quote/splice and representative macros, but framework
integrations such as HTML-to-Blazor should begin as separate libraries and
samples. A macro should hide boilerplate, not hide or replace the framework's
semantic model.

Playground support has two stages:

1. show macro diagnostics, token classifications, expanded Raven, and authored
   to generated navigation using the same compiler APIs; then
2. host a runtime preview when the resulting application model is safely
   distributable and the Playground can resolve its macro/runtime package.

The HTML/Blazor sample remains separate until its macros can be distributed as
a library. The fact that the Playground is itself a Blazor application makes a
future embedded component preview attractive, but does not justify coupling
the prototype to the Playground now.

## Prior art and comparison

Raven should adopt mechanisms because they solve Raven problems, not because a
particular language has them. The following comparison identifies the source
and the adaptation.

| System | Useful idea | Limitation for Raven's DSL goal | Raven adaptation/benefit |
| --- | --- | --- | --- |
| Nim | Macros receive `NimNode`; `typed` and `untyped` inputs; `quote do`, node builders, `parseExpr`, `parseStmt`, `genSym`, type/implementation inspection, and line information. | Even `untyped` input must be parsable Nim, so literal HTML-like syntax needs a carrier or string-shaped escape. Direct tree construction can also require manual location care. | Provide the same broad syntax toolbox for Raven fragments, but preserve arbitrary raw DSL bodies and make source-backed spans/provenance compiler primitives. |
| Rust | Procedural macros consume and produce `TokenStream`; tokens carry `Span`; hygiene and diagnostics build on spans; expansion is explicit. | A token stream is intentionally weak semantic structure, and IDEs must map between macro input/output while procedural macros remain relatively opaque. | Keep a lossless token boundary, then add provider classifications, Raven fragment spans, and compiler-owned semantic/editor snapshots as first-class optional data. |
| Swift | Invocable/attached roles, syntax-checked input and output, constrained expansion regions, and sandboxed implementations make expansion predictable. | Swift macros operate on SwiftSyntax-shaped input and output; arbitrary token DSL authoring is not their primary model. | Retain explicit roles, validation, isolation, and incremental locality while allowing raw bodies whose private grammar is not Raven syntax. |
| Scala 3 | Typed quotes/splices and quote reflection provide type-safe construction plus deep typed-tree inspection. | Typed reflection is powerful but complex and couples advanced macro code to compiler-context-dependent tree APIs. It is also not a general raw-token DSL editor contract. | Make quote/splice the convenient path and Roslyn-like semantic APIs the advanced path, while keeping custom DSL input separate and optional. |
| Elixir | Explicit quote/unquote, metadata, hygienic variables, unique variables, and deliberate hygiene escape hatches. | The quoted representation is dynamically shaped and does not provide Raven's desired statically typed syntax/output categories or integrated DSL editor snapshot. | Adopt explicit hygiene intent and source metadata while retaining immutable typed syntax nodes and compiler validation. |
| Roslyn | Immutable, full-fidelity syntax trees, factories, parsing APIs, source locations, semantic models, and operations form a mature compiler service architecture. | Roslyn does not itself define this Raven macro/DSL execution model, and ordinary syntax trees cannot represent every custom language. | Keep Raven's public compiler APIs Roslyn-like, use them for expansion output and semantics, and add a bounded token/span/provenance layer instead of making syntax extensible by plugins. |

Primary references:

* [Nim Tutorial Part III](https://nim-lang.org/docs/tut3.html) describes typed
  and untyped macro arguments, `NimNode`, `quote do`, and programmatic tree
  construction.
* [Nim `std/macros`](https://nim-lang.org/docs/macros.html) documents
  `parseExpr`, `parseStmt`, `quote`, `genSym`, inspection helpers, and line-info
  APIs.
* [Rust `proc_macro`](https://doc.rust-lang.org/stable/proc_macro/) documents
  token streams, token trees, spans, diagnostics, and procedural macro support.
* [rust-analyzer architecture](https://rust-analyzer.github.io/book/contributing/architecture.html)
  and [IDEs and Macros](https://rust-analyzer.github.io/blog/2021/11/21/ides-and-macros.html)
  explain the practical mapping and isolation work needed to provide IDE
  features through macro expansion.
* [The Swift Programming Language: Macros](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/macros/)
  describes macro roles, AST expansion, output validation, locality, caching,
  and sandboxing.
* [Scala 3 macros](https://docs.scala-lang.org/scala3/reference/metaprogramming/)
  and [quote reflection](https://docs.scala-lang.org/scala3/guides/macros/reflection.html)
  describe typed quotes, splices, reflection, symbols, types, and positions.
* [Elixir macros](https://hexdocs.pm/elixir/macros.html) and the
  [`Macro` module](https://hexdocs.pm/elixir/Macro.html) describe
  quote/unquote, hygiene, unique variables, and AST metadata.
* [Roslyn syntax analysis](https://learn.microsoft.com/en-us/dotnet/csharp/roslyn-sdk/get-started/syntax-analysis)
  describes immutable, full-fidelity syntax trees, while
  [working with syntax](https://learn.microsoft.com/en-us/dotnet/csharp/roslyn-sdk/work-with-syntax)
  describes factories and immutable transformations.

## Why this fits Raven particularly well

No cited system provides exactly this combination. Raven can do so because its
compiler already aims to expose Roslyn-like syntax and semantic services, its
macro carriers can preserve arbitrary source bodies, and its language service
is expected to consume compiler-owned answers.

The potential advantage is not a more powerful private AST. Macro authors can
already build one. The advantage is a narrow shared contract that serves
compilation and editing simultaneously:

* token spans describe the DSL surface without standardizing its structure;
* fragment spans reuse Raven's parser, binder, completion, and semantic model;
* provenance maps normal generated Raven diagnostics back through lowering;
* ordinary syntax output keeps the rest of the compiler macro-agnostic; and
* one compiler-owned snapshot prevents build, LSP, and Playground behavior
  from drifting apart.

This is especially suitable for hybrid DSLs such as HTML with `{expression}`
holes: the macro owns HTML meaning, Raven owns Raven meaning, and spans define
the exact boundary.

## Implementation stages

The stages are capability gates, not compatibility releases:

1. **Consolidate the minimal execution contract.** Normalize the one-method
   category interfaces, contexts, typed arguments, result factories,
   cancellation, and category validation. Keep descriptors compiler-derived
   and replace current experimental APIs where doing so produces a cleaner
   model.
2. **Complete fragment parsing (implemented).** Expression, statement, type,
   pattern, compilation-unit, and exact-one member/declaration helpers use
   category-specific syntax and diagnostic-bearing results.
3. **Complete syntax construction.** Extend quote/splice by category, preserve
   trivia, provide list/repetition splices, and round out factory/inspection
   helpers.
4. **Specify hygiene and provenance.** Introduce unique/definition-site/
   call-site names, origin attachment, diagnostic remapping, and expanded-view
   navigation. Do not stabilize declaration quotes before this stage.
5. **Publish input snapshots.** Add classified tokens, recovery-aware fragment
   spans, `SemanticModel.GetMacroInputSnapshot`, and compiler-owned caching.
6. **Add editor routing.** Implement semantic tokens and ordinary Raven
   completion inside fragments, followed by optional macro-owned completion
   and scope bridges.
7. **Harden execution.** Add tracked resource APIs, cache dependency accounting,
   work limits, out-of-process isolation, and protocol versioning.
8. **Package representative libraries.** Extract HTML/Blazor only after the
   sample proves component composition, diagnostics, provenance, styling, and
   distributable compiler/runtime dependencies.
9. **Integrate Playground previews.** Use the same package and compiler APIs;
   avoid a Playground-only macro implementation.

Each stage should be proven with focused observable tests. Stable tests should
cover diagnostics, symbol/operation shape, source mapping, completion results,
incremental invalidation, and runtime behavior rather than exact emitted
instructions or internal lowered tree shape.

## Acceptance criteria for a complete system

The design is complete enough to stabilize when all of the following hold:

* a simple macro can be authored locally with little more than a declaration
  and quote;
* a reusable macro can be packaged with separate compiler and runtime assets;
* a raw DSL can publish classifications and Raven expression recovery slots
  without exposing its parser tree;
* malformed embedded Raven reports native diagnostics at authored positions;
* generated semantic diagnostics map to the responsible DSL token or fragment;
* generated locals cannot accidentally capture caller locals;
* intentional caller and definition-site references have explicit APIs;
* expansion is deterministic, cancellable, cacheable, and crash-contained;
* build, editor, and Playground hosts obtain the same compiler-owned answers;
* nested and attached macro composition has deterministic documented order;
* expanded source can be inspected and navigated in both directions; and
* the HTML/Blazor library can implement component, parameter, event, attribute,
  content, and nested-component behavior without compiler-specific Blazor
  rules.

## Open questions

The proposal intentionally leaves these decisions for focused experiments:

* What is the exact default lookup rule for bare identifiers inside quotes?
* Should preparation and expansion be one generic class contract, separate
  capabilities, or compiler-generated adapters over `macro` declarations?
* Which additional scope-bridge shapes are justified beyond the implemented
  sequence-element local, and what hygiene rules should they use?
* Which semantic queries are legal during each expansion phase, and how are
  their dependencies recorded without over-invalidating caches?
* How much provenance should factories infer, and when must authors attach it
  explicitly?
* Which isolation boundary gives local macros a fast authoring loop while
  preserving the behavior of packaged plugins?
* What work and recursion limits are appropriate for interactive and build
  hosts?
* Which syntax and ABI surface, if any, should be declared stable first?

These questions should be answered with multiple macros of different shapes.
The HTML/Blazor prototype is the primary hybrid-token case; at least one
non-HTML token DSL and one host-shaped attached macro should inform the final
public API.
