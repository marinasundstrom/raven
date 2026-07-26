# Macro and DSL developer experience

This document defines the intended authoring and tooling experience for Raven
procedural macros, especially macros that embed domain-specific languages.
Implementation sequencing is tracked separately in
[the macro implementation plan](implementation-plan.md).

## Experience goals

A macro DSL should feel like a first-class, contained language region:

* the Raven file parses even when the macro plugin is unavailable
* the authored macro body is preserved exactly
* diagnostics point into the authored body
* completion, highlighting, hover, and navigation can be supplied by the macro
* embedded Raven fragments use Raven parsing and semantic services
* expansion is visible and debuggable
* editing remains responsive and results are tied to one document snapshot
* failures in a macro or its tooling do not crash the compiler or editor

The compiler owns macro semantic truth. The language server schedules requests
and presents compiler results; it does not load or invoke macro plugins
directly.

In user-facing terms, freestanding procedural macros use function-like or
delimited invocations, while attached macros use attributes. During binding,
the compiler resolves the macro implementation and expands the invocation or
attribute into typed ordinary Raven syntax. It is not unconstrained textual
substitution.

Macros also move validation into compilation. A DSL parser, embedded Raven
fragment parser, or macro semantic check runs as part of binding and can report
a diagnostic against the authored invocation. Documentation should contrast
this with runtime string parsing or deferred generation when early,
source-accurate failure is a material reason to choose the macro.

## Two kinds of syntax structure

### Raven invocation carriers

The Raven syntax tree contains compiler-known carrier nodes at valid grammar
positions. The current expression carrier is
`FreestandingMacroExpressionSyntax`:

```raven
val result = #query {
    from user in users
    select user.Name
}
```

Future carriers may represent statement and member/declaration positions. A
carrier preserves the macro name, delimiters, and lossless raw body. It means
"expand here"; it is not itself the expansion result.

The compiler validates that expansion returns ordinary Raven syntax matching
the carrier category. An expression carrier must produce an `ExpressionSyntax`,
a statement carrier must produce statement syntax, and so on.

### Structured DSL wrappers

A macro may parse its body into a secondary tree of special wrapper nodes for
its own DSL. These wrappers can model DSL expressions, statements,
declarations, clauses, or other concepts, but they do not derive from Raven
`ExpressionSyntax`, `StatementSyntax`, or other ordinary grammar nodes.

Wrapper nodes should follow familiar Raven conventions:

* provider-owned raw kinds
* child nodes and `SyntaxToken` values
* body-relative spans
* diagnostics
* visitors and rewriters where useful

The secondary tree is derived from the raw body and is associated with the
macro invocation plus document snapshot. It supports expansion and tooling.
The macro ultimately lowers it to ordinary Raven syntax before binding.

## Token streams

Macro parsers consume a common token-stream contract whose output primitive is
`SyntaxToken`.

### Standard Raven stream

Raven provides a default stream backed by the normal Raven lexer. This is the
preferred starting point for DSLs that mostly use Raven-compatible lexical
forms.

A macro can install a body-scoped keyword overlay. The standard stream first
lexes a word as an ordinary Raven identifier, then reclassifies it with a
provider-owned `RawKind` and a keyword or reserved-word presentation:

```text
Raven Kind: IdentifierToken
Macro RawKind: QueryKeyword
Text: "query"
```

This does not add a value to `SyntaxKind`, change `SyntaxFacts`, or affect
lexing outside the macro body.

### Custom streams

A DSL with a genuinely different lexical grammar may replace the standard
stream with a custom lexer-backed implementation. Custom tokens use
provider-owned integer raw kinds. Fully custom tokens may use `SyntaxKind.None`
as their Raven kind while retaining text, value, span, and `RawKind`.

The stream/provider owns the mapping from raw kinds to names and editor
classifications. Equal integers from different providers do not imply equal
token kinds.

## Hybrid parsing and embedded Raven

A macro may choose:

* complete custom parsing
* complete Raven fragment parsing
* custom outer parsing with selected spans delegated to Raven

There are two supported lowering paths:

1. **Direct lowering:** consume the standard or custom token stream and
   immediately construct the ordinary Raven syntax returned by expansion.
2. **Structured lowering:** build and retain a secondary DSL wrapper tree for
   diagnostics and editor tooling, then translate that structure into the
   ordinary Raven syntax returned by expansion.

The structured layer is optional. Simple macros should not have to manufacture
a secondary tree, while tooling-rich DSLs should not have to reparse raw text
independently for every editor request. Both paths share the authored raw body,
body-relative spans, token conventions, diagnostics, caching boundary, and
typed Raven output contract.

The first executable MVP deliberately uses direct lowering:

```raven
let shouldRetry = #guard {
    unless answer == 42
}
```

The macro reads the body-scoped `unless` keyword from Raven's standard token
stream, delegates `answer == 42` back to Raven's expression parser, and returns
an ordinary logical-negation expression. This validates the common path before
introducing retained DSL structure.

The next direct-lowering step uses multiple clause boundaries:

```raven
let verdict = #choose {
    test answer == 42
    then "correct"
    otherwise "wrong"
}
```

Here the clause words are reserved only inside this macro body. Their token
positions define three Raven fragment spans, which are parsed independently
and lowered to one ordinary `if` expression. A LINQ-like DSL follows the same
basic approach with more clause kinds and repeated clauses.

The first LINQ-like MVP applies that model directly:

```raven
let result = #query {
    from item in source
    where item.IsActive
    select item.Name
}
```

The source, predicate, and projection are Raven fragments. The authored `item`
name becomes the parameter of ordinary generated `Where` and `Select` lambdas,
so it scopes over the predicate and projection without leaking outside the
expansion. No hidden macro-generated name is needed in this subset.

Fragment parsing has two API shapes. `ParseExpression(span)` is the concise
syntax-only convenience for macros that deliberately handle invalid input
themselves. `ParseExpressionResult(span)` returns
`MacroSyntaxParseResult<ExpressionSyntax>`, containing the recovered syntax,
native Raven parser diagnostics, and `HasErrors`. Diagnostic locations are
mapped to the invocation's original syntax tree, so a macro can forward them
through `FreestandingMacroExpansionResult.Diagnostics` without translating
generated-tree positions or wrapping them as macro failures.

`ParseStatement(span)` and `ParseStatementResult(span)` provide the same two
shapes for a complete Raven statement. The parse-result type remains generic so
later type, pattern, member, and quote fragment entry points can share the same
developer experience. Adding those remaining category-specific parsers is
incremental work; the generic result does not make Raven's ordinary syntax
hierarchy extensible.

For example, a query DSL can parse its clauses itself while treating a filter
body as a Raven expression:

```raven
val result = #query {
    from user in users
    where {{ user.IsActive && user.Age >= 21 }}
    select {{ user.Name }}
}
```

All lexer tokens, wrapper nodes, diagnostics, completion edits, and delegated
Raven fragments use one body-relative coordinate system. Compiler APIs map
those spans back to the authored syntax tree.

Expression fragment helpers return both syntax and mapped parser diagnostics.
The same result shape should extend incrementally to statements, patterns,
types, members, declarations, and compilation units.

## Macro capability hooks

The macro definition is the registration point for optional capabilities:

* expansion/lowering
* standard keyword overlay or custom token-stream provider
* structured DSL parser
* completion provider
* token classification/highlighting provider
* hover and signature-help providers
* definition/navigation provider where the DSL has referable entities

The compiler registry discovers these capabilities together. Compiler services
own caching, cancellation, invalidation, diagnostics, and deterministic result
ordering.

Editor capabilities should share the same cached token and structure snapshot
as expansion. A completion provider should not silently tokenize or parse the
body under different rules from the macro expander.

Retained structure is also the routing map for mixed-language editor services.
A structure snapshot should be able to mark a body-relative region or recovery
slot as expecting a Raven expression, statement, type, pattern, member, or
other supported fragment category. At a completion position, the compiler can
then choose between:

* macro-owned completion for DSL tokens and structure
* ordinary Raven completion for a marked Raven fragment
* a macro-provided semantic bridge when the DSL introduces names visible
  inside that fragment, such as a query range variable

Recovery slots matter as much as successfully parsed child nodes because
completion is commonly requested at an empty or incomplete location. The
compiler-owned snapshot must therefore retain expected categories and spans
even when no valid Raven fragment exists yet.

The exact scope-bridge API remains future work. It must preserve caller scope,
macro hygiene, and compiler-owned semantic caching rather than requiring the
language server to reconstruct an expansion or call the plugin directly.

The same snapshot is the analyzer boundary. A future semantic API should
conceptually support:

```text
GetMacroStructure(invocation) -> MacroStructureSnapshot?
```

The snapshot exposes provider-defined structured nodes and compiler-recognized
embedded Raven fragments. A null result means the macro did not opt into
retained structure; it does not mean an analyzer should attempt its own parse.

## Compiler-plugin dependencies

### Compiler plugins, not workspace plugins

Procedural macros are compiler plugins. The compiler resolves and executes them
during binding because their expansions become the Raven program that is
subsequently analyzed and emitted. A caller can therefore construct a
`Compilation` directly and run macros without creating a `Workspace`, loading a
project, or invoking MSBuild.

Analyzers and generators are workspace plugins in Raven's architecture. A
workspace or build host discovers and schedules them around a compilation.
Their orchestration belongs to workspace/project-system functionality even
when their diagnostics or generated sources are eventually supplied to a
compilation.

| Concern | Procedural macros | Analyzers and generators |
|---|---|---|
| Plugin boundary | Compiler | Workspace/build host |
| Semantic phase | Expansion during binding | Scheduled around compilation |
| Needed for direct `Compilation` use | Yes, through defaults or `MacroReference` | No |
| Defines the bound program | Yes | No; may inspect it or provide host-generated inputs |
| Requires a `Workspace` | No | Yes, for plugin discovery and orchestration |

This boundary does not mean the compiler must discover arbitrary assemblies.
The SDK may resolve provider-declared assets for a project, but it passes macro
references into the compiler. From that point onward, macro registration,
expansion, diagnostics, and semantic behavior are compiler-owned.

Workspace analyzers may still act on macro-authored code. The compiler should
expose invocation syntax and expansion mappings through stable compiler APIs.
For a macro that explicitly retains structured DSL syntax, the semantic model
should additionally expose that immutable structure snapshot.

An `ExpressionSyntax` embedded in that macro structure can automatically
trigger the ordinary Raven expression-analysis pipeline when a workspace
analyzer host is present. The same rule can later apply to structured
statements, types, patterns, and members. The macro author marks the typed
fragment once; the macro does not manually invoke individual analyzers.

This is opt-in structure, not inference. If a macro lowers directly or performs
completely opaque custom parsing without returning structure, the structured
macro query returns no result. Analyzers must not reconstruct a supposed DSL
tree from raw tokens or reverse-engineer one from the expansion.

That interoperability is optional. Macro parsing, expansion, binding, and core
diagnostics must not require an analyzer or a workspace. An analyzer consumes
compiler-owned macro information when it is available; it does not create that
information or participate in the macro's execution contract.

A macro project or package should declare that its output is a Raven compiler
plugin. A consuming project should then use an ordinary project or package
dependency. The SDK reads the provider metadata, classifies the compiler-plugin
asset, and passes it to `Compilation`; the compiler loads its manifest and
registers the exported macros.

Provider identity may be represented by an assembly-level marker emitted by
the macro project, conceptually:

```raven
[assembly: RavenCompilerPlugin]
```

The marker may explicitly list one or more plugin types when they are known.
That is the preferred deterministic manifest. A bare marker can authorize
fallback discovery of `IRavenMacroPlugin` implementations inside that marked
assembly when plugin types were not declared individually. Raven must never
perform that type scan for an unmarked runtime reference.

Consumers should not need a separate source import or an analyzer-like
"import macros from this assembly" item. Conversely, Raven must not discover
plugins by scanning and executing every ordinary runtime reference. Provider
metadata supplies explicit plugin identity and execution intent while keeping
the consumer dependency model conventional.

The current `<RavenMacro Include="...">` item is MVP plumbing to replace with
this provider-declared asset model. Macro-name conflicts and load failures
remain compilation diagnostics regardless of how the plugin asset was
resolved. The final assembly attribute names and whether the explicit manifest
stores plugin types directly or through generated metadata remain open.

## Default macro environment

The selected Raven compiler and SDK may provide a version-matched default macro
set. Default macros are registered automatically in normal compilations and in
the Playground, without a source import, package dependency, or project item.

`#quote` is the first default macro and is currently implemented as a compiler
intrinsic. A future tracked-resource macro such as `#embedFile` may instead be
implemented as an SDK-bundled compiler plugin. That implementation distinction
must not change invocation syntax, completion, diagnostics, or documentation.

Third-party macros still arrive through provider-marked project or package
dependencies. Same-project macros arrive through the local compile-time
partition. Default registration must be deterministic for the selected Raven
toolchain version, and conflicts with dependency-provided macros must produce
clear compiler diagnostics.

## Same-project macros

Raven should allow a project to declare and consume procedural macros in the
same source project. A separate macro project remains useful for reuse and
packaging, but it must not be required for local development or experimentation.

Same-project support requires a staged compilation rather than loading the
project's final runtime assembly:

1. parse the complete project snapshot;
2. identify project-local compiler-plugin and macro declarations;
3. bind and compile the compile-time partition against its allowed references
   and compiler contracts;
4. load or otherwise activate that partition through the macro execution host;
5. register its macros with the project compilation;
6. expand invocations in the remaining source; and
7. continue ordinary binding and emit over the expanded program.

The initial dependency rule should be deliberately acyclic. Project-local macro
implementations may depend on compiler contracts, referenced libraries, and
other declarations explicitly admitted to the compile-time partition. They
must not depend on runtime declarations whose own binding requires those macro
expansions. Raven should diagnose that cycle rather than relying on source-file
or declaration order.

Macro declarations are compile-time implementation code. Whether they are also
emitted into the final runtime assembly should be explicit; the default should
not accidentally ship compiler-plugin implementation details as application
runtime API.

Incremental compilation should cache the activated local plugin by the macro
source partition, references, parse options, and compiler version. Editing only
consumer code can reuse it. Editing a macro declaration invalidates that
artifact and all dependent expansions.

This path is also required for the Playground. It must operate from an in-memory
project snapshot and must not depend on MSBuild, a separate project, or a plugin
assembly written to disk. The execution-host abstraction must support the
Playground environment with cancellation, resource limits, deterministic
diagnostics, and isolation appropriate to untrusted interactive code.

The compiler now accepts an emitted macro assembly image directly as a
`MacroReference`. This proves the disk-free activation boundary needed by the
Playground and same-project staging.

The compiler-only MVP now also accepts an explicit local source partition:

```csharp
var compilation = Compilation.Create("App", options)
    .AddReferences(references)
    .AddMacroSyntaxTrees(macroTree)
    .AddSyntaxTrees(consumerTree);
```

`AddMacroSyntaxTrees` keeps those trees out of the runtime source assembly,
compiles them as an in-memory library before consumer binding, activates their
plugins, and forwards their diagnostics through the consumer compilation.
Completion reads from the same compiler macro registry and therefore includes
successfully activated local macros.

For the dedicated-file MVP, a project may instead mark a plugin declaration:

```raven
import Raven.CodeAnalysis.Macros.*

[LocalMacroPlugin]
class ProjectMacros: IRavenMacroPlugin {
    // ...
}
```

When syntax trees are added through
`Compilation.AddSyntaxTreesWithLocalMacros`, or through normal Workspace and
SDK project construction, the complete file containing that marker is
classified into the local compile-time partition. No `RavenMacro` item,
separate project, on-disk plugin assembly, or explicit project reference to
`Raven.CodeAnalysis` is required. The local partition receives the compatible
compiler contracts automatically.

This is deliberately a syntax-only, file-granular rule. The macro plugin and
its supporting implementation types must live in a dedicated source file;
ordinary consumer declarations in that file would also be compile-time-only.
`LocalMacroPluginAttribute` is a local source-partition marker, not a macro
invocation and not the future assembly-level marker that opts reusable
dependency outputs into compiler-plugin discovery.

Interactive and mixed-file code can instead mark individual top-level
compile-time declarations:

```raven
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*

[LocalMacro]
class ProjectMacros: IRavenMacroPlugin {
    val Name: string => "Project macros"

    func GetMacros() -> ImmutableArray<IMacroDefinition>
        => [AnswerMacro()]
}

[LocalMacro]
class AnswerMacro: ITokenTreeExpressionMacro {
    // ...
}

let answer = #answer { }
```

`[LocalMacro]` moves only the marked top-level declaration and its nested
declarations into the compile-time partition. Each separate top-level plugin or
support type must be marked. The compiler derives macro and consumer syntax
trees with the same length and line layout as the authored document, replacing
the opposite partition with whitespace so expansion diagnostics map to the
original offsets.

The partition enforces the initial acyclic rule by construction: local macro
code cannot bind against consumer declarations. Dedicated-file Workspace
documents retain semantic-model access to a marked macro tree even though it is
excluded from the runtime source assembly.

The Playground uses the declaration marker to declare and consume macros in one
user source buffer. The included local-macro example covers attached,
argument-style expression, and raw token-tree expression macros; a second
example demonstrates `#quote` directly.

Macro-author hover, completion, navigation, and semantic queries across the two
derived projections remain future developer-experience work. Remaining
compiler work also includes independent partition caching and invalidation,
richer dependency resolution for the in-memory image, and dedicated cycle
diagnostics.

## Expansion result construction

Macro result types should eventually provide category-aware factory methods so
authors do not have to discover valid property combinations through object
initializers. The factories should cover at least:

* a successful expression, statement, member, or declaration expansion;
* successful syntax plus forwarded Raven parser diagnostics;
* one or more macro-authored diagnostics with no expansion;
* replacement plus introduced members for attached macros; and
* an explicitly empty/no-change result.

The final names remain open, but the factories should normalize default
immutable arrays, reject contradictory combinations, and preserve the typed
output contract. Property initialization can remain as a low-level or
compatibility path rather than being the primary authoring experience.

## Documenting macros

Built-in and plugin macros should use the same task-oriented documentation
shape. Invocation should feel uniform; origin and installation are important
availability facts, not the opening definition of the feature.

A macro reference page should cover, in this order:

1. **Purpose:** the problem it solves and when to choose it.
2. **Quick example:** the smallest realistic invocation and observable result.
3. **Availability:** compiler built-in, SDK/package plugin, or project-local
   plugin; required project references and trust/execution implications.
4. **Input contract:** delimiters, DSL grammar, embedded Raven fragment slots,
   introduced names, and recovery behavior.
5. **Output contract:** expression, statement, declaration, member, or other
   expansion category and any runtime syntax value type.
6. **Composition rules:** scope, hygiene, trivia, splices, repetition, and
   evaluation behavior.
7. **Tooling:** diagnostics, highlighting, completion, hover, navigation, and
   expansion preview currently supported.
8. **Limitations:** unsupported categories, performance constraints, and
   version/runtime dependencies.
9. **Expansion model:** a secondary explanation for debugging and advanced
   users, not the primary statement of purpose.

Documentation for a DSL macro should explicitly mark which regions use custom
grammar and which delegate to ordinary Raven. The same boundary description
should drive highlighting and completion expectations, so the manual does not
promise editor behavior the macro has not registered.

For example, `#quote` should be introduced as a syntax literal: write Raven
code in its readable form and receive the corresponding syntax tree. Its
motivating cases are macro templates, generators, refactorings, and
syntax-oriented tests. The `SyntaxFactory` expansion is useful supporting
detail, but it is not the user-facing purpose.

## Diagnostics

Macro diagnostics should:

* use authored body spans whenever possible
* identify whether an error came from envelope parsing, DSL lexing/parsing,
  embedded Raven parsing, semantic validation, or expansion
* retain the macro invocation as a related location when useful
* remain stable across non-intersecting edits when spans can be translated
* never surface plugin exception stacks as ordinary user diagnostics

An unavailable or failed macro plugin should leave the carrier and raw body
intact so syntax highlighting, navigation around the containing Raven file,
and later recovery remain possible.

## Completion and highlighting

Macro completion has two levels:

1. Raven completes macro names at `#name` and inserts the appropriate carrier
   shape, such as `query { }`.
2. Inside the body, the compiler routes completion to the resolved macro
   capability using the current body-relative position and cached DSL
   structure.

Highlighting uses classifications rather than global `SyntaxKind` additions.
Macro token metadata can mark raw kinds as keyword, reserved word, identifier,
literal, operator, punctuation, comment, or a future custom semantic
classification.

When a macro has no editor provider, the body should still retain neutral,
readable delimiter/text presentation without being misclassified as broken
Raven code.

## Expansion visibility and navigation

Developers should be able to:

* preview the expanded Raven syntax
* compare authored and expanded source
* navigate from the invocation to the macro definition
* see whether a diagnostic belongs to authored DSL input or generated Raven
  output
* inspect macro expansion from CLI/debug captures without an editor

Generated syntax should carry source mappings back to body spans or the
invocation. Navigation and diagnostics must not require consumers to understand
macro caches or generated-tree identities.

Compiler analyzers must eventually account for references introduced through
expanded syntax. Until that integration exists, a value referenced only inside
a raw macro body can still receive an unused-value diagnostic even though the
expansion binds and executes it.

## Compile-time resources

Resource-producing macros are a useful procedural-macro case. For example:

```raven
let template = #embedText("templates/welcome.txt")
```

Such a macro reads the file during expansion and returns an ordinary Raven
string literal, so a missing or unreadable file can be reported as a
compile-time diagnostic on the path argument rather than failing at runtime.
A test-only macro validates that basic execution and diagnostic path.

Direct `File.ReadAllText` is not yet the intended production contract. A
compiler-owned resource API should:

* resolve relative paths from an explicit project or source-file base;
* record the normalized path and content identity as expansion dependencies;
* invalidate cached expansion and diagnostics when the resource changes;
* participate in cancellation and deterministic build inputs;
* report missing, inaccessible, or disallowed resources at the authored
  argument; and
* enforce the configured macro file-access policy.

Until those inputs are tracked, file-reading macros remain trusted build
extensions whose external dependencies are invisible to incremental
compilation.

## Responsiveness and isolation

Macro tokenization, structure parsing, editor services, and expansion must be
cancellable and versioned with the compilation/document snapshot. Foreground
completion and hover should not wait behind stale project-wide macro work.

The initial in-process plugin model treats macro assemblies as trusted build
extensions. `AssemblyLoadContext` provides dependency isolation, not a security
sandbox. Time/resource limits and an out-of-process execution protocol remain
future hardening work.
