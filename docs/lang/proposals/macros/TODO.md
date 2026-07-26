# TODO

Compiler-plugin. Macro. Integration points.

* Minimal impact on Raven architecture. Uses the Raven API.

In a macro you can put any content and parse it however you want.

* Keep macro discovery, expansion, and diagnostics usable through
  `Compilation` without `Workspace`. Analyzers and generators remain workspace
  plugins; do not route macro execution through their host pipeline.
* Syntax-producing macros - from Token Stream
* Expand and substitute
* Read and Peek
* [x] Ability to use Raven expression and statement fragment parsers
* Report diagnostics
* Mapping of location in macro onto expanded source to keep source location during binding
* Semantic pass for additional
* Contextual re-mapping of token kinds
* Syntax-level macros - Processes syntaxes
* [x] Compiler-owned expression-only `#quote` intrinsic.
* [x] `#(expression)` quote holes lowering directly to caller-bound
  `ExpressionSyntax`.
* Contextual statement, member, and declaration categories plus
  token/identifier/list/repetition splices remain. See
  [quote-macro.md](quote-macro.md).
* Add a semantic-model query for macro-provided retained structure. An embedded
  `ExpressionSyntax` should automatically enter ordinary Raven expression
  analysis when an analyzer host is present. Unstructured macros return no
  structure, and macro execution must not require analyzers.
* Make workspace analyzers expansion-aware so references used only by
  expansions participate in unused-value and related analysis.
* Add a compiler-owned compile-time resource API for project-relative path
  resolution, dependency tracking, incremental invalidation, diagnostics, and
  file-access policy. The test-only `#embedText` macro currently proves direct
  expansion-time loading only.
* Add category-aware factory methods for macro expansion results so authors can
  create success, success-with-diagnostics, diagnostic-only, replacement, and
  no-change combinations without manually assigning result properties.
* Replace the consumer-authored `RavenMacro` item with provider-declared
  compiler-plugin metadata carried through normal project/package references.
  Do not scan and execute arbitrary runtime references.
* Support macros declared and consumed in the same project through an acyclic
  compile-time source partition. The activation path must work in memory for
  the Playground, exclude compile-time-only implementation details from normal
  emit, cache independently from consumer edits, and diagnose dependency
  cycles.
  * [x] Activate an already-emitted macro assembly from an in-memory image.
  * [x] Compile and activate an explicitly classified local source partition.
  * [x] Automatically classify local macro declarations or files.
  * [x] Cache the partition independently and invalidate dependent expansions.
  * [x] Diagnose direct dependencies from local macro code to consumer
    declarations.
  * [x] Route authored positions to the current macro or consumer semantic
    projection.
  * [x] Adopt position-aware semantic routing in hover and completion.
  * [x] Adopt position-aware semantic routing in definition, references, and
    rename.
  * [x] Run workspace analyzers over both projections of mixed local-macro
    documents.
* Grow the automatically registered default macro environment beyond `#quote`.
  A future `#embedFile` must use compiler-owned resource resolution and
  dependency tracking rather than unrestricted direct file I/O.

## Sandbox


```raven
func xml (context: MacroContext<ExpressionSyntax>) -> () {
    val token = context.ReadToken
    val expr = context.ParseExpression()
    
    val newNode = SyntaxFactory.IdentifierName("foo")

    context.Materialize(newNode)
}
```

```raven
val name = "Foo"

var root = xml! {
    <root>
        <a text="{name}">{
            if x > 2 {

            } else {

            }
        }</b>
    </root>
}
```
