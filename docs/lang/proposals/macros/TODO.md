# TODO

Compiler-plugin. Macro. Integration points.

* Minimal impact on Raven architecture. Uses the Raven API.

In a macro you can put any content and parse it however you want.

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
* Make compiler analyzers expansion-aware so references used only by macro
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
