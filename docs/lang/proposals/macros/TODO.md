# TODO

Compiler-plugin. Macro. Integration points.

* Minimal impact on Raven architecture. Uses the Raven API.

In a macro you can put any content and parse it however you want.

* Syntax-producing macros - from Token Stream
* Expand and substitute
* Read and Peek
* Ability to use Ravens expression and statement parsers
* Report diagnostics
* Mapping of location in macro onto expanded source to keep source location during binding
* Semantic pass for additional
* Contextual re-mapping of token kinds
* Syntax-level macros - Processes syntaxes

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