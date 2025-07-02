# Common problem

Here are listed some common problems that might occur when developing Raven:

## List

* **Getting an exception when accessing property in Red Tree.**

    * The order of the slots in a Green Node doesn't reflect the order of properties in the Red Node.
    * The property in the Red tree is non-nullable.

* **SyntaxPrinter is failing to resolve source location**

    * If there is a mismatch between the input and the output of the source tree the sources spans are not correct and the source location can't be resolved. This usually indicates a problem with the nodes, or at the lexer or tokenizer level.

    * Set ``includeLocation`` to ``false`` and you can see the output and compare it to the input. Then you are able to debug.

    Like this:

```csharp
root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = false });
```