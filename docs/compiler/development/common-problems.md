# Common problem

Here are listed some common problems that might occur when developing Raven:

## List

* **SyntaxPrinter is failing to resolve source location**

    * If there is a mismatch between the input and output of the source tree, they are no identical, the sources spans are not correct and the source location can't be resolved. This usually indicates a problem with the parsing on the nodes, or at the lexer level.

    * Set ``includeLocation`` to ``false`` and you can see the output and compare it to the input. Then you are able to debug and see where token widths don't add up.

    Like this:

```csharp
root.PrintSyntaxTree(new PrinterOptions { IncludeNames = true, IncludeTokens = true, IncludeTrivia = true, IncludeSpans = true, IncludeLocations = false });
```