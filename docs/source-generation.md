# Source generation

Some parts of the implementation is quite repetitive for all the nodes in the "Red" tree. That is why we use source generators to implement that. 

With the help of partial classes and partial properties we can add code to existing class definitions.

We talk about one specific scenario for generating code in the document about the [syntax tree](syntax-tree.md).

## What is being generated?

### Syntax node

For each `partial` class that is extending `SyntaxNode`, or a class that is extending `SyntaxNode`:

* The implementation of `partial` properties retrieving child of type `SyntaxNode`, `SyntaxToken`, or lists. Plus helper function.

* `With*` methods (e.g. `WithName`, `WithIfKeyword`)

* ``Update`` methods.

### Visitor

* Generate `Visit*` methods (e.g. `VisitIfStatement`)


### Rewriter

* Generate overrides that invoke `node.Update( ... )`

### Syntax Factory

Generate methods for creating nodes, token, trivias.