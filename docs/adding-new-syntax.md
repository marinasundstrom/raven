# Adding new syntax

This document is (at the moment) more of a checklist of what to do when adding new syntax to the compiler.

Prerequisite reading:
* [Syntax tree](syntax-tree.md)
* [Source generation](source-generation.md)

Keep in mind:

Syntax resides in the `Raven.CodeAnalysis.Syntax` namespace .

## Adding token

If you need to add a new kind of token:

* Add `SyntaxKind` for token
* Make changes to the lexer and the tokenizer.
* Add methods to `SyntaxFactory`, both for Red and Green tree.


## Adding syntax nodes

### Naming of classes

Syntax node types must have the suffix `Syntax`. Thus the class for the statement syntax is called `StatementSyntax`. This applies both for the Red and Green tree.

### Steps

The following steps need to be followed in order to add a new `SyntaxNode`:

* Add `SyntaxKind` for node
* Add Green Node class (`InternalSyntax`)
    * Derives from `InternalSyntax.SyntaxNode`
* Add Red Node (External API)
    * Class should: 
        * Be marked as `partial`
        * Derive from `SyntaxNode`, or a class that derive from `SyntaxNode`
    * Properties should:
        * Be of supported types: `SyntaxToken`, derivatives of `SyntaxNode`, and syntax lists. 
        * Be marked as `partial`
        * Have `get` accessor
    
* Add methods to `SyntaxFactory`, both for Red and Green tree.

### Handled by the generator

The rest will be dealt with by the generator, including:

* Implementing `Update` method for updating the node
* Implementing `With*` methods for updating tokens and child nodes
* Implementing `Accept` methods for the visitor pattern
* Implementing `Visit*` methods for node in `SyntaxVisitor` and `SyntaxRewriter`.

### To think about

If you are designing an hierarchy of nodes involving abstract classes, then you might have to manually add some `Visit*` methods for the abstract types in `SyntaxVisitor` and `SyntaxVisitor<TResult>`. 

As we have done for `StatementSyntax` and `ExpressionSyntax`, with the methods `VisitStatement` and `VisitExpression`.