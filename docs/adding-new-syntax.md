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

## Adding syntax nodes

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
    
* Add methods to `SyntaxFactory`

The rest will be dealt with by the generator.