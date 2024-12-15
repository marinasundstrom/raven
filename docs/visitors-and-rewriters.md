# Syntax Visitors and Rewriters

Raven compiler architecture stores parsed source code as an abstract syntax tree. Tree structures can be easily traversed using the Visitor pattern in order to analyze the nodes. 

A tree can also be transformed, or "rewritten". Since the syntax trees are immutable, every modification results in a new syntax tree.

This document is about the `SyntaxVisitor`, `SyntaxVisitor<TNode>`, and `SyntaxRewriter` classes.

## Types of visitors

* Plain `SyntaxVisitor` allow you traverse a tree to make some analysis on nodes.

* Rewriters are a special kind of `SyntaxVisitor` that allows you to rewrite nodes. For transforming trees.

## How it works

`SyntaxVisitor` and `SyntaxRewriter` have `Visit` methods for each `SyntaxNode` type. These are source generated. And can be overridden by a class that extends the visitor or rewriter class.

Every `SyntaxNode` implement `Accept` methods that take the `SyntaxVisitor` as an argument and call the appropriate `Visit` method for the node type. These are also source generated.

## Source generation

As mentioned, each `SyntaxNode` type has a set of generated `Accept` methods. Accepting `SyntaxVisitor` and `SyntaxVisitor<TNode>` (like `SyntaxRewriter` also), respectively.

The `Visit` methods of the `SyntaxVisitor`, `SyntaxVisitor<TNode>`, and `SyntaxRewriter` are source generated for each `SyntaxNode` class by a source generator. So all you have to do is to override those virtual methods. 

Because of being source generated `SyntaxVisitor` and `SyntaxRewriter` already contain default implementations for traversing and rewriting the tree nodes.