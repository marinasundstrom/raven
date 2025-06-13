# Semantic binding

Semantic binding is the process of identifying meaningful parts in your program.

This article is covering the following concepts:

* Binders
* Bound tree

## Binders

A `Binder` is a specialized class responsible for resolving symbols in a particular scope. It binds syntax to symbols.

Binders are an implementation detail of the `SemanticModel`, which is responsible for the overall semantic analysis of a syntax tree - resulting in Bound Nodes, a so called Bound Tree.

Due to their chained nature, they can ask a parent binder for a symbol if they cannot find it in their own scope. This allows for a hierarchical resolution of symbols

Here are some of the key binder types:

* `GlobalBinder` is the top-level binder that represents the entire compilation. It is responsible for binding global symbols, such as namespaces and types, and serves as the entry point for symbol resolution.

* `BlockBinder` is a specific type of binder that is responsible for binding local variables and parameters within a method or block of code. It uses the `SemanticModel` to resolve symbols and check for any potential conflicts or errors.

## Bound tree

The semantic analysis of a syntax tree results in a Bound tree. It's the binders that create bound nodes.

The Bound tree contains Bound nodes that represent meaning in the program, such as expressions and their bound symbols an child expressions. This tree is not tied to the syntax, but the units of meaning.

The code generator is using this bound tree when generating the actual IL (bytecode).