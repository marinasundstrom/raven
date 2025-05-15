# Binders

A `Binder` is a specialized class responsible for resolving symbols in a particular scope. It binds syntax to symbols.

Binders are an implementation detail of the `SemanticModel`, which is responsible for the overall semantic analysis of a syntax tree.

Due to their chained nature, they can ask a parent binder for a symbol if they cannot find it in their own scope. This allows for a hierarchical resolution of symbols

Here are some of the key binder types:

* `GlobalBinder` is the top-level binder that represents the entire compilation. It is responsible for binding global symbols, such as namespaces and types, and serves as the entry point for symbol resolution.

* `BlockBinder` is a specific type of binder that is responsible for binding local variables and parameters within a method or block of code. It uses the `SemanticModel` to resolve symbols and check for any potential conflicts or errors.