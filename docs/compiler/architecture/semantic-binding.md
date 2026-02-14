# Semantic binding

Semantic binding is the process of identifying meaningful parts in your program.

This article is covering the following concepts:

* Binders
* Bound tree

## Binders

A `Binder` is a specialized class responsible for resolving symbols in a particular scope. It binds syntax to symbols.

Binders are an implementation detail of the `SemanticModel`, which is responsible for the overall semantic analysis of a syntax tree - resulting in Bound Nodes, a so called Bound Tree.

Due to their chained nature, they can ask a parent binder for a symbol if they cannot find it in their own scope. This allows for a hierarchical resolution of symbols

Binders are chained together so that each scope can fall back to its parent. The compiler creates them lazily as semantic analysis walks the syntax tree. The table below lists the main binder kinds, their responsibilities, and when they come into play.

| Binder | Responsibility | Created When | Parent |
| --- | --- | --- | --- |
| `GlobalBinder` | Root binder for the entire compilation and entry point for symbol lookup. | Once per compilation. | – |
| `NamespaceBinder` | Tracks the current namespace and collects declared types. | Entering a namespace declaration or the global namespace. | `GlobalBinder` or another `NamespaceBinder` |
| `ImportBinder` | Applies `import` directives and alias declarations. | After a `NamespaceBinder` is created for the file. | `NamespaceBinder` |
| `CompilationUnitBinder` | Associates a syntax tree with its `SemanticModel`. | For each compilation unit. | `ImportBinder` |
| `TopLevelBinder` | Binds global statements and synthesizes the entry point. | When a file contains top‑level statements. | `ImportBinder` |
| `TypeDeclarationBinder` | Base binder for type declarations, establishing member scope. | For each type (class, enum, etc.). | `ImportBinder` or containing `TypeDeclarationBinder` |
| `TypeMemberBinder` | Base binder for members declared in a type. | When binding a member. | `TypeDeclarationBinder` |
| `MethodBinder` | Provides parameter symbols and method context. | Entering a method or function declaration. | `TypeMemberBinder` |
| `MethodBodyBinder` | Binds statements and locals inside a method body. | When binding a method's block. | `MethodBinder` |
| `BlockBinder` | Represents a block scope. | Encountering a `{}` block. | `MethodBinder` or another `BlockBinder` |
| `LocalScopeBinder` | Tracks nested scopes such as `if`, `while`, or `for` statements. | Entering a nested statement scope. | `BlockBinder` |
| `LambdaBinder` | Binds lambda expressions. | Encountering a lambda expression. | `BlockBinder` or `MethodBinder` |
| `FunctionBinder` | Binds function declarations inside other functions. | Encountering a function. | `BlockBinder` |

### Typical order
From the outermost scope inward, binders typically appear in the following order:

`GlobalBinder → NamespaceBinder → ImportBinder → CompilationUnitBinder → [TopLevelBinder] → TypeDeclarationBinder → MethodBinder → MethodBodyBinder → BlockBinder → LocalScopeBinder`

### Visualizing binder hierarchy
For debugging, a semantic model instance can print the active binder chain with `semanticModel.PrintBinderTree();`.
The method produces a textual tree that shows how binders are nested for the current syntax tree.

## Bound tree

The semantic analysis of a syntax tree results in a Bound tree. It's the binders that create bound nodes.

The Bound tree contains Bound nodes that represent meaning in the program, such as expressions and their bound symbols an child expressions. This tree is not tied to the syntax, but the units of meaning.

The code generator is using this bound tree when generating the actual IL (bytecode).

### Match binding split

Pattern matching uses separate bound nodes for expression and statement forms:

* `BindMatchExpression` produces `BoundMatchExpression` for value-producing
  contexts.
* `BindMatchStatement` produces `BoundMatchStatement` for statement control-flow
  contexts.

Both paths share arm binding, pattern validation, and exhaustiveness checks so
diagnostics stay consistent between forms.

## Bound tree walkers

Analyses that operate on bound nodes use `BoundTreeWalker`, a base class that provides
virtual `Visit*` methods for the bound expressions and statements.  Subclasses can
override these methods to traverse or gather information without modifying the bound
tree.

### Return type inference

`ReturnTypeCollector` is a specialized `BoundTreeWalker` used by the binder to infer
return types for blocks and lambdas when no explicit return type is declared. It
collects the types from all explicit `return` statements and the outer body tail
expression (when present). Expression statements inside nested statement blocks do
not contribute implicit return types for the enclosing member. The collected types
are normalized before inference completes: nested unions flatten, literal members
collapse into their underlying type when a broader branch is present, and a `null`
branch paired with a single non-nullable type becomes that type's nullable form.
When multiple distinct types remain, the collector produces a union; otherwise the
single type is returned. The inferred (and normalized) result is used as the
default type when assigning the result to a declaration without an explicit annotation.
