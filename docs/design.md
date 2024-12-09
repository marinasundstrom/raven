# Design
 
## Design goals

Build with "Compiler as a service" in mind. Giving the consumer a great experience through a public API.

## Abstract Syntax Tree (AST)

The syntax tree is Immutable. Modification of one node creates a new node, and potentially a new syntax tree. This allows for maximum re-use of nodes. Other than guaranteeing that the current syntax tree is not tampered with, this also allows for versioning of syntax trees. It further enables things like incremental compilation.

A syntax tree is composed of two kinds of elements.

### Tokens

Tokens are the basic element of your source code: identifiers, keywords, operators, and other symbols.

They come together in syntax nodes to create elements that have meaning within the source code.

### Nodes

A syntax node is a non-terminal that may have one or more children. There is also a reference to the parent node.

There are multiple derived node types representing the different elements of the source code, such as `MethodDeclarationSyntax` and `IfStatementSyntax`.

### Internal tree

As an implementation detail there is an optimized internal tree that holds the actual information node and token. These nodes can be re-used during the compilation.

In "Roslyn" parlance, this is referred to as the "Green tree", or "Green nodes". While the outer tree is called the "Red tree".

The "Green tree" holds the information about what children (nodes and tokens) a node has. But it doesn't know about the parent as that might change during "modification", or rather "non-destructive mutation", of the syntax tree.

### Generator

Parts of the Syntax nodes in the outer syntax tree, or API, is repetitive in nature, such as properties for nodes a tokens. So we use a source generator to generate these properties. 

Provided that a Syntax node class is a partial class and its properties of type `SyntaxToken` and `SyntaxNode` are partial, the generator produces the implementation for you when building.

This is what Roslyn also does, but they use T4 templates instead.

## Semantic model

The semantic model holds information about the semantics of the program. Like the meaning of identifiers, whether they are variables or methods etc. It also deals with external symbols. All to validate the program.