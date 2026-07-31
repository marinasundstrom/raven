# Architecture
 
## Design goals

Build with "Compiler as a service" in mind. Giving the consumer a great experience through a public API.

For the current live-editing architecture direction, see
[Live semantic model](live-semantic-model.md). That document is the canonical
guide for binder-owned semantic state, incremental snapshots, analyzer
diagnostic lanes, and language-server scheduling.

Before porting compiler components to Raven, use
[Syntactic and semantic stabilization](syntactic-and-semantic-stabilization.md)
as the evidence-based stabilization plan and behavioral exit gate.

Use [.NET conformance, Raven divergences, and emitted
IL](dotnet-conformance-and-divergence.md) when deciding whether Raven should
follow an established C#/.NET convention, retain a Raven-native semantic rule,
or improve a noncanonical lowering shape.

## Raven-first development and bootstrap boundaries

Raven increasingly builds its own language-facing infrastructure. New
libraries, macros, tools, samples, and compiler API examples should be authored
in Raven when the current language and toolchain can support them reliably.
This is both a product direction and a stabilization practice: real Raven
infrastructure continuously exercises public APIs and incremental build/editor
paths.

The dependency layers should remain explicit:

1. `Raven.CodeAnalysis` is the current C# compiler and public compiler API.
2. `Raven.Compiler` and the bootstrap host load the compiler and establish the
   CLR/MSBuild boundary.
3. `Raven.Core` contains foundational Raven-authored runtime APIs without a
   direct application dependency on compiler services.
4. `Raven.Macros` is a version-matched Raven-authored compiler-plugin library;
   authoring and executing macros legitimately crosses into
   `Raven.CodeAnalysis`.
5. Higher-level tools and infrastructure should consume those public layers
   from Raven rather than reaching into compiler internals.

C# remains intentional where it breaks bootstrap cycles, implements the
compiler core, or demonstrates a C# host integrating Raven. It should not be
the automatic choice for new infrastructure. Missing Raven capabilities found
while dogfooding should be treated as design and stabilization input. This
layered approach permits gradual self-hosting without requiring an all-at-once
compiler rewrite.

## Abstract Syntax Tree (AST)

The syntax tree is Immutable. Modification of one node creates a new node, and potentially a new syntax tree. This allows for maximum re-use of nodes. Other than guaranteeing that the current syntax tree is not tampered with, this also allows for versioning of syntax trees. It further enables things like incremental compilation.

A syntax tree is composed of two kinds of elements.

### Tokens

Tokens are the basic element of your source code: identifiers, keywords, operators, and other symbols.

They come together in syntax nodes to create elements that have meaning within the source code.

### Nodes

A syntax node is a non-terminal that may have one or more children. There is also a reference to the parent node.

There are multiple derived node types representing the different elements of the source code, such as `MethodDeclarationSyntax` and `IfStatementSyntax`.

#### Syntax lists

A node may have multiple child nodes of the same type. A block may have multiple statements in a list of statement nodes.

Lists aren't themselves nodes. The "child nodes" are children of the parent node.

### Internal tree

As an implementation detail there is an optimized internal tree that holds the actual information node and token. These nodes can be re-used during the compilation.

In "Roslyn" parlance, this is referred to as the "Green tree", or "Green nodes". While the outer tree is called the "Red tree".

The "Green tree" holds the information about what children (nodes and tokens) a node has. But it doesn't know about the parent as that might change during "modification", or rather "non-destructive mutation", of the syntax tree.

### Generator

Parts of the Syntax nodes in the outer syntax tree, or API, is repetitive in nature, such as properties for nodes a tokens. So we use a source generator to generate these properties. 

Provided that a Syntax node class is a partial class and its properties of type `SyntaxToken` and `SyntaxNode` are partial, the generator produces the implementation for you when building.

This is what Roslyn also does, but they use T4 templates instead.

## Tokenizer

The tokenizer divides the source code into tokens.

## Parser

The parser reads the tokens and produces syntax according to the rules of the language.

**Note:**

Expression parser logic can be taken from [ExpressionEvaluator](https://github.com/marinasundstrom/ExpressionEvaluator). This is a [Operator-precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser), originally based on the IronPython source code (in C#),

## Semantic model

The semantic model holds information about the semantics of the program. Like the meaning of identifiers, whether they are variables or methods etc. It also deals with external symbols. All to validate the program.
