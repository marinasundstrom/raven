# Node generator

The maintained C# generator remains the production entry point. The
[parallel Raven library](../NodeGenerator.Raven/README.md) ports syntax-kind,
node-facts, token, green-node/factory, visitor and rewriter emitters with identical
output modes. Run
`scripts/test-generator-parity.sh` from the repository root to validate both
implementations. Red-node emission, model loading and CLI orchestration
are not yet part of that port.

## XML format specification

This section outlines how to interpret the XML format.

The actual rules are in the source code.

Nodes and TokenKind definitions are collectively referred to as "SyntaxKind".


### Node

> Defined in Model.xml

Represents a syntax node type.

**Attributes**

* **Name** - the name of the node type
* **Inherits** - the node type which the current one inherits from. This is the *base node*.
* **IsAbstract** - indicates whether the node is abstract, and not instantiable. They don't have their syntax kind.
* **HasExplicitKind** - indicates whether the node requires you to set explicit kind. Which is useful for node types that might represent multiple syntaxes.
* **PermittedType** - adds an out-of-model direct subtype to the generated closed-hierarchy metadata for an abstract node. Direct model subtypes are included automatically.
* **Slots** - the slots containing tokens or nodes.

### Hierarchy

> Defined in Model.xml

Adds closed-hierarchy metadata to a public abstract syntax type declared outside
the generated node model. `ModelBase` includes direct model children and
`PermittedType` lists handwritten direct children.


### Slot

> Defined in Model.xml, under Node.

Represents a slot where a node or token lives.

> If nullable, they are optional. But even non-nullable, might contain tokens of syntax kind `None`, It's up to the parser to determine if that is valid in that particular instance.

**Attributes**

* **Name** - the name of the slot.
* **Type** - the type of node or token stored in the slot.
* **ElementType** - if the *Type* is a list kind, this will indicate the type of objects stored. But not for TokenList.
* **IsNullable** - indicates whether the slot is nullable.
* **IsInherited** - indicates whether the slot is inherited from a base node.
* **IsAbstract** - indicates whether the slot is abstract, and should not be implemented in this node type.

## Token

> Defined in Tokens.xml

The supported kinds of tokens and their characteristics. Used when generating factories and Syntax Kind.

## NodeKind

> Defined in NodeKinds.xml

These are node kinds that don't have their own syntax nodes. The kind is instead explicitly passed into the node type specified as attribute `Type`.

### Raven syntax facts during compiler porting

From `src/Raven.CodeAnalysis/Syntax`, run:

```sh
dotnet run --project ../../../tools/NodeGenerator -- --raven-syntax-facts <output-directory>
```

This writes `SyntaxFacts.Tokens.g.rvn` and `SyntaxFacts.Nodes.g.rvn` from the same
XML inputs used for C#. It preserves keyword and reserved-word lookup, token text,
operator precedence and node classification. The Raven compiler project tracks
these inputs and includes the generated files on a first build as well as on
incremental builds. This mode does not overwrite C# outputs or their stamp.
Retain the ordinary C# generation mode and validate both outputs when changing
shared models or emitters. The generator itself remains a C# bootstrap tool.
