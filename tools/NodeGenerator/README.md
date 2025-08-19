# Node generator

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
* **Slots** - the slots containing tokens or nodes.


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