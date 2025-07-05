# Generator specification

**This documentation is outdated**: Nodes are created from a type specification: `Model.yml`.

The generators scan for matching type declarations, and create partial declarations that merge with the original definition, adding members and implementation.

Details below.

## Overview

**SyntaxNodes** (Red)
* Properties for resolving child syntax (Nodes, Tokens, and Lists)
* `With` and `Update` methods
* `Accept` methods
* Visitor and Rewriter

**Internal SyntaxNodes** (Green)
* "Default" constructor (private)
* `CreateRed`
* `WithUpdatedChildren` method
* `Accept` methods
* Visitor and Rewriter

**Symbols**
* `With` and `Update` methods
* `Accept` methods
* Visitor

**Bound Tree nodes**
* `Update` method
* `Accept` methods
* Visitor and Rewriter

## Visitor

This is the same for every visitor type, whether for SyntaxNode, Symbol, or BoundNode.

### `Accept` methods

These methods are generated for the element type - here it is for a `SyntaxNode` type, and using `SyntaxVisitor`.

Standard visitor:

```csharp
public override void Accept(SyntaxVisitor visitor)
{
    visitor.VisitNamespaceDeclaration(this);
}
```

Generic version - mainly used by rewriters.

```csharp
public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
{
    return visitor.VisitNamespaceDeclaration(this);
}
```

### `Visit*` method on `Visitor` and `Visitor<T>`

In `Visitor`:

```csharp
public virtual void VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
{
    DefaultVisit(node);
}
```

Generic version in `Visitor<T>` - mainly used as a based by rewriters.

```csharp
public virtual TResult VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
{
    return DefaultVisit(node);
}
```

## Syntax Tree (Red)

### Properties

These properties are meant to lazily resolve Red versions of the nodes in the slots of the underlying Green node.

The properties that are implemented by the generator has to be marked as `partial`.

#### Sample: SyntaxNode

The sample below demonstrates SyntaxNode definition from which members are being implemented through source generation, as shown below.

```csharp
namespace Raven.CodeAnalysis.Syntax;

public partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public partial SyntaxToken NamespaceKeyword { get; }

    public override partial NameSyntax Name { get; }

    public partial SyntaxToken OpenBraceToken { get; }

    public override partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken CloseBraceToken { get; }

    public partial SyntaxToken? SemicolonToken { get; }

    internal NamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxToken namespaceKeyword, NameSyntax name,
        SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? semicolonToken = null)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(namespaceKeyword.Green, (InternalSyntax.NameSyntax)name.Green, openBraceToken.Green, imports.Green, members.Green, closeBraceToken.Green, semicolonToken?.Green), (SyntaxNode)null)
    {
    }

    public NamespaceDeclarationSyntax(NameSyntax name,
        SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.NameSyntax)name.Green, SyntaxFactory.OpenBraceToken.Green, imports.Green, members.Green, SyntaxFactory.CloseBraceToken.Green, null), (SyntaxNode)null)
    {
    }
}
```

#### SyntaxNode property

```csharp
public override partial NameSyntax Name { get; }
```

Implemented as:

```csharp
public override NameSyntax Name => (NameSyntax)GetNodeSlot(1);
```

#### SyntaxToken property

```csharp
public partial SyntaxToken OpenBraceToken { get; }
```

Implemented as:

```csharp
public SyntaxToken OpenBraceToken => new SyntaxToken(Green.GetSlot(2) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this, base.Position + Green.GetChildStartPosition(2));
```

#### SyntaxList property

```csharp
public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }
```

Implemented as:

```csharp
public override SyntaxList<MemberDeclarationSyntax> Members
{
    get
    {
        GreenNode green = Green.GetSlot(4);
        if (green == null)
        {
            return default(SyntaxList<MemberDeclarationSyntax>);
        }
        return new SyntaxList<MemberDeclarationSyntax>(green as SyntaxList, this, base.Position + Green.GetChildStartPosition(4));
    }
}
```

### `With*` methods

There is a `With` method for every syntax property whose purpose is to create a copy of a node with the property having been updated with the specified value. This is non-destructive mutation.

Behind the scenes, the `With` method calls the `Update` method, which makes sure that a new node is created only when the value of the specified property has changed.

Example: `EnumDeclarationSyntax`

```csharp
public EnumDeclarationSyntax WithIdentifier(SyntaxToken identifier)
{
    return Update(EnumKeyword, identifier, OpenBraceToken, Members, CloseBraceToken, SemicolonToken);
}
```

### `Update` method

This method is used both by `With` methods and the Rewriter.

Example: `EnumDeclarationSyntax`

```csharp
public EnumDeclarationSyntax Update(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? semicolonToken)
{
    if (EnumKeyword != enumKeyword || Identifier != identifier || OpenBraceToken != openBraceToken || Members != (SeparatedSyntaxList<EnumMemberDeclarationSyntax>?)members || CloseBraceToken != closeBraceToken || SemicolonToken != semicolonToken)
    {
        return new EnumDeclarationSyntax(enumKeyword, identifier, openBraceToken, members, closeBraceToken, semicolonToken);
    }
    return this;
}
```

### Rewriter `Visit` method

The rewriter method for `EnumDeclarationSyntax` looks like this:

```csharp
    public abstract partial class SyntaxRewriter : SyntaxVisitor<SyntaxNode?>
    {
        public override SyntaxNode? VisitEnumDeclaration(EnumDeclarationSyntax node) => node?.Update((Raven.CodeAnalysis.Syntax.SyntaxToken)VisitToken(node.EnumKeyword), (Raven.CodeAnalysis.Syntax.SyntaxToken)VisitToken(node.Identifier), (Raven.CodeAnalysis.Syntax.SyntaxToken)VisitToken(node.OpenBraceToken), (Raven.CodeAnalysis.Syntax.SeparatedSyntaxList<Raven.CodeAnalysis.Syntax.EnumMemberDeclarationSyntax>)VisitList<Raven.CodeAnalysis.Syntax.EnumMemberDeclarationSyntax>(node.Members), (Raven.CodeAnalysis.Syntax.SyntaxToken)VisitToken(node.CloseBraceToken), (Raven.CodeAnalysis.Syntax.SyntaxToken? )VisitToken(node.SemicolonToken));
    }
```

## Internal Syntax Tree (Green)

For the defined internal node:

```csharp
internal partial class ArgumentListSyntax : SyntaxNode
{
    public ArgumentListSyntax(
        SyntaxToken openParenToken,
        SyntaxList arguments,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ArgumentList,
              [
                      openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                      arguments ?? throw new ArgumentNullException(nameof(arguments)),
                      closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken))
              ],
              diagnostics)
    {
    }
}
```

This is being generated:

```csharp
internal partial class ArgumentListSyntax
{
    private ArgumentListSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ArgumentListSyntax(this, parent, position);
    }

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return new ArgumentListSyntax(Kind, newChildren);
    }

    internal override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitArgumentList(this);
    }

    internal override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitArgumentList(this);
    }
}

internal abstract partial class SyntaxVisitor
{
    public virtual void VisitArgumentList(ArgumentListSyntax node)
    {
        DefaultVisit(node);
    }
}

internal abstract partial class SyntaxVisitor<TResult>
{
    public virtual TResult VisitArgumentList(ArgumentListSyntax node)
    {
        return DefaultVisit(node);
    }
}
```

## BoundTree

### `Update` method

Parameters to `Update` are based on the constructor parameters for a specific concrete derivative of `BoundNode`.

```csharp
public BoundIndexerAccessExpression Update(BoundExpression receiver, System.Collections.Generic.IEnumerable<BoundExpression> arguments, IPropertySymbol indexer, BoundExpressionReason reason)
{
    if (Receiver != receiver || Arguments != arguments || Indexer != indexer || Reason != reson)
    {
        return new BoundIndexerAccessExpression(receiver, arguments, indexer, reason);
    }
    return this;
}
```

### Rewriter `Visit` method

In each `Visit` method, calls to `Update`, `Visit` (or `VisitList`) calls are emitted for parameters of types derived from `BoundNode`, or implementing `ISymbol`, or a collection of those. Parameters of other types will just be passed directly as-is to `Update`.

```csharp
public override BoundNode? VisitIndexerAccessExpression(BoundIndexerAccessExpression node)
{
    return node?.Update(VisitExpression(node.Receiver), VisitList(node.Arguments), (IPropertySymbol)VisitSymbol(node.Indexer), node.Reason);
}
```