# Syntax tree

The basic architecture of the syntax tree is based around a "Green" tree (internal tree) and a "Red" tree (external tree).

The purpose is to maximize re-use of nodes while keeping the tree as an immutable structure.

## Internal tree (Green tree)

The basic information is stored in the so called "green" tree.

The green tree keeps track of nodes and their children. It knows the width of the nodes - with or without leading and trailing trivia.

Due to their immutability and structure, nodes can be re-used.

### Green node

This is the basic element in the tree. It acts as both a non-terminal, and a terminal.

Green nodes are reference types.

There are two derived types: `SyntaxToken` and `SyntaxNode`. The latter (`SyntaxNode`) has derived types for each syntax.

Child nodes reside in `slots`.

The green tree is not exactly a one to one mapping of the external tree (red tree), but pretty close. Because of encoding in the internal tree being a bit more basic.

The `GreenNode` looks something like this:

```csharp
public class GreenNode 
{
    public string? Value { get; }

    public SyntaxKind Kind { get; }

    public int Width { get; }

    public int SlotCount { get; }

    public GreenNode? GetSlot(int index);

    // Omitting members such as LeadingTrivia and TrailingTrivia and FullWidth

    // There is also a method for creating the equivalent Red node wrapper.
}
```

#### Kind

The kind of a node is indicated by the `SyntaxKind` enum.

#### Slots

Child nodes (and tokens) are held in what is referred to as `slots`

These slots can be `null` if the node permits so for a child.

#### Width and Full Width

When calculating the Width and Full Width or a node, we simply sum up the Width, or Full Width, of the nodes held in the child slots.

#### Syntax token

The `SyntaxToken` "node" is a non-terminal that represents the most basic element of code, a token, that has a value represented in text. 

It might be of any of the `SyntaxKind` available for tokens; such as identifier, keyword, number literal, trivia, whitespace, etc.

Since, syntax tokens are terminals, they don't have slots. But they do have leading and trailing trivia.

#### Trivia

Trivia is what normally don't have any meaning in interpreting your program. Like whitespaces, newlines, and even comments. 

They are attached to tokens, either before (leading) or after (trailing.)

#### Lists

Syntax lists are special nodes that can keep multiple nodes for its parent. Their purpose is to allow the parent type to hold a variable number of child nodes.

The parent of a list item is the parent syntax node of the list, and not the list.

There are: Normal syntax lists and separated syntax lists (having separator tokens)

An example of usage of lists is the `BlockSyntax`(statement) having multiple statements that have the block as its parent. 

### External tree (Red tree)

This is the public API of the compiler.

This is essentially a light wrapper on top of the "green" tree that adds some more information.

The main difference from the internal tree is that we keep track of the parent and the span (and full span) of each node.

Just like the internal tree, the nodes are immutable. Modifying or connecting nodes create new nodes, but retain information internally through "green nodes" when possible.

#### Syntax Token

Wraps in a Green node syntax token in a struct.

#### Syntax Node

Wraps non-terminal Green nodes. And there is a class hierarchy corresponding to the one used in the internal tree. 

#### Lists

There are corresponding types for syntax lists and separated syntax lists.

#### Resolving child nodes

Each red node is responsible for wrapping its green node's child nodes (green nodes) and keep a reference to the wrapper object (red node).

Syntax tokens and lists are not stored but created on demand.

There could be some caching of red nodes later. But there currently is none.

#### Generating red nodes

The implementation of the red nodes is a repetitive and tedious process. But it is possible to automate it since every implementation is essentially the same.

Every navigation property in a syntax node has to call the same methods for resolving the red node from a green child node.

The C# source generator handles this.

For the generator to work, each syntax node has to be a `partial class` extending either `SyntaxNode`, or a class that derive from `SyntaxNode`. 

Subsequently, for a property to be implemented by the source generator it has to have a valid `SyntaxNode`, `SyntaxToken`, `SyntaxList` type. And it also has to be declared as `partial` and with a `get` accessor.

## Example implementation of a node

Take this as an example of a "green" node (that has been manually implemented):

```csharp
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public NamespaceDeclarationSyntax(
        SyntaxToken namespaceKeyword,
        IdentifierNameSyntax name,
        SyntaxToken openBraceToken,
        SyntaxList imports,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.CompilationUnit,
              [
                    namespaceKeyword,
                    name,
                    openBraceToken,
                    imports,
                    members,
                    closeBraceToken,
                    semicolonToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.NamespaceDeclarationSyntax(this, parent, position);
    }
}
```

Here is the "red" node for which implementation will be generated:

```csharp
namespace Raven.CodeAnalysis.Syntax;

public partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public partial SyntaxToken NamespaceKeyword { get; }

    public partial IdentifierNameSyntax Name { get; }

    public partial SyntaxToken OpenBraceToken { get; }

    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken CloseBraceToken { get; }

    public partial SyntaxToken? SemicolonToken { get; }

    public NamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxToken namespaceKeyword, IdentifierNameSyntax name,
        SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, openBraceToken.Green, imports?.Green, members?.Green, closeBraceToken.Green, SyntaxFactory.SemicolonToken.Green), (SyntaxNode)null)
    {
    }

    public NamespaceDeclarationSyntax(IdentifierNameSyntax name,
        SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, SyntaxFactory.OpenBraceToken.Green, imports?.Green, members?.Green, SyntaxFactory.CloseBraceToken.Green, SyntaxFactory.SemicolonToken.Green), (SyntaxNode)null)
    {
    }

    public NamespaceDeclarationSyntax WithImports(SyntaxList<ImportDirectiveSyntax> imports)
    {
        return new NamespaceDeclarationSyntax(NamespaceKeyword, Name, OpenBraceToken, imports, Members, CloseBraceToken);
    }

    public NamespaceDeclarationSyntax WithMembers(SyntaxList<MemberDeclarationSyntax> members)
    {
        return new NamespaceDeclarationSyntax(NamespaceKeyword, Name, OpenBraceToken, Imports, members, CloseBraceToken);
    }
}
```

### Generated code

With resulting generated code that will be merged with the original definition at compile time:

```csharp
using System;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax
{
    public partial class NamespaceDeclarationSyntax
    {
        internal Raven.CodeAnalysis.Syntax.IdentifierNameSyntax _name;

        public partial Raven.CodeAnalysis.Syntax.SyntaxToken NamespaceKeyword => new SyntaxToken(Green.GetSlot(0) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this, Position + Green.GetChildStartPosition(0));
        public partial Raven.CodeAnalysis.Syntax.IdentifierNameSyntax Name => (Raven.CodeAnalysis.Syntax.IdentifierNameSyntax)GetNodeSlot(1);
        public partial Raven.CodeAnalysis.Syntax.SyntaxToken OpenBraceToken => new SyntaxToken(Green.GetSlot(2) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this, Position + Green.GetChildStartPosition(2));

        public partial Raven.CodeAnalysis.Syntax.SyntaxList<Raven.CodeAnalysis.Syntax.ImportDirectiveSyntax> Imports
        {
            get
            {
                GreenNode green = Green.GetSlot(3);
                if (green is null)
                {
                    return default(Raven.CodeAnalysis.Syntax.SyntaxList<Raven.CodeAnalysis.Syntax.ImportDirectiveSyntax>);
                }

                return new Raven.CodeAnalysis.Syntax.SyntaxList<Raven.CodeAnalysis.Syntax.ImportDirectiveSyntax>(green as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxList, this, Position + Green.GetChildStartPosition(3));
            }
        }

        public partial Raven.CodeAnalysis.Syntax.SyntaxList<Raven.CodeAnalysis.Syntax.MemberDeclarationSyntax> Members
        {
            get
            {
                GreenNode green = Green.GetSlot(4);
                if (green is null)
                {
                    return default(Raven.CodeAnalysis.Syntax.SyntaxList<Raven.CodeAnalysis.Syntax.MemberDeclarationSyntax>);
                }

                return new Raven.CodeAnalysis.Syntax.SyntaxList<Raven.CodeAnalysis.Syntax.MemberDeclarationSyntax>(green as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxList, this, Position + Green.GetChildStartPosition(4));
            }
        }

        public partial Raven.CodeAnalysis.Syntax.SyntaxToken CloseBraceToken => new SyntaxToken(Green.GetSlot(5) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this, Position + Green.GetChildStartPosition(5));

        public partial Raven.CodeAnalysis.Syntax.SyntaxToken? SemicolonToken => new SyntaxToken(Green.GetSlot(6) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this, Position + Green.GetChildStartPosition(6));

        internal override SyntaxNode? GetNodeSlot(int index)
        {
            return index switch
            {
                1 => this.GetRed(ref _name, 1),
                _ => throw new Exception()};
        }
    }
}
```