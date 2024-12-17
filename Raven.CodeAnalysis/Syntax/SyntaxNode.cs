using System.Diagnostics;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class SyntaxNode : IEquatable<SyntaxNode>
{
    internal readonly GreenNode Green;
    private readonly SyntaxTree _syntaxTree;
    private readonly SyntaxNode _parent;

    public SyntaxNode Parent => _parent;

    public bool IsMissing { get; private set; }

    public SyntaxTree? SyntaxTree
    {
        get
        {
            if (_syntaxTree is not null)
            {
                return _syntaxTree;
            }

            if (_parent is not null)
            {
                return _parent.SyntaxTree;
            }

            return null;
        }
    }

    public virtual SyntaxKind Kind => Green.Kind;

    public int Position { get; }

    public int Width => Green.Width;

    public int FullWidth => Green.FullWidth;

    public TextSpan Span
    {
        get
        {
            var start = Position + LeadingTrivia.Width;
            return new TextSpan(start, Width);
        }
    }

    public TextSpan FullSpan
    {
        get
        {
            return new TextSpan(Position, FullWidth);
        }
    }

    /// <summary>
    /// Gets a list of the child nodes in prefix document order.
    /// </summary>
    public IEnumerable<SyntaxNode> ChildNodes()
    {
        foreach (var nodeOrToken in this.ChildNodesAndTokens())
        {
            if (nodeOrToken.AsNode(out var node))
            {
                yield return node;
            }
        }
    }

    public ChildSyntaxList ChildNodesAndTokens()
    {
        return new ChildSyntaxList(this);
    }

    public SyntaxTriviaList LeadingTrivia => GetFirstToken().LeadingTrivia;

    public SyntaxTriviaList TrailingTrivia => GetLastToken().TrailingTrivia;

    public SyntaxToken GetFirstToken(bool includeZeroWidth = false)
    {
        return (SyntaxToken)this.Green.GetFirstTerminal();
    }

    public SyntaxToken GetLastToken(bool includeZeroWidth = false)
    {
        return (SyntaxToken)this.Green.GetLastTerminal();
    }

    public SyntaxNode(GreenNode greenNode, SyntaxNode parent, int position = 0)
    {
        Green = greenNode ?? throw new ArgumentNullException(nameof(greenNode));
        _parent = parent;
        Position = position;
    }

    public SyntaxNode(GreenNode greenNode, SyntaxTree syntaxTree)
    {
        Green = greenNode ?? throw new ArgumentNullException(nameof(greenNode));
        _syntaxTree = syntaxTree;
    }

    /// <summary>
    /// Retrieves all descendant nodes recursively.
    /// </summary>
    public IEnumerable<SyntaxNode> DescendantNodes()
    {
        foreach (var child in ChildNodes())
        {
            yield return child;

            if (child is not null)
            {
                foreach (var descendant in child.DescendantNodes())
                {
                    yield return descendant;
                }
            }
        }
    }

    /// <summary>
    /// Retrieves all ancestor nodes up to the root.
    /// </summary>
    public IEnumerable<SyntaxNode> AncestorNodes()
    {
        var current = _parent;
        while (current != null)
        {
            yield return current;
            current = current._parent;
        }
    }

    private string GetDebuggerDisplay()
    {
        return GetType().Name;
    }

    internal virtual SyntaxNode? GetNodeSlot(int index) { return null; }

    public virtual TNode GetRed<TNode>(ref TNode node, int index)
        where TNode : Syntax.SyntaxNode
    {
        if (node is not null)
        {
            return node;
        }

        var slot = this.Green.GetSlot(index);
        if (slot is not null)
        {
            var position = Position + Green.GetChildStartPosition(index);

            node = (TNode)SyntaxNodeCache.GetValue(slot, (s) =>
            {
                return s.CreateRed(this, position);
            });
            return node;
        }

        return null!;
    }

    public override string ToString()
    {
        return SourceTextWriter.WriteNodeToText(this, false);
    }

    public string ToFullString()
    {
        return SourceTextWriter.WriteNodeToText(this, true);
    }

    public SyntaxNode ReplaceToken(SyntaxToken oldToken, SyntaxToken newToken)
    {
        // Step 1: Traverse and locate the node to replace in the green tree
        var greenToReplace = oldToken.Green;
        var newGreen = this.Green.ReplaceNode(greenToReplace, newToken.Green);

        // Step 2: Rebuild the red tree with the updated green node
        return newGreen.CreateRed(this.Parent, this.Position);
    }

    public SyntaxNode ReplaceNode(SyntaxNode oldNode, SyntaxNode newNode)
    {
        if (oldNode == null)
            throw new ArgumentNullException(nameof(oldNode));

        if (newNode == null)
            throw new ArgumentNullException(nameof(newNode));

        if (newNode.Parent is not null)
            throw new ArgumentException(nameof(newNode), "Syntax node is not part of the current syntax tree.");

        // Step 1: Traverse and locate the node to replace in the green tree
        var greenToReplace = oldNode.Green;
        var newGreen = this.Green.ReplaceNode(greenToReplace, newNode.Green);

        // Step 2: Rebuild the red tree with the updated green node
        return newGreen.CreateRed(this.Parent, this.Position);
    }

    public abstract void Accept(SyntaxVisitor visitor);

    public abstract TResult Accept<TResult>(SyntaxVisitor<TResult> visitor);

    public static bool operator ==(SyntaxNode left, SyntaxNode? right) => Equals(left, right);

    public static bool operator !=(SyntaxNode left, SyntaxNode? right) => !Equals(left, right);

    public bool Equals(SyntaxNode? other)
    {
        if (other is null)
        {
            return false;
        }

        if (ReferenceEquals(this, other))
        {
            return true;
        }

        return Green.Equals(other.Green) && ((Object)_parent).Equals(other._parent);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
        {
            return false;
        }

        if (ReferenceEquals(this, obj))
        {
            return true;
        }

        if (obj.GetType() != GetType())
        {
            return false;
        }

        return Equals((SyntaxNode)obj);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Green, _syntaxTree, _parent);
    }
}