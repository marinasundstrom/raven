using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class SyntaxNode
{
    internal readonly GreenNode Green;
    private readonly SyntaxTree _syntaxTree;
    private readonly SyntaxNode _parent;

    public SyntaxNode Parent => _parent;

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

    public SyntaxKind Kind => Green.Kind;

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

    public IEnumerable<DiagnosticInfo> Diagnostics => Green.Diagnostics;

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

            foreach (var descendant in child.DescendantNodes())
            {
                yield return descendant;
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
            node = (TNode)slot.CreateRed(this, position);
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

    internal SyntaxNode ReplaceTokenInListCore(SyntaxToken tokenInList, IEnumerable<SyntaxToken> newTokens)
    {
        //ar newGreen = Green.ReplaceToken(tokenInList, newTokens)
        return this;
    }

    public abstract void Accept(SyntaxVisitor visitor);

    public abstract TResult Accept<TResult>(SyntaxVisitor<TResult> visitor);
}
