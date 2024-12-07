using System.Diagnostics;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class SyntaxNode
{
    internal readonly GreenNode Green;
    private readonly SyntaxTree _syntaxTree;
    private readonly SyntaxNode _parent;

    public SyntaxNode Parent => _parent;
    public SyntaxTree SyntaxTree => _syntaxTree ?? _parent.SyntaxTree;

    public SyntaxKind Kind => Green.Kind;
    public int FullWidth => Green.FullWidth;
    public int StartPosition => Green.StartPosition;
    public int EndPosition => Green.EndPosition;
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

    public SyntaxTriviaList LeadingTrivia => GetFirstToken(this).LeadingTrivia;

    public SyntaxTriviaList TrailingTrivia => GetLastToken(this).TrailingTrivia;

    private static SyntaxToken GetFirstToken(SyntaxNode syntaxNode)
    {
        return (SyntaxToken)syntaxNode.Green.GetFirstTerminal();
    }

    private static SyntaxToken GetLastToken(SyntaxNode syntaxNode)
    {
        return (SyntaxToken)syntaxNode.Green.GetLastTerminal();
    }

    public SyntaxNode(GreenNode greenNode, SyntaxNode parent)
    {
        Green = greenNode ?? throw new ArgumentNullException(nameof(greenNode));
        _parent = parent;
    }

    public SyntaxNode(GreenNode greenNode, SyntaxTree syntaxTree)
    {
        Green = greenNode ?? throw new ArgumentNullException(nameof(greenNode));
        _syntaxTree = syntaxTree;
    }

    /// <summary>
    /// Retrieves all descendant nodes recursively.
    /// </summary>
    public IEnumerable<SyntaxNode> Descendants()
    {
        foreach (var child in ChildNodes())
        {
            yield return child;

            foreach (var descendant in child.Descendants())
            {
                yield return descendant;
            }
        }
    }

    /// <summary>
    /// Retrieves all ancestor nodes up to the root.
    /// </summary>
    public IEnumerable<SyntaxNode> Ancestors()
    {
        var current = _parent;
        while (current != null)
        {
            yield return current;
            current = current._parent;
        }
    }

    public override string ToString() => Kind.ToString();

    private string GetDebuggerDisplay()
    {
        return GetType().Name;
    }

    internal virtual SyntaxNode? GetNodeSlot(int index) { return null; }
}