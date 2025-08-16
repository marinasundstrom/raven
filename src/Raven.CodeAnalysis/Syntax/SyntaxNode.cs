using System.Diagnostics;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract partial class SyntaxNode : IEquatable<SyntaxNode>
{
    internal readonly GreenNode Green;
    private readonly SyntaxTree _syntaxTree;
    private readonly SyntaxNode _parent;
    private bool? _isMissing = false;
    private List<Diagnostic>? _diagnostics;
    private bool? _containsDiagnostics;

    public SyntaxNode Parent => _parent;

    public bool IsMissing => _isMissing ??= Green.IsMissing;

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

    internal int Position { get; }

    internal int End => Position + FullWidth;

    internal int Width => Green.Width;

    internal int FullWidth => Green.FullWidth;

    public TextSpan Span
    {
        get
        {
            var start = Position + GetLeadingTrivia().Width;
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
            if (nodeOrToken.TryGetNode(out var node))
            {
                yield return node;
            }
        }
    }

    public ChildSyntaxList ChildNodesAndTokens()
    {
        return new ChildSyntaxList(this);
    }

    public bool HasLeadingTrivia => Green.GetLeadingTrivia().Count > 0;

    public bool HasTrailingTrivia => Green.GetTrailingTrivia().Count > 0;

    public SyntaxTriviaList GetLeadingTrivia()
    {
        var firstToken = GetFirstToken();
        return firstToken == default(SyntaxToken) ? SyntaxTriviaList.Empty : firstToken.LeadingTrivia;
    }

    public SyntaxTriviaList GetTrailingTrivia()
    {
        var lastToken = GetLastToken();
        return lastToken == default(SyntaxToken) ? SyntaxTriviaList.Empty : lastToken.TrailingTrivia;
    }

    public SyntaxNode(GreenNode greenNode, SyntaxNode? parent, int position = 0)
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

    protected virtual string GetDebuggerDisplay()
     => $"{GetType().Name} {Kind} {ToString()}";

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

    internal SyntaxNode ReplaceTokenCore(SyntaxToken oldToken, SyntaxToken newToken)
    {
        // Step 1: Traverse and locate the node to replace in the green tree
        var greenToReplace = oldToken.Green;
        var newGreen = this.Green.ReplaceNode(greenToReplace, newToken.Green);

        // Step 2: Rebuild the red tree with the updated green node
        return newGreen.CreateRed(this.Parent, this.Position);
    }

    internal SyntaxNode ReplaceNodeCore(SyntaxNode oldNode, SyntaxNode newNode)
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

    internal SyntaxNode ReplaceNodeWithNodesCore(SyntaxNode oldNode, IEnumerable<SyntaxNode> newNodes)
    {
        if (oldNode == null)
            throw new ArgumentNullException(nameof(oldNode));

        if (newNodes == null)
            throw new ArgumentNullException(nameof(newNodes));

        // Convert red nodes to green nodes
        var greenOldNode = oldNode.Green;
        var greenNewNodes = newNodes.Select(n => n.Green);

        // Perform the replacement in the green tree
        var newGreen = this.Green.ReplaceNode(greenOldNode, greenNewNodes);

        // Rebuild the red tree
        return newGreen.CreateRed(this.Parent, this.Position);
    }

    public abstract void Accept(SyntaxVisitor visitor);

    public abstract TResult Accept<TResult>(SyntaxVisitor<TResult> visitor);

    public Location GetLocation()
    {
        if (SyntaxTree is null)
        {
            return default(Location)!;
        }
        return SyntaxTree!.GetLocation(Span);
    }

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
        return HashCode.Combine(Green, _parent);
    }

    public bool ContainsDiagnostics => _containsDiagnostics ??= GetDiagnostics().Any();

    public IEnumerable<Diagnostic> GetDiagnostics()
    {
        if (_diagnostics is not null)
            return _diagnostics;

        foreach (var diagnostic in Green.GetDiagnostics())
        {
            var location = SyntaxTree.GetLocation(diagnostic.Span);
            var d = Diagnostic.Create(diagnostic.Descriptor, location, diagnostic.Args);
            (_diagnostics ??= new List<Diagnostic>()).Add(d);
        }

        foreach (var child in ChildNodesAndTokens())
        {
            if (child.IsNode)
            {
                foreach (var diagnostic in child.AsNode()!.GetDiagnostics())
                {
                    (_diagnostics ??= new List<Diagnostic>()).Add(diagnostic);
                }
            }
            else if (child.IsToken)
            {
                foreach (var diagnostic in child.AsToken()!.GetDiagnostics())
                {
                    (_diagnostics ??= new List<Diagnostic>()).Add(diagnostic);
                }
            }
        }

        return _diagnostics ?? Enumerable.Empty<Diagnostic>();
    }

    public IEnumerable<SyntaxAnnotation> GetAnnotations(IEnumerable<string> annotationKinds)
    {
        return Green.GetAnnotations(annotationKinds);
    }

    public SyntaxAnnotation? GetAnnotation(string kind)
    {
        return Green.GetAnnotation(kind);
    }

    public SyntaxNode WithAdditionalAnnotations(params SyntaxAnnotation[] annotations)
    {
        return Green.WithAdditionalAnnotations(annotations).CreateRed();
    }

    public SyntaxReference GetReference() => new SyntaxReference(SyntaxTree!, this);
}