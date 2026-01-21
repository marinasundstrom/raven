using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Diagnostics;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class GreenNode
{
    internal readonly DiagnosticInfo[]? _diagnostics;
    internal readonly SyntaxAnnotation[]? _annotations;

    internal GreenNode(SyntaxKind kind, int slotCount, IEnumerable<DiagnosticInfo>? diagnostics = null, IEnumerable<SyntaxAnnotation>? annotations = null)
    {
        Kind = kind;
        SlotCount = slotCount;
        _diagnostics = diagnostics?.ToArray() ?? null;
        _annotations = annotations?.ToArray() ?? null;
    }

    public virtual SyntaxKind Kind { get; }

    public bool IsKind(SyntaxKind kind) => Kind == kind;

    public int Width { get; protected set; }
    public int FullWidth { get; protected set; }
    public int SlotCount { get; }

    internal InternalSyntax.SyntaxTriviaList GetLeadingTrivia()
    {
        var firstToken = GetFirstToken();
        return firstToken?.LeadingTrivia ?? InternalSyntax.SyntaxTriviaList.Empty;
    }

    internal InternalSyntax.SyntaxTriviaList GetTrailingTrivia()
    {
        var lastToken = GetLastToken();
        return lastToken?.TrailingTrivia ?? InternalSyntax.SyntaxTriviaList.Empty;
    }

    public virtual int GetLeadingTriviaWidth()
    {
        var firstToken = GetFirstToken();
        return firstToken is null ? 0 : firstToken.LeadingTrivia.Width;
    }

    public virtual int GetTrailingTriviaWidth()
    {
        var lastToken = GetLastToken();
        return lastToken is null ? 0 : lastToken.TrailingTrivia.Width;
    }

    public virtual bool IsMissing { get; }

    public abstract GreenNode GetSlot(int index);

    public T GetSlot<T>(int index) where T : GreenNode => (T)GetSlot(index);

    internal IEnumerable<GreenNode> GetChildren()
    {
        for (int i = 0; i < SlotCount; i++)
        {
            var child = GetSlot(i);

            if (child is null)
                continue;

            if (child is InternalSyntax.SyntaxList)
            {
                for (int i2 = 0; i2 < child.SlotCount; i2++)
                {
                    var child2 = child.GetSlot(i2);

                    if (child2 is null)
                        continue;

                    yield return child2;
                }
            }
            else
            {
                yield return child;
            }
        }
    }

    internal InternalSyntax.SyntaxToken? GetFirstToken()
    {
        for (int i = 0, n = SlotCount; i < n; i++)
        {
            var child = GetSlot(i);
            if (child == null) continue;

            if (child is InternalSyntax.SyntaxToken t) return t;

            var tok = child.GetFirstToken();
            if (tok != null) return tok; // keep scanning siblings if null
        }
        return null;
    }

    internal InternalSyntax.SyntaxToken? GetLastToken()
    {
        for (int i = SlotCount - 1; i >= 0; i--)
        {
            var child = GetSlot(i);
            if (child == null) continue;

            if (child is InternalSyntax.SyntaxToken t) return t;

            var tok = child.GetLastToken();
            if (tok != null) return tok; // keep scanning siblings if null
        }
        return null;
    }

    public SyntaxNode CreateRed()
    {
        return CreateRed(null, 0);
    }

    public virtual SyntaxNode CreateRed(SyntaxNode? parent, int position)
    {
        return null!;
    }

    public virtual object? GetValue() => (int)Kind;

    public virtual string? GetValueText() => SyntaxFacts.IsReservedWordKind(Kind) ? SyntaxFacts.GetSyntaxTokenText(Kind) : Kind.ToString();

    public virtual int GetChildStartPosition(int childIndex)
    {
        int offset = 0;

        for (int i = 0; i < childIndex; i++)
        {
            var slot = GetSlot(i);
            if (slot != null)
            {
                offset += slot.FullWidth;
            }
        }

        return offset;
    }
    public GreenNode ReplaceNode(GreenNode oldNode, GreenNode newNode)
    {
        if (this == oldNode)
        {
            return newNode;
        }

        var updatedChildren = new GreenNode[this.SlotCount];
        for (int i = 0; i < this.SlotCount; i++)
        {
            var child = this.GetSlot(i);
            if (child == oldNode)
            {
                updatedChildren[i] = newNode;
            }
            else
            {
                updatedChildren[i] = child?.ReplaceNode(oldNode, newNode) ?? child;
            }
        }

        return With(updatedChildren);
    }

    public GreenNode ReplaceNode(GreenNode oldNode, IEnumerable<GreenNode> newNodes)
    {
        return ReplaceNodeWithNodes(oldNode, newNodes);
    }

    private GreenNode ReplaceNodeWithNodes(GreenNode oldNode, IEnumerable<GreenNode> newNodes)
    {
        if (this == oldNode)
        {
            // If the current node is the one to replace, return a new parent with the new nodes
            return CreateParentWithNodes(newNodes);
        }

        // Traverse child nodes to find the node to replace
        var updatedChildren = new List<GreenNode>();
        for (int i = 0; i < SlotCount; i++)
        {
            var child = GetSlot(i);

            if (child == oldNode)
            {
                // Add the new nodes in place of the old node
                updatedChildren.AddRange(newNodes);
            }
            else if (child != null)
            {
                // Recur for other children
                updatedChildren.Add(child.ReplaceNodeWithNodes(oldNode, newNodes));
            }
        }

        // Create a new green node with updated children
        return With(updatedChildren.ToArray());
    }

    public GreenNode ReplaceNodes(Func<GreenNode, bool> condition, Func<GreenNode, GreenNode> replacement)
    {
        // If the current node matches the condition, replace it using the replacement function
        if (condition(this))
        {
            return replacement(this);
        }

        // Otherwise, traverse and replace children recursively
        var updatedChildren = new GreenNode[this.SlotCount];
        bool anyChildReplaced = false;

        for (int i = 0; i < this.SlotCount; i++)
        {
            var child = this.GetSlot(i);

            if (child != null)
            {
                var updatedChild = child.ReplaceNodes(condition, replacement);

                if (!ReferenceEquals(updatedChild, child))
                {
                    anyChildReplaced = true;
                }

                updatedChildren[i] = updatedChild;
            }
            else
            {
                updatedChildren[i] = child;
            }
        }

        // If no children were replaced, return the current node to avoid creating unnecessary copies
        if (!anyChildReplaced)
        {
            return this;
        }

        // Otherwise, return a new node with the updated children
        return With(updatedChildren);
    }

    internal virtual GreenNode CreateParentWithNodes(IEnumerable<GreenNode> newNodes)
    {
        // This implementation depends on the specific type of the parent node.
        // For simplicity, you can return a SyntaxList for now, or customize it based on the context.

        return new SyntaxList(newNodes.ToArray());
    }

    internal abstract GreenNode With(GreenNode[] children, DiagnosticInfo[]? diagnostics = null, SyntaxAnnotation[]? annotations = null);

    internal IEnumerable<DiagnosticInfo> GetDiagnostics()
    {
        return _diagnostics ?? Array.Empty<DiagnosticInfo>();
    }

    internal GreenNode WithAdditionalDiagnostics(params DiagnosticInfo[] diagnostics)
    {
        if (diagnostics is null || diagnostics.Length == 0)
        {
            return this;
        }

        if (_diagnostics is null || _diagnostics.Length == 0)
        {
            return SetDiagnostics(diagnostics);
        }

        var merged = new DiagnosticInfo[_diagnostics.Length + diagnostics.Length];
        _diagnostics.CopyTo(merged, 0);
        diagnostics.CopyTo(merged, _diagnostics.Length);
        return SetDiagnostics(merged);
    }

    public IEnumerable<SyntaxAnnotation> GetAnnotations(IEnumerable<string> annotationKinds)
    {
        if (_annotations is null)
            return Array.Empty<SyntaxAnnotation>();

        return _annotations.Where(x => annotationKinds.Contains(x.Kind));
    }

    public SyntaxAnnotation? GetAnnotation(string kind)
    {
        return _annotations?.FirstOrDefault(x => x.Kind == kind);
    }

    internal virtual GreenNode WithAdditionalAnnotations(params SyntaxAnnotation[] annotations)
    {
        throw new NotImplementedException("Override method");
    }

    internal abstract IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive();

    internal abstract GreenNode SetDiagnostics(params DiagnosticInfo[] diagnostics);

    private string GetDebuggerDisplay() => $"{GetType().Name} {(IsMissing ? "<missing>" : GetValueText())}";

    internal abstract void Accept(InternalSyntax.SyntaxVisitor visitor);

    internal abstract TResult Accept<TResult>(InternalSyntax.SyntaxVisitor<TResult> visitor);

    // Used by generated code
    protected static bool AreEqual<T>(T[]? a, T[]? b)
    {
        if (ReferenceEquals(a, b)) return true;
        if (a == null || b == null) return false;
        if (a.Length != b.Length) return false;

        for (int i = 0; i < a.Length; i++)
        {
            if (!Equals(a[i], b[i])) return false;
        }

        return true;
    }

    protected void CalculateWidths()
    {
        // Assumes slots are already set.
        int slotCount = SlotCount;

        if (slotCount == 0) { Width = 0; FullWidth = 0; return; }

        if (slotCount == 1)
        {
            var only = GetSlot(0);
            Width = only?.Width ?? 0;
            FullWidth = only?.FullWidth ?? 0;
            return;
        }

        int sumFull = 0;
        for (int i = 0; i < slotCount; i++)
        {
            var s = GetSlot(i);
            if (s != null) sumFull += s.FullWidth;
        }

        var firstTok = GetFirstToken();
        var lastTok = GetLastToken();

        int leading = firstTok?.LeadingTrivia.Width ?? 0;
        int trailing = lastTok?.TrailingTrivia.Width ?? 0;

        FullWidth = sumFull;
        Width = Math.Max(0, sumFull - leading - trailing);
    }

    [Conditional("DEBUG")]
    public void DebugValidateAgainstText(string source)
    {
        // 1) FullWidth must equal total text length for the root.
        if (this is InternalSyntax.CompilationUnitSyntax && FullWidth != source.Length)
            throw new InvalidOperationException($"Root.FullWidth={FullWidth} != source.Length={source.Length}");

        // 2) Width <= FullWidth, both non-negative.
        if (Width < 0 || FullWidth < 0 || Width > FullWidth)
            throw new InvalidOperationException($"Invalid widths: Width={Width}, FullWidth={FullWidth}");

        // 3) Child FullWidth sum must equal this.FullWidth
        int sum = 0;
        for (int i = 0; i < SlotCount; i++)
            sum += GetSlot(i)?.FullWidth ?? 0;
        if (sum != FullWidth)
            throw new InvalidOperationException($"Child FullWidth sum {sum} != node.FullWidth {FullWidth}");

        // 4) If FullWidth > 0 and Width == 0, then all tokens are zero-width and/or all width is trivia.
        if (FullWidth > 0 && Width == 0)
        {
            var firstTok = GetFirstToken();
            var lastTok = GetLastToken();
            if (firstTok == null || lastTok == null)
                throw new InvalidOperationException("Non-zero FullWidth but no tokens found.");
            // Leading + trailing trivia must consume everything.
            var leading = firstTok.LeadingTrivia.Width;
            var trailing = lastTok.TrailingTrivia.Width;
            if (leading + trailing != FullWidth)
                throw new InvalidOperationException("Width==0 but outer trivia does not consume FullWidth.");
        }

        // 5) Monotonic child offsets
        int offset = 0;
        for (int i = 0; i < SlotCount; i++)
        {
            var child = GetSlot(i);
            if (child == null) continue;
            var start = GetChildStartPosition(i);
            if (start != offset)
                throw new InvalidOperationException($"Child[{i}] start {start} != expected {offset}");
            offset += child.FullWidth;
        }

        // Recurse
        for (int i = 0; i < SlotCount; i++)
            GetSlot(i)?.DebugValidateAgainstText(source);
    }
}
