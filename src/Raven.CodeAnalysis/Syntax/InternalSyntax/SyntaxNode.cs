﻿namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxNode : GreenNode
{
    private readonly GreenNode[] _slots;
    private bool _isMissing;

    public SyntaxNode(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(kind, slots?.Length ?? 0, diagnostics)
    {
        _slots = slots ?? Array.Empty<GreenNode>();

        Width = this.CalculateWidth();
        FullWidth = this.CalculateFullWidth();
    }

    public override bool IsMissing => _isMissing = GetChildren()
        .All(s => s.IsMissing);

    public override GreenNode GetSlot(int index)
    {
        if (index < 0 || index >= SlotCount)
            return null;
        //throw new IndexOutOfRangeException($"Invalid slot index: {index}");
        return _slots[index];
    }

    public override int GetChildStartPosition(int childIndex)
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

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return new SyntaxNode(Kind, newChildren, _diagnostics);
    }

    internal override GreenNode WithDiagnostics(params Diagnostic[] diagnostics)
    {
        return new SyntaxNode(Kind, _slots, _diagnostics);
    }

    internal override IEnumerable<Diagnostic> GetDiagnostics()
    {
        if (_diagnostics is not null)
        {
            foreach (var diagnostic in _diagnostics)
            {
                yield return diagnostic;
            }
        }

        foreach (var child in GetChildren())
        {
            foreach (var diagnostic in child.GetDiagnostics())
            {
                yield return diagnostic;
            }
        }
    }
}