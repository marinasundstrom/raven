using System;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class SyntaxNode : GreenNode
{
    private readonly GreenNode[] _slots;

    public SyntaxNode(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null,
        IEnumerable<SyntaxAnnotation>? annotations = null)
        : base(kind, slots?.Length ?? 0, diagnostics, annotations)
    {
        _slots = slots ?? Array.Empty<GreenNode>();

        CalculateWidths();
    }

    public override bool IsMissing => Width == 0;

    public override GreenNode GetSlot(int index)
    {
        if (index < 0 || index >= SlotCount)
            return null;

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

    internal override GreenNode With(GreenNode[] children, DiagnosticInfo[]? diagnostics = null, SyntaxAnnotation[]? annotations = null)
    {
        throw new NotImplementedException("Override method");
    }

    internal override GreenNode SetDiagnostics(params DiagnosticInfo[] diagnostics)
    {
        return With(_slots, diagnostics, _annotations);
    }

    internal override GreenNode WithAdditionalAnnotations(params SyntaxAnnotation[] annotations)
    {
        return With(_slots, null, annotations);
    }

    internal override IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive()
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
            foreach (var diagnostic in child.GetDiagnosticsRecursive())
            {
                yield return diagnostic;
            }
        }
    }
}
