namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class VariableDesignationSyntax : SyntaxNode
{
    public VariableDesignationSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}

internal partial class SingleVariableDesignationSyntax : VariableDesignationSyntax
{
    public SingleVariableDesignationSyntax(
        SyntaxToken identifier,
        IEnumerable<DiagnosticInfo>? diagnostics = null
    )
        : base(SyntaxKind.SingleVariableDesignation, [
            identifier ?? throw new ArgumentNullException(nameof(identifier)),
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static SingleVariableDesignationSyntax SingleVariableDesignation(
        SyntaxToken identifier,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(identifier, diagnostics);
}