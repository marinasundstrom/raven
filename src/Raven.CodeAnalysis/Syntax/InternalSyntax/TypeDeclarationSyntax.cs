

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class TypeDeclarationSyntax : BaseTypeDeclarationSyntax
{
    protected TypeDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}