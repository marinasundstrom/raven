

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class BaseTypeDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseTypeDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}