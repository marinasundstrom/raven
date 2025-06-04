

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class BaseMethodDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseMethodDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}