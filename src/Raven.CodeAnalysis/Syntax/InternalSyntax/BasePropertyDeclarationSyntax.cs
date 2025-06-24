

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class BasePropertyDeclarationSyntax : MemberDeclarationSyntax
{
    protected BasePropertyDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}