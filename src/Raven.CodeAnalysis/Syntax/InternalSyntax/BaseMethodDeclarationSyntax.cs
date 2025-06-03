

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class BaseMethodDeclarationSyntax : MemberDeclarationSyntax
{
    protected BaseMethodDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }

    /*
    public BaseMethodDeclarationSyntax(
        SyntaxKind kind,
        ParameterListSyntax parameters,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind,
              [
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      body ?? throw new ArgumentNullException(nameof(body)),
              ],
              diagnostics)
    {
    }
    */
}