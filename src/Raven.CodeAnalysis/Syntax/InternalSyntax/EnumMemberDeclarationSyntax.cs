
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EnumMemberDeclarationSyntax : MemberDeclarationSyntax
{
    public EnumMemberDeclarationSyntax(
        SyntaxToken identifier,
        EqualsValueClauseSyntax? equalsValue,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(SyntaxKind.EnumMemberDeclaration,
          [
            identifier ?? throw new ArgumentNullException(nameof(identifier)),
            equalsValue!
          ],
          diagnostics)
    {
    }
}


internal static partial class SyntaxFactory
{
    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(
        SyntaxToken identifier,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(identifier, null, diagnostics);

    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(
        SyntaxToken name,
        EqualsValueClauseSyntax equalsValue,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(name, equalsValue, diagnostics);
}
