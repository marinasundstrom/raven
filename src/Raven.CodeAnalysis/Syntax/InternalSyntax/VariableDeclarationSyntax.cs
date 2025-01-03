namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class VariableDeclarationSyntax : SyntaxNode
{
    public VariableDeclarationSyntax(
        SyntaxToken letKeyword,
        SyntaxList variableDeclarators,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclaration,
              [
                      letKeyword,
                      variableDeclarators
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static VariableDeclarationSyntax VariableDeclaration(
        SyntaxToken letKeyword,
        SyntaxList variableDeclarators,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(letKeyword, variableDeclarators, diagnostics);
}