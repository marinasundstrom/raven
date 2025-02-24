namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class VariableDeclarationSyntax : SyntaxNode
{
    public VariableDeclarationSyntax(
        SyntaxToken letOrVarKeyword,
        SyntaxList variableDeclarators,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.VariableDeclaration,
              [
                      letOrVarKeyword ?? throw new ArgumentNullException(nameof(letOrVarKeyword)),
                      variableDeclarators ?? throw new ArgumentNullException(nameof(variableDeclarators))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static VariableDeclarationSyntax VariableDeclaration(
        SyntaxToken letOrVarKeyword,
        SyntaxList variableDeclarators,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(letOrVarKeyword, variableDeclarators, diagnostics);
}