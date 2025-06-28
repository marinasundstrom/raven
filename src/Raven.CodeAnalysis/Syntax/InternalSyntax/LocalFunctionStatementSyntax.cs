
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public LocalFunctionStatementSyntax(
        SyntaxToken funcKeyword,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ArrowTypeClauseSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.LocalFunctionStatement,
              [
                     funcKeyword ?? throw new ArgumentNullException(nameof(funcKeyword)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      returnTypeAnnotation ?? throw new ArgumentNullException(nameof(returnTypeAnnotation)),
                      body ?? throw new ArgumentNullException(nameof(body)),
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static LocalFunctionStatementSyntax LocalFunctionStatement(
        SyntaxToken funcKeyword,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ArrowTypeClauseSyntax returnType,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(funcKeyword, identifier, parameters, returnType, body, diagnostics);
}