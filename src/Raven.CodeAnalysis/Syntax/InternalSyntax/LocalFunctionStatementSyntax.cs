
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public LocalFunctionStatementSyntax(
        SyntaxToken funcKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.LocalFunctionStatement,
              [
                     funcKeyword ?? throw new ArgumentNullException(nameof(name)),
                      name ?? throw new ArgumentNullException(nameof(name)),
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
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnType,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(funcKeyword, name, parameters, returnType, body, diagnostics);
}