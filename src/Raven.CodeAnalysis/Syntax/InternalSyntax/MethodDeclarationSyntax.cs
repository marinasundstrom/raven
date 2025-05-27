
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MethodDeclarationSyntax : MemberDeclarationSyntax
{
    public MethodDeclarationSyntax(
        SyntaxToken funKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.MethodDeclaration,
              [
                      funKeyword ?? throw new ArgumentNullException(nameof(name)),
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
    public static MethodDeclarationSyntax MethodDeclaration(
        SyntaxToken funKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(funKeyword, name, parameters, returnTypeAnnotation, body, diagnostics);
}

internal partial class LocalFunctionStatementSyntax : StatementSyntax
{
    public LocalFunctionStatementSyntax(
        SyntaxToken funKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.LocalFunctionStatement,
              [
                     funKeyword ?? throw new ArgumentNullException(nameof(name)),
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
        SyntaxToken funKeyword,
        IdentifierNameSyntax name,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnType,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(funKeyword, name, parameters, returnType, body, diagnostics);
}