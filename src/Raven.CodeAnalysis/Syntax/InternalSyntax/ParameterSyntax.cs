namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParameterSyntax : SyntaxNode
{
    public ParameterSyntax(
        SyntaxToken identifier,
        TypeAnnotationSyntax? typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null
    )
        : base(SyntaxKind.Parameter, [
            identifier ?? throw new ArgumentNullException(nameof(identifier)),
            typeAnnotation
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(
        SyntaxToken identifier,
        TypeAnnotationSyntax? typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(identifier, typeAnnotation, diagnostics);
}