namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParameterSyntax : SyntaxNode
{
    public ParameterSyntax(
        SyntaxList modifiers,
        SyntaxToken identifier,
        TypeAnnotationSyntax? typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null
    )
        : base(SyntaxKind.Parameter, [
            modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
            identifier ?? throw new ArgumentNullException(nameof(identifier)),
            typeAnnotation!
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(
        SyntaxList modifiers,
        SyntaxToken identifier,
        TypeAnnotationSyntax? typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, identifier, typeAnnotation, diagnostics);
}