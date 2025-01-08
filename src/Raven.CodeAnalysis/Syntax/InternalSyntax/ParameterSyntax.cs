namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParameterSyntax : SyntaxNode
{
    public ParameterSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax? typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null
    )
        : base(SyntaxKind.Parameter, [
            name ?? throw new ArgumentNullException(nameof(name)),
            typeAnnotation
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax? typeAnnotation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(name, typeAnnotation, diagnostics);
}