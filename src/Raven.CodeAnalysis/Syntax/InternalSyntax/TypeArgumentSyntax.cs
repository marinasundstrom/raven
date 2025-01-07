namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeArgumentSyntax : SyntaxNode
{
    public TypeArgumentSyntax(
        TypeSyntax type,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
        SyntaxKind.TypeArgument,
        [
            type ?? throw new ArgumentNullException(nameof(type)),
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static TypeArgumentSyntax TypeArgument(
        TypeSyntax type,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(type, diagnostics);
}
