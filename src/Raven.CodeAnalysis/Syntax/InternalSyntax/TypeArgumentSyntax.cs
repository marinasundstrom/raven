namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeArgumentSyntax : SyntaxNode
{
    public TypeArgumentSyntax(
        TypeSyntax type,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
        SyntaxKind.TypeArgument,
        [
            type,
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static TypeArgumentSyntax TypeArgument(
        TypeSyntax type,
        IEnumerable<Diagnostic>? diagnostics = null)
      => new(type, diagnostics);
}
