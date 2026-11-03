using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class SymbolDeclarationUtilities
{
    public static bool HasDeclaringSyntax(Symbol symbol, SyntaxNode syntax)
        => symbol.DeclaringSyntaxReferences.Any(reference => reference.GetSyntax() == syntax);

    public static bool HasDeclaringSpan(Symbol symbol, SyntaxNode syntax)
        => symbol.DeclaringSyntaxReferences.Any(reference =>
            reference.SyntaxTree == syntax.SyntaxTree &&
            reference.Span == syntax.Span);
}
