using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class SymbolDeclarationUtilities
{
    public static bool HasDeclaringSyntax(ISymbol symbol, SyntaxNode syntax)
        => symbol.DeclaringSyntaxReferences.Any(reference => reference.GetSyntax() == syntax);

    public static bool HasDeclaringSpan(ISymbol symbol, SyntaxNode syntax)
        => symbol.DeclaringSyntaxReferences.Any(reference =>
            reference.SyntaxTree == syntax.SyntaxTree &&
            reference.Span == syntax.Span);
}
