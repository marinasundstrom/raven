using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal interface MacroFragmentServices : SequenceTypeServices
{
    MacroArgumentServices ArgumentServices { get; }
    TypeSymbol ErrorType { get; }
    TypeSymbol? GetExpressionType(SyntaxNode invocation, ExpressionSyntax expression);
    TypeInfo GetTypeInfo(SyntaxNode invocation, ExpressionSyntax expression);
    SymbolInfo GetSymbolInfo(SyntaxNode invocation, ExpressionSyntax expression);
}
