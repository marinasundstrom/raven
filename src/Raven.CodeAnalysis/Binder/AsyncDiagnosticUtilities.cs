using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AsyncDiagnosticUtilities
{
    public static string GetAsyncMemberDescription(IMethodSymbol methodSymbol)
    {
        var kind = methodSymbol.MethodKind switch
        {
            MethodKind.Function => "local function",
            MethodKind.LambdaMethod => "lambda expression",
            MethodKind.PropertyGet or MethodKind.PropertySet => "accessor",
            MethodKind.Ordinary => "method",
            MethodKind.Constructor or MethodKind.NamedConstructor => "constructor",
            _ => "function",
        };

        var display = methodSymbol.ToDisplayString(SymbolDisplayFormat.RavenShortErrorMessageFormat);

        if (string.IsNullOrWhiteSpace(display) ||
            display == "<lambda>" ||
            display.StartsWith("<lambda_", StringComparison.Ordinal))
            return kind;

        return $"{kind} '{display}'";
    }

    public static Location GetAsyncKeywordLocation(IMethodSymbol methodSymbol, SyntaxNode fallback)
    {
        var syntax = methodSymbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
        if (syntax is not null)
        {
            if (TryGetAsyncKeyword(syntax) is { } asyncToken && asyncToken.Kind == SyntaxKind.AsyncKeyword && !asyncToken.IsMissing)
                return asyncToken.GetLocation();

            return syntax.GetLocation();
        }

        return fallback.GetLocation();
    }

    private static SyntaxToken? TryGetAsyncKeyword(SyntaxNode syntax)
    {
        return syntax switch
        {
            MethodDeclarationSyntax method => method.Modifiers.FirstOrDefault(m => m.Kind == SyntaxKind.AsyncKeyword),
            FunctionStatementSyntax function => function.Modifiers.FirstOrDefault(m => m.Kind == SyntaxKind.AsyncKeyword),
            AccessorDeclarationSyntax accessor => accessor.Modifiers.FirstOrDefault(m => m.Kind == SyntaxKind.AsyncKeyword),
            _ => null,
        };
    }
}
