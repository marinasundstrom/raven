using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public ImmutableArray<ISymbol> GetCapturedVariables(ISymbol symbol)
    {
        if (symbol is null)
            return ImmutableArray<ISymbol>.Empty;

        return symbol switch
        {
            SourceLambdaSymbol lambda => lambda.CapturedVariables,
            SourceMethodSymbol method => method.CapturedVariables,
            _ => ImmutableArray<ISymbol>.Empty
        };
    }

    public ImmutableArray<ISymbol> GetCapturedVariables(SyntaxNode node)
    {
        if (node is null)
            return ImmutableArray<ISymbol>.Empty;

        EnsureDiagnosticsCollected();

        if (node is FunctionStatementSyntax function &&
            GetDeclaredSymbol(function) is ISymbol functionSymbol)
        {
            return GetCapturedVariables(functionSymbol);
        }

        if (node is LambdaExpressionSyntax lambdaSyntax &&
            GetBoundNode(lambdaSyntax) is BoundLambdaExpression boundLambda)
        {
            return boundLambda.CapturedVariables
                .Where(static symbol => symbol is not null)
                .Distinct(SymbolEqualityComparer.Default)
                .ToImmutableArray();
        }

        var symbol = node switch
        {
            VariableDeclaratorSyntax variableDeclarator => GetDeclaredSymbol(variableDeclarator),
            ParameterSyntax parameter => GetDeclaredSymbol(parameter),
            _ => GetSymbolInfo(node).Symbol
        };

        return symbol is null
            ? ImmutableArray<ISymbol>.Empty
            : IsCapturedVariable(symbol) ? ImmutableArray.Create(symbol) : ImmutableArray<ISymbol>.Empty;
    }

    public bool IsCapturedVariable(ISymbol symbol)
    {
        if (symbol is not ILocalSymbol and not IParameterSymbol and not ITypeSymbol)
            return false;

        EnsureDiagnosticsCollected();

        var root = SyntaxTree.GetRoot();
        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            if (GetDeclaredSymbol(function) is not ISymbol functionSymbol)
                continue;

            var captures = GetCapturedVariables(functionSymbol);
            if (!captures.IsDefaultOrEmpty &&
                captures.Contains(symbol, SymbolEqualityComparer.Default))
            {
                return true;
            }
        }

        foreach (var lambda in root.DescendantNodes().OfType<LambdaExpressionSyntax>())
        {
            var captures = GetCapturedVariables(lambda);
            if (!captures.IsDefaultOrEmpty &&
                captures.Contains(symbol, SymbolEqualityComparer.Default))
            {
                return true;
            }
        }

        return false;
    }
}
