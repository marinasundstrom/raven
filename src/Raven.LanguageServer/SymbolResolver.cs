using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static class SymbolResolver
{
    public static SymbolResolutionResult? ResolveSymbolAtPosition(SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        foreach (var candidateNode in GetCandidateNodes(root, offset))
        {
            var symbol = ResolveSymbolFromNode(semanticModel, candidateNode);
            if (symbol is not null)
                return new SymbolResolutionResult(symbol.UnderlyingSymbol, candidateNode);
        }

        return null;
    }

    private static IEnumerable<SyntaxNode> GetCandidateNodes(SyntaxNode root, int offset)
    {
        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(normalizedOffset);
            }
            catch
            {
                continue;
            }

            var current = token.Parent;
            while (current is not null)
            {
                yield return current;
                current = current.Parent;
            }
        }
    }

    private static IEnumerable<int> NormalizeOffsets(int offset, int maxOffset)
    {
        if (maxOffset < 0)
            yield break;

        var clamped = Math.Clamp(offset, 0, maxOffset);
        yield return clamped;

        if (clamped > 0)
            yield return clamped - 1;
    }

    private static ISymbol? ResolveSymbolFromNode(SemanticModel semanticModel, SyntaxNode node)
    {
        if (node is ParameterSyntax parameterDeclaration)
            return semanticModel.GetDeclaredSymbol(parameterDeclaration);

        if (node.Parent is ParameterSyntax parentParameterDeclaration)
            return semanticModel.GetDeclaredSymbol(parentParameterDeclaration);

        var symbolInfo = semanticModel.GetSymbolInfo(node);
        if (symbolInfo.Symbol is not null)
            return symbolInfo.Symbol;

        if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
            return symbolInfo.CandidateSymbols[0];

        var operation = semanticModel.GetOperation(node);
        var operationSymbol = operation switch
        {
            IParameterReferenceOperation parameterReference => (ISymbol?)parameterReference.Parameter,
            ILocalReferenceOperation localReference => localReference.Local,
            IVariableReferenceOperation variableReference => variableReference.Variable,
            IFieldReferenceOperation fieldReference => fieldReference.Field,
            IPropertyReferenceOperation propertyReference => propertyReference.Property,
            IMethodReferenceOperation methodReference => methodReference.Method,
            IMemberReferenceOperation memberReference => memberReference.Symbol,
            IInvocationOperation invocation => invocation.TargetMethod,
            _ => null
        };

        if (operationSymbol is not null)
            return operationSymbol;

        return semanticModel.GetDeclaredSymbol(node);
    }
}

internal readonly record struct SymbolResolutionResult(ISymbol Symbol, SyntaxNode Node);
