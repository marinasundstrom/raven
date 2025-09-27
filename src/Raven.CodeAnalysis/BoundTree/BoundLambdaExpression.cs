using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal partial class BoundLambdaExpression : BoundExpression
{
    public IEnumerable<IParameterSymbol> Parameters { get; }
    public BoundExpression Body { get; }
    public ITypeSymbol ReturnType { get; }
    public ITypeSymbol DelegateType { get; }
    public IEnumerable<ISymbol> CapturedVariables { get; }
    public ImmutableArray<INamedTypeSymbol> CandidateDelegates { get; }

    public BoundLambdaExpression(
        IEnumerable<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        BoundExpression body,
        ISymbol symbol,
        ITypeSymbol delegateType,
        IEnumerable<ISymbol> capturedVariables,
        ImmutableArray<INamedTypeSymbol> candidateDelegates)
        : base(delegateType, symbol, BoundExpressionReason.None)
    {
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
        DelegateType = delegateType;
        CapturedVariables = capturedVariables;
        CandidateDelegates = candidateDelegates;
    }

    public bool IsCompatibleWithDelegate(INamedTypeSymbol delegateType, Compilation compilation)
    {
        if (delegateType.TypeKind != TypeKind.Delegate)
            return false;

        var targetInvoke = delegateType.GetDelegateInvokeMethod();
        if (targetInvoke is null)
            return false;

        if (CandidateDelegates.IsDefaultOrEmpty)
            return targetInvoke.Parameters.Length == Parameters.Count();

        foreach (var candidate in CandidateDelegates)
        {
            if (candidate.TypeKind != TypeKind.Delegate)
                continue;

            var candidateInvoke = candidate.GetDelegateInvokeMethod();
            if (candidateInvoke is null)
                continue;

            if (!HaveCompatibleSignature(candidateInvoke, targetInvoke, compilation))
                continue;

            return true;
        }

        return false;

        static bool HaveCompatibleSignature(IMethodSymbol source, IMethodSymbol target, Compilation compilation)
        {
            if (source.Parameters.Length != target.Parameters.Length)
                return false;

            for (int i = 0; i < source.Parameters.Length; i++)
            {
                var sourceParameter = source.Parameters[i];
                var targetParameter = target.Parameters[i];

                if (sourceParameter.RefKind != targetParameter.RefKind)
                    return false;

                var conversion = compilation.ClassifyConversion(sourceParameter.Type, targetParameter.Type);
                if (!conversion.Exists || !conversion.IsImplicit)
                    return false;
            }

            var returnConversion = compilation.ClassifyConversion(source.ReturnType, target.ReturnType);
            return returnConversion.Exists && returnConversion.IsImplicit;
        }
    }
}