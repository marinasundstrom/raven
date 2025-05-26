namespace Raven.CodeAnalysis;

internal class BoundIsPatternExpression : BoundExpression
{
    public BoundIsPatternExpression(ILocalSymbol? local, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(local.ContainingAssembly.GetTypeByMetadataName("System.Boolean"), local, candidateReason)
    {

    }
}

internal abstract class BoundPatternExpression : BoundExpression
{
    public BoundPatternExpression(ITypeSymbol type, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(type, null, candidateReason)
    {

    }

    public virtual IEnumerable<BoundDesignatorExpression> GetDesignators() => [];
}

internal class BoundDesignatorExpression : BoundExpression
{
    public BoundDesignatorExpression(ILocalSymbol local, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(local.Type, local, candidateReason)
    {

    }
}