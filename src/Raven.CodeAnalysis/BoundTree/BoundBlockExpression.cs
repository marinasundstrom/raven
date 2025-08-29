using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

internal partial class BoundBlockExpression : BoundExpression
{
    public BoundBlockExpression(IEnumerable<BoundStatement> statements, ITypeSymbol unitType)
        : base((statements.LastOrDefault() as BoundExpressionStatement)?.Expression.Type ?? unitType, null, BoundExpressionReason.None)
    {
        Statements = statements;
        UnitType = unitType;
    }

    public IEnumerable<BoundStatement> Statements { get; }
    public ITypeSymbol UnitType { get; }
}

