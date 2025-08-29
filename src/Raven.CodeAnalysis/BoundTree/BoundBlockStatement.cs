using System.Collections.Generic;

namespace Raven.CodeAnalysis;

internal partial class BoundBlockStatement : BoundStatement
{
    public IEnumerable<BoundStatement> Statements { get; }

    public BoundBlockStatement(IEnumerable<BoundStatement> statements)
    {
        Statements = statements;
    }
}
