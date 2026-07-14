namespace Raven.CodeAnalysis;

internal partial class BoundLoopStatement : BoundStatement
{
    public BoundStatement Body { get; }

    public BoundLoopStatement(BoundStatement body)
    {
        Body = body;
    }
}
