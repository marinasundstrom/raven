namespace Raven.CodeAnalysis;

internal sealed partial class BoundLoopStatement : BoundStatement
{
    public BoundStatement Body { get; }

    public BoundLoopStatement(BoundStatement body)
    {
        Body = body;
    }
}
