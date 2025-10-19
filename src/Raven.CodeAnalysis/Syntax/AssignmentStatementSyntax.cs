namespace Raven.CodeAnalysis.Syntax;

public sealed partial class AssignmentStatementSyntax
{
    public bool IsDiscard => Left switch
    {
        DiscardPatternSyntax => true,
        DiscardExpressionSyntax => true,
        _ => false,
    };
}
