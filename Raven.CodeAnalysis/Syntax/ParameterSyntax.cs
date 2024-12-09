namespace Raven.CodeAnalysis.Syntax;

public partial class ParameterSyntax : SyntaxNode
{
    public ParameterSyntax(GreenNode greenNode, SyntaxNode parent)
        : base(greenNode, parent)
    {
    }

    public ParameterSyntax() : this(new InternalSyntax.ParameterSyntax(), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter()
        => new ParameterSyntax();
}