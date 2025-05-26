namespace Raven.CodeAnalysis.Syntax;

public abstract partial class VariableDesignationSyntax : SyntaxNode
{

    internal VariableDesignationSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}


public partial class SingleVariableDesignationSyntax : VariableDesignationSyntax
{
    public partial SyntaxToken Identifier { get; }

    internal SingleVariableDesignationSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public SingleVariableDesignationSyntax(SyntaxToken identifier)
        : this(new InternalSyntax.SingleVariableDesignationSyntax(identifier.Green), (SyntaxNode)null, 0)
    {

    }
}


public static partial class SyntaxFactory
{
    public static SingleVariableDesignationSyntax SingleVariableDesignation(SyntaxToken identifier)
        => new SingleVariableDesignationSyntax(identifier);
}