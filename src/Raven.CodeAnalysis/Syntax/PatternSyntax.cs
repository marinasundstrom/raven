namespace Raven.CodeAnalysis.Syntax;

public abstract partial class PatternSyntax : ExpressionOrPatternSyntax
{

    internal PatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public partial class UnaryPatternSyntax : PatternSyntax
{
    public partial SyntaxToken OperatorToken { get; }

    public partial PatternSyntax Pattern { get; }

    internal UnaryPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public UnaryPatternSyntax(SyntaxToken operatorToken, PatternSyntax pattern)
        : this(new InternalSyntax.UnaryPatternSyntax(operatorToken.Green, (InternalSyntax.PatternSyntax)pattern.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class DeclarationPatternSyntax : PatternSyntax
{
    public partial TypeSyntax Type { get; }

    public partial VariableDesignationSyntax Designation { get; }

    internal DeclarationPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public DeclarationPatternSyntax(TypeSyntax type, VariableDesignationSyntax designation)
        : this(new InternalSyntax.DeclarationPatternSyntax((InternalSyntax.TypeSyntax)type.Green, (InternalSyntax.VariableDesignationSyntax)designation.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static UnaryPatternSyntax UnaryPattern(SyntaxToken operatorToken, PatternSyntax pattern)
        => new UnaryPatternSyntax(operatorToken, pattern);

    public static DeclarationPatternSyntax DeclarationPattern(TypeSyntax type, VariableDesignationSyntax designation)
        => new DeclarationPatternSyntax(type, designation);
}