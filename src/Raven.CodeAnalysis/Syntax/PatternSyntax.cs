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
    public override partial SyntaxKind Kind { get; }

    public partial SyntaxToken OperatorToken { get; }

    public partial PatternSyntax Pattern { get; }

    internal UnaryPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public UnaryPatternSyntax(SyntaxKind kind, SyntaxToken operatorToken, PatternSyntax pattern)
        : this(new InternalSyntax.UnaryPatternSyntax(kind, operatorToken.Green, (InternalSyntax.PatternSyntax)pattern.Green), (SyntaxNode)null, 0)
    {

    }
}

public partial class BinaryPatternSyntax : PatternSyntax
{
    public override partial SyntaxKind Kind { get; }

    public partial PatternSyntax Left { get; }

    public partial SyntaxToken OperatorToken { get; }

    public partial PatternSyntax Right { get; }

    internal BinaryPatternSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public BinaryPatternSyntax(SyntaxKind kind, PatternSyntax left, SyntaxToken operatorToken, PatternSyntax right)
        : this(new InternalSyntax.BinaryPatternSyntax(kind, (InternalSyntax.PatternSyntax)left.Green, operatorToken.Green, (InternalSyntax.PatternSyntax)right.Green), (SyntaxNode)null, 0)
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
    public static UnaryPatternSyntax UnaryPattern(SyntaxKind kind, SyntaxToken operatorToken, PatternSyntax pattern)
        => new UnaryPatternSyntax(kind, operatorToken, pattern);

    public static BinaryPatternSyntax BinaryPattern(SyntaxKind kind, PatternSyntax left, SyntaxToken operatorToken, PatternSyntax right)
        => new BinaryPatternSyntax(kind, left, operatorToken, right);

    public static DeclarationPatternSyntax DeclarationPattern(TypeSyntax type, VariableDesignationSyntax designation)
        => new DeclarationPatternSyntax(type, designation);
}