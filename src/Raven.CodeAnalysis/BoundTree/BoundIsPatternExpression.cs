using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BoundIsPatternExpression : BoundExpression
{
    public BoundExpression Expression { get; }
    public BoundPattern Pattern { get; }

    public BoundIsPatternExpression(BoundExpression expression, BoundPattern pattern, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(expression.Type.BaseType.ContainingAssembly.GetTypeByMetadataName("System.Boolean")!, null, reason)
    {
        Expression = expression;
        Pattern = pattern;
    }
}

internal abstract class BoundPattern : BoundExpression
{
    public BoundPattern(ITypeSymbol type, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(type, null, candidateReason)
    {

    }

    public virtual IEnumerable<BoundDesignator> GetDesignators() => [];
}

internal sealed partial class BoundNotPattern : BoundPattern
{
    public BoundPattern Pattern { get; }

    public BoundNotPattern(BoundPattern pattern)
        : base(pattern.Type)
    {
        Pattern = pattern;
    }
}

internal sealed partial class BoundAndPattern : BoundPattern
{
    public BoundPattern Left { get; }
    public BoundPattern Right { get; }

    public BoundAndPattern(BoundPattern left, BoundPattern right)
        : base(left.Type) // Typval kan justeras
    {
        Left = left;
        Right = right;
    }
}

internal sealed partial class BoundOrPattern : BoundPattern
{
    public BoundPattern Left { get; }
    public BoundPattern Right { get; }

    public BoundOrPattern(BoundPattern left, BoundPattern right)
        : base(left.Type) // Typval kan justeras
    {
        Left = left;
        Right = right;
    }
}

internal partial class BoundDeclarationPattern : BoundPattern
{
    public ITypeSymbol DeclaredType { get; }
    public BoundDesignator Designator { get; }

    public BoundDeclarationPattern(
        ITypeSymbol declaredType,
        BoundDesignator designator,
        BoundExpressionReason candidateReason = BoundExpressionReason.None)
        : base(declaredType, candidateReason)
    {
        DeclaredType = declaredType;
        Designator = designator;
    }

    public override IEnumerable<BoundDesignator> GetDesignators() => [Designator];
}

internal partial class BoundDesignator : BoundExpression
{
    public BoundDesignator(ILocalSymbol local, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(local.Type, local, candidateReason)
    {

    }
}

internal partial class BlockBinder
{
    public virtual BoundPattern BindPattern(PatternSyntax syntax)
    {
        return syntax switch
        {
            DeclarationPatternSyntax d => BindDeclarationPattern(d),
            UnaryPatternSyntax u => BindUnaryPattern(u),
            BinaryPatternSyntax b => BindBinaryPattern(b),
            _ => throw new NotImplementedException($"Unknown pattern kind: {syntax.Kind}")
        };
    }

    private BoundPattern BindDeclarationPattern(DeclarationPatternSyntax syntax)
    {
        var type = BindTypeSyntax(syntax.Type);
        var local = BindSingleVariableDesignation((SingleVariableDesignationSyntax)syntax.Designation);
        return new BoundDeclarationPattern(type.Type, new BoundDesignator(local));
    }

    private BoundPattern BindUnaryPattern(UnaryPatternSyntax syntax)
    {
        var operand = BindPattern(syntax.Pattern);
        return syntax.Kind switch
        {
            SyntaxKind.NotPattern => new BoundNotPattern(operand),
            _ => throw new NotImplementedException($"Unsupported unary pattern: {syntax.Kind}")
        };
    }

    private BoundPattern BindBinaryPattern(BinaryPatternSyntax syntax)
    {
        var left = BindPattern(syntax.Left);
        var right = BindPattern(syntax.Right);

        return syntax.Kind switch
        {
            SyntaxKind.AndPattern => new BoundAndPattern(left, right),
            SyntaxKind.OrPattern => new BoundOrPattern(left, right),
            _ => throw new NotImplementedException($"Unsupported binary pattern: {syntax.Kind}")
        };
    }
}

internal partial class BlockBinder
{
    private BoundExpression BindIsPatternExpression(IsPatternExpressionSyntax syntax)
    {
        var expression = BindExpression(syntax.Expression);
        var pattern = BindPattern(syntax.Pattern);

        return new BoundIsPatternExpression(expression, pattern);
    }

    private ILocalSymbol? BindSingleVariableDesignation(SingleVariableDesignationSyntax singleVariableDesignation)
    {
        var declaration = singleVariableDesignation.Parent as DeclarationPatternSyntax;
        var name = singleVariableDesignation.Identifier.Text;
        var type = ResolveType(declaration.Type);
        return CreateLocalSymbol(singleVariableDesignation, name, true, type);
    }
}