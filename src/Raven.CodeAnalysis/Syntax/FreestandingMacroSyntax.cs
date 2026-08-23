namespace Raven.CodeAnalysis.Syntax;

public sealed partial class FreestandingMacroExpressionSyntax
{
    public ArgumentListSyntax? ArgumentList
        => (Carrier as ParenthesizedMacroCarrierSyntax)?.ArgumentList;

    public ExpressionSyntax? ExpressionArgument
        => (Carrier as ExpressionHeaderMacroCarrierSyntax)?.Expression;

    public MacroTokenTreeSyntax? TokenTree => Carrier.GetTokenTree();
}

public sealed partial class FreestandingMacroMemberDeclarationSyntax
{
    public ArgumentListSyntax? ArgumentList
        => (Carrier as ParenthesizedMacroCarrierSyntax)?.ArgumentList;

    public ExpressionSyntax? ExpressionArgument
        => (Carrier as ExpressionHeaderMacroCarrierSyntax)?.Expression;

    public MacroTokenTreeSyntax? TokenTree => Carrier.GetTokenTree();
}

public sealed partial class FreestandingMacroDeclarationSyntax
{
    public MacroDeclarationHeaderSyntax Header => Carrier.Header;

    public SyntaxToken Identifier => Header.Identifier;

    public TypeParameterListSyntax? TypeParameterList => Header.TypeParameterList;

    public ParameterListSyntax? ParameterList => Header.ParameterList;

    public MacroDeclarationSuffixSyntax? Suffix => Header.Suffix;

    public BaseListSyntax? BaseList
        => (Suffix as MacroBaseListSuffixSyntax)?.BaseList;

    public ArrowTypeClauseSyntax? ReturnType
        => (Suffix as MacroReturnTypeSuffixSyntax)?.ReturnType;

    public SyntaxList<TypeParameterConstraintClauseSyntax> ConstraintClauses
        => Header.ConstraintClauses;

    public PermitsClauseSyntax? PermitsClause => Header.PermitsClause;

    public MacroTokenTreeSyntax? TokenTree => Carrier.TokenTree;
}

internal static class MacroCarrierSyntaxExtensions
{
    public static MacroTokenTreeSyntax? GetTokenTree(this MacroCarrierSyntax carrier)
        => carrier switch
        {
            ParenthesizedMacroCarrierSyntax parenthesized => parenthesized.TokenTree,
            ExpressionHeaderMacroCarrierSyntax expressionHeader => expressionHeader.TokenTree,
            TokenTreeMacroCarrierSyntax tokenTree => tokenTree.TokenTree,
            DeclarationMacroCarrierSyntax declaration => declaration.TokenTree,
            _ => null
        };
}
