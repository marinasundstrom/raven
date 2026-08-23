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
    public SyntaxToken Identifier => Carrier.Identifier;

    public ParameterListSyntax? ParameterList => Carrier.ParameterList;

    public MacroTokenTreeSyntax TokenTree => Carrier.TokenTree;
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
