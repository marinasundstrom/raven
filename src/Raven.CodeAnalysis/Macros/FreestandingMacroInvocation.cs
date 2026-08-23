using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal readonly record struct FreestandingMacroInvocation(
    SyntaxNode Syntax,
    NameSyntax Name,
    SyntaxToken ExclamationToken,
    MacroCarrierSyntax Carrier)
{
    public ArgumentListSyntax? ArgumentList
        => (Carrier as ParenthesizedMacroCarrierSyntax)?.ArgumentList;

    public ExpressionSyntax? ExpressionArgument
        => (Carrier as ExpressionHeaderMacroCarrierSyntax)?.Expression;

    public MacroDeclarationHeaderSyntax? DeclarationHeader
        => (Carrier as DeclarationMacroCarrierSyntax)?.Header;

    public MacroTokenTreeSyntax? TokenTree
        => Carrier switch
        {
            ParenthesizedMacroCarrierSyntax parenthesized => parenthesized.TokenTree,
            ExpressionHeaderMacroCarrierSyntax expressionHeader => expressionHeader.TokenTree,
            TokenTreeMacroCarrierSyntax tokenTree => tokenTree.TokenTree,
            DeclarationMacroCarrierSyntax declaration => declaration.TokenTree,
            _ => null
        };

    public static FreestandingMacroInvocation Create(FreestandingMacroExpressionSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.Carrier);
    }

    public static FreestandingMacroInvocation Create(FreestandingMacroMemberDeclarationSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.Carrier);
    }

    public static FreestandingMacroInvocation Create(FreestandingMacroDeclarationSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.Carrier);
    }

    public static bool TryCreate(SyntaxNode syntax, out FreestandingMacroInvocation invocation)
    {
        switch (syntax)
        {
            case FreestandingMacroExpressionSyntax expression:
                invocation = Create(expression);
                return true;
            case FreestandingMacroMemberDeclarationSyntax member:
                invocation = Create(member);
                return true;
            case FreestandingMacroDeclarationSyntax declaration:
                invocation = Create(declaration);
                return true;
            default:
                invocation = default;
                return false;
        }
    }

    public bool TryGetMacroName(out string macroName)
        => Syntax switch
        {
            FreestandingMacroExpressionSyntax expression => expression.TryGetMacroName(out macroName),
            FreestandingMacroMemberDeclarationSyntax member => member.TryGetMacroName(out macroName),
            FreestandingMacroDeclarationSyntax declaration => declaration.TryGetMacroName(out macroName),
            _ => Fail(out macroName)
        };

    private static bool Fail(out string value)
    {
        value = string.Empty;
        return false;
    }
}
