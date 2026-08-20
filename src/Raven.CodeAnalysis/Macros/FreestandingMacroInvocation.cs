using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal readonly record struct FreestandingMacroInvocation(
    SyntaxNode Syntax,
    NameSyntax Name,
    SyntaxToken ExclamationToken,
    ArgumentListSyntax? ArgumentList,
    MacroTokenTreeSyntax? TokenTree)
{
    public static FreestandingMacroInvocation Create(FreestandingMacroExpressionSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.ArgumentList, syntax.TokenTree);
    }

    public static FreestandingMacroInvocation Create(FreestandingMacroMemberDeclarationSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.ArgumentList, syntax.TokenTree);
    }

    public static FreestandingMacroInvocation Create(FreestandingMacroDeclarationSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, null, syntax.TokenTree);
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
