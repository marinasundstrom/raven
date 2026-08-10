using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal readonly record struct InvocableMacroInvocation(
    SyntaxNode Syntax,
    NameSyntax Name,
    SyntaxToken ExclamationToken,
    ArgumentListSyntax ArgumentList,
    MacroTokenTreeSyntax? TokenTree)
{
    public static InvocableMacroInvocation Create(InvocableMacroExpressionSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.ArgumentList, syntax.TokenTree);
    }

    public static InvocableMacroInvocation Create(InvocableMacroMemberDeclarationSyntax syntax)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return new(syntax, syntax.Name, syntax.ExclamationToken, syntax.ArgumentList, syntax.TokenTree);
    }

    public static bool TryCreate(SyntaxNode syntax, out InvocableMacroInvocation invocation)
    {
        switch (syntax)
        {
            case InvocableMacroExpressionSyntax expression:
                invocation = Create(expression);
                return true;
            case InvocableMacroMemberDeclarationSyntax member:
                invocation = Create(member);
                return true;
            default:
                invocation = default;
                return false;
        }
    }

    public bool TryGetMacroName(out string macroName)
        => Syntax switch
        {
            InvocableMacroExpressionSyntax expression => expression.TryGetMacroName(out macroName),
            InvocableMacroMemberDeclarationSyntax member => member.TryGetMacroName(out macroName),
            _ => Fail(out macroName)
        };

    private static bool Fail(out string value)
    {
        value = string.Empty;
        return false;
    }
}
