using System.Collections.Generic;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal static class ParserRecoverySets
{
    private static readonly HashSet<SyntaxKind> s_statementTerminators = new()
    {
        SyntaxKind.SemicolonToken,
        SyntaxKind.LineFeedToken,
        SyntaxKind.CarriageReturnToken,
        SyntaxKind.CarriageReturnLineFeedToken,
        SyntaxKind.NewLineToken
    };

    private static readonly HashSet<SyntaxKind> s_statementRecovery = new(s_statementTerminators)
    {
        SyntaxKind.CloseBraceToken,
        SyntaxKind.EndOfFileToken,
        SyntaxKind.ElseKeyword,
        SyntaxKind.CatchKeyword,
        SyntaxKind.FinallyKeyword,
        SyntaxKind.CloseParenToken,
        SyntaxKind.CloseBracketToken,
        SyntaxKind.CommaToken,
        SyntaxKind.ColonToken,
    };

    private static readonly HashSet<SyntaxKind> s_typeMemberRecovery = new()
    {
        SyntaxKind.CloseBraceToken,
        SyntaxKind.EndOfFileToken,
        SyntaxKind.OpenBracketToken,
        SyntaxKind.ClassKeyword,
        SyntaxKind.StructKeyword,
        SyntaxKind.InterfaceKeyword,
        SyntaxKind.EnumKeyword,
        SyntaxKind.UnionKeyword,
        SyntaxKind.ExtensionKeyword,
        SyntaxKind.InitKeyword,
        SyntaxKind.FuncKeyword,
        SyntaxKind.IdentifierToken,
        SyntaxKind.LetKeyword,
        SyntaxKind.ValKeyword,
        SyntaxKind.VarKeyword,
        SyntaxKind.ConstKeyword,
        SyntaxKind.PublicKeyword,
        SyntaxKind.PrivateKeyword,
        SyntaxKind.InternalKeyword,
        SyntaxKind.ProtectedKeyword,
        SyntaxKind.StaticKeyword,
        SyntaxKind.AbstractKeyword,
        SyntaxKind.SealedKeyword,
        SyntaxKind.PartialKeyword,
        SyntaxKind.OpenKeyword,
        SyntaxKind.OverrideKeyword,
        SyntaxKind.AsyncKeyword,
        SyntaxKind.VirtualKeyword,
    };

    private static readonly HashSet<SyntaxKind> s_compilationUnitRecovery = new(s_typeMemberRecovery)
    {
        SyntaxKind.NamespaceKeyword,
        SyntaxKind.ImportKeyword,
        SyntaxKind.AliasKeyword,
    };

    private static readonly HashSet<SyntaxKind> s_expressionRecovery = new()
    {
        SyntaxKind.CommaToken,
        SyntaxKind.CloseParenToken,
        SyntaxKind.CloseBracketToken,
        SyntaxKind.CloseBraceToken,
        SyntaxKind.SemicolonToken,
        SyntaxKind.EndOfFileToken,
    };

    public static bool IsStatementTerminator(SyntaxKind kind) => s_statementTerminators.Contains(kind);

    public static bool IsStatementRecovery(SyntaxKind kind) => s_statementRecovery.Contains(kind);

    public static IReadOnlyCollection<SyntaxKind> StatementRecoveryKinds => s_statementRecovery;

    public static bool IsTypeMemberStartOrRecovery(SyntaxKind kind) => s_typeMemberRecovery.Contains(kind);

    public static IReadOnlyCollection<SyntaxKind> TypeMemberRecoveryKinds => s_typeMemberRecovery;

    public static bool IsCompilationUnitMemberStartOrRecovery(SyntaxKind kind) => s_compilationUnitRecovery.Contains(kind);

    public static IReadOnlyCollection<SyntaxKind> CompilationUnitRecoveryKinds => s_compilationUnitRecovery;

    public static IReadOnlyCollection<SyntaxKind> ExpressionRecoveryKinds => s_expressionRecovery;
}
