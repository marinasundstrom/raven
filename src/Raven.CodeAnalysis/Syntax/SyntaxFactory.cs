using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => (SyntaxToken)InternalSyntax.SyntaxFactory.MissingToken(kind);

    public static SyntaxToken IdentifierToken(string text) => (SyntaxToken)InternalSyntax.SyntaxFactory.IdentifierToken(text);
    public static SyntaxToken NumericLiteral(int value) => (SyntaxToken)InternalSyntax.SyntaxFactory.NumericLiteral(value);

    public static readonly SyntaxToken VoidKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.VoidKeyword;
    public static readonly SyntaxToken NullKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.NullKeyword;
    public static readonly SyntaxToken IntKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.IntKeyword;
    public static readonly SyntaxToken StringKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.StringKeyword;
    public static readonly SyntaxToken CharKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.CharKeyword;
    public static readonly SyntaxToken ImportKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.ImportKeyword;
    public static readonly SyntaxToken NamespaceKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.NamespaceKeyword;
    public static readonly SyntaxToken LetKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.LetKeyword;
    public static readonly SyntaxToken VarKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.VarKeyword;
    public static readonly SyntaxToken EnumKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.EnumKeyword;
    public static readonly SyntaxToken IfKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.IfKeyword;
    public static readonly SyntaxToken ElseKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.ElseKeyword;
    public static readonly SyntaxToken WhileKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.WhileKeyword;
    public static readonly SyntaxToken ReturnKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.ReturnKeyword;
    public static readonly SyntaxToken NewKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.NewKeyword;
    public static readonly SyntaxToken TrueKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.TrueKeyword;
    public static readonly SyntaxToken FalseKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.FalseKeyword;
    public static readonly SyntaxToken IsKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.IsKeyword;
    public static readonly SyntaxToken NotKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.NotKeyword;
    public static readonly SyntaxToken AndKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.AndKeyword;
    public static readonly SyntaxToken OrKeyword = (SyntaxToken)InternalSyntax.SyntaxFactory.OrKeyword;

    public static readonly SyntaxToken OrToken = (SyntaxToken)InternalSyntax.SyntaxFactory.OrToken;
    public static readonly SyntaxToken AndToken = (SyntaxToken)InternalSyntax.SyntaxFactory.AndToken;

    public static readonly SyntaxToken OpenParenToken = (SyntaxToken)InternalSyntax.SyntaxFactory.OpenParenToken;
    public static readonly SyntaxToken CloseParenToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CloseParenToken;
    public static readonly SyntaxToken OpenBraceToken = (SyntaxToken)InternalSyntax.SyntaxFactory.OpenBraceToken;
    public static readonly SyntaxToken CloseBraceToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CloseBraceToken;
    public static readonly SyntaxToken OpenBracketToken = (SyntaxToken)InternalSyntax.SyntaxFactory.OpenBracketToken;
    public static readonly SyntaxToken CloseBracketToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CloseBracketToken;

    public static readonly SyntaxToken GreaterThanToken = (SyntaxToken)InternalSyntax.SyntaxFactory.GreaterThanToken;
    public static readonly SyntaxToken LessThanToken = (SyntaxToken)InternalSyntax.SyntaxFactory.LessThanToken;
    public static readonly SyntaxToken GreaterOrEqualsToken = (SyntaxToken)InternalSyntax.SyntaxFactory.GreaterOrEqualsToken;
    public static readonly SyntaxToken LessThanEqualsToken = (SyntaxToken)InternalSyntax.SyntaxFactory.LessThanEqualsToken;

    public static readonly SyntaxToken EqualsToken = (SyntaxToken)InternalSyntax.SyntaxFactory.EqualsToken;
    public static readonly SyntaxToken EqualsEqualsToken = (SyntaxToken)InternalSyntax.SyntaxFactory.EqualsEqualsToken;
    public static readonly SyntaxToken NotEqualsToken = (SyntaxToken)InternalSyntax.SyntaxFactory.NotEqualsToken;
    public static readonly SyntaxToken PlusToken = (SyntaxToken)InternalSyntax.SyntaxFactory.PlusToken;
    public static readonly SyntaxToken MinusToken = (SyntaxToken)InternalSyntax.SyntaxFactory.MinusToken;
    public static readonly SyntaxToken PercentToken = (SyntaxToken)InternalSyntax.SyntaxFactory.PercentToken;
    public static readonly SyntaxToken SlashToken = (SyntaxToken)InternalSyntax.SyntaxFactory.SlashToken;
    public static readonly SyntaxToken StarToken = (SyntaxToken)InternalSyntax.SyntaxFactory.StarToken;
    public static readonly SyntaxToken DotToken = (SyntaxToken)InternalSyntax.SyntaxFactory.DotToken;
    public static readonly SyntaxToken CaretToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CaretToken;
    public static readonly SyntaxToken ExclamationToken = (SyntaxToken)InternalSyntax.SyntaxFactory.ExclamationToken;
    public static readonly SyntaxToken QuestionToken = (SyntaxToken)InternalSyntax.SyntaxFactory.QuestionToken;

    public static readonly SyntaxToken ArrowToken = (SyntaxToken)InternalSyntax.SyntaxFactory.ArrowToken;

    public static readonly SyntaxToken CommaToken = (SyntaxToken)InternalSyntax.SyntaxFactory.CommaToken;
    public static readonly SyntaxToken ColonToken = (SyntaxToken)InternalSyntax.SyntaxFactory.ColonToken;
    public static readonly SyntaxToken SemicolonToken = (SyntaxToken)InternalSyntax.SyntaxFactory.SemicolonToken;

    
    public static readonly SyntaxToken NewLineToken = (SyntaxToken)InternalSyntax.SyntaxFactory.NewLineToken;

    
    public static readonly SyntaxToken EndOfFile = (SyntaxToken)InternalSyntax.SyntaxFactory.EndOfFile;

}