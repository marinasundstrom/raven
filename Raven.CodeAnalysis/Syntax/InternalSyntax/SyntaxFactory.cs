﻿namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public static class SyntaxFactory
{
    public static readonly SyntaxToken OpenParenToken = new SyntaxToken(SyntaxKind.OpenParenToken, "(");

    public static readonly SyntaxToken CloseParenToken = new SyntaxToken(SyntaxKind.CloseParenToken, ")");

    public static readonly SyntaxToken OpenBraceToken = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");

    public static readonly SyntaxToken CloseBraceToken = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");

    public static readonly SyntaxToken GreaterThanToken = new SyntaxToken(SyntaxKind.GreaterThanToken, ">");

    public static readonly SyntaxToken IfKeyword = new SyntaxToken(SyntaxKind.IfKeyword, "if");

    public static SyntaxToken Identifier(string text) => new SyntaxToken(SyntaxKind.IdentifierToken, text);
}