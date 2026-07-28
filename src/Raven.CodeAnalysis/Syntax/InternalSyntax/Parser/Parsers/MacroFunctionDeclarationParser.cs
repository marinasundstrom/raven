namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Linq;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal sealed class MacroFunctionDeclarationParser : SyntaxParser
{
    public MacroFunctionDeclarationParser(ParseContext parent)
        : base(parent)
    {
    }

    public bool IsDeclarationStart()
        => IsMacroKeyword(PeekToken()) &&
           PeekToken(1).IsKind(SyntaxKind.FuncKeyword);

    public MacroFunctionDeclarationSyntax Parse(
        SyntaxList attributeLists,
        SyntaxList modifiers)
    {
        if (!IsDeclarationStart())
            throw new InvalidOperationException("The current token does not begin a macro function declaration.");

        var macroKeyword = ReadToken();
        var funcKeyword = ExpectToken(SyntaxKind.FuncKeyword);
        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            identifier = ReadIdentifierToken();
        }
        else
        {
            identifier = MissingToken(SyntaxKind.IdentifierToken);
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.IdentifierExpected,
                    GetEndOfLastToken()));
        }

        TypeParameterListSyntax? typeParameterList = null;
        if (IsNextToken(SyntaxKind.LessThanToken, out _))
        {
            var typeParameterParser = new TypeDeclarationParser(this);
            typeParameterList = typeParameterParser.ParseTypeParameterList();
        }

        var parameterList = new StatementSyntaxParser(this).ParseParameterList();
        var returnType = new TypeAnnotationClauseSyntaxParser(this).ParseReturnTypeAnnotation();
        var constraintClauses = new ConstrainClauseListParser(this).ParseConstraintClauseList();
        var isExtern = modifiers.GetChildren().Any(child => child.IsKind(SyntaxKind.ExternKeyword));

        BlockStatementSyntax? body = null;
        ArrowExpressionClauseSyntax? expressionBody = null;
        if (IsNextToken(SyntaxKind.OpenBraceToken, out _))
        {
            body = new StatementSyntaxParser(this).ParseBlockStatementSyntax();
        }
        else if (IsNextToken(SyntaxKind.FatArrowToken, out _))
        {
            expressionBody = new ExpressionSyntaxParser(this).ParseArrowExpressionClause();
        }
        else if (!isExtern)
        {
            body = new StatementSyntaxParser(this).ParseBlockStatementSyntax();
        }

        TryConsumeTerminator(out var terminatorToken);

        return MacroFunctionDeclaration(
            attributeLists,
            modifiers,
            macroKeyword,
            funcKeyword,
            identifier,
            typeParameterList,
            parameterList,
            returnType,
            constraintClauses,
            body,
            expressionBody,
            terminatorToken);
    }

    internal static bool IsMacroKeyword(SyntaxToken token)
        => token.IsKind(SyntaxKind.IdentifierToken) &&
           string.Equals(token.GetValueText(), "macro", StringComparison.Ordinal);
}
