namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Linq;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal sealed class MacroDeclarationParser : SyntaxParser
{
    public MacroDeclarationParser(ParseContext parent)
        : base(parent)
    {
    }

    public override bool IsInMacro => true;

    public bool IsDeclarationStart()
        => IsMacroKeyword(PeekToken()) && IsDeclarationNameOrRecoveryStart(1);

    public bool IsDeclarationStartAfterModifiers()
    {
        var offset = 0;
        while (IsDeclarationModifier(PeekToken(offset).Kind))
            offset++;

        return IsMacroKeyword(PeekToken(offset)) && IsDeclarationNameOrRecoveryStart(offset + 1);
    }

    public MacroDeclarationSyntax Parse(
        SyntaxList attributeLists,
        SyntaxList modifiers)
    {
        if (!IsDeclarationStart())
            throw new InvalidOperationException("The current token does not begin a macro declaration.");

        var macroKeyword = ReadToken();
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

        var parameterList = new StatementSyntaxParser(this).ParseParameterList(
            allowMacroTargetModifier: true);

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

        return MacroDeclaration(
            attributeLists,
            modifiers,
            macroKeyword,
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
        => IsContextualKeyword(token, "macro");

    private bool IsDeclarationNameOrRecoveryStart(int offset)
    {
        var token = PeekToken(offset);
        return CanTokenBeIdentifier(token) || token.IsKind(SyntaxKind.OpenParenToken);
    }

    private static bool IsDeclarationModifier(SyntaxKind kind)
        => kind is SyntaxKind.PublicKeyword or
            SyntaxKind.PrivateKeyword or
            SyntaxKind.InternalKeyword or
            SyntaxKind.ProtectedKeyword or
            SyntaxKind.FileprivateKeyword or
            SyntaxKind.StaticKeyword or
            SyntaxKind.RefKeyword or
            SyntaxKind.ReadonlyKeyword or
            SyntaxKind.AbstractKeyword or
            SyntaxKind.FinalKeyword or
            SyntaxKind.SealedKeyword or
            SyntaxKind.PartialKeyword or
            SyntaxKind.VirtualKeyword or
            SyntaxKind.AsyncKeyword or
            SyntaxKind.OpenKeyword or
            SyntaxKind.RecordKeyword or
            SyntaxKind.OverrideKeyword;

    private static bool IsContextualKeyword(SyntaxToken token, string value)
        => token.IsKind(SyntaxKind.IdentifierToken) &&
           string.Equals(token.GetValueText(), value, StringComparison.Ordinal);
}
