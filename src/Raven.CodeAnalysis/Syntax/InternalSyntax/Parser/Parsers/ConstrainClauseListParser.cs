namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System.Collections.Generic;

using static SyntaxFactory;

internal class ConstrainClauseListParser : SyntaxParser
{
    public ConstrainClauseListParser(ParseContext context) : base(context)
    {

    }

    public SyntaxList ParseConstraintClauseList()
    {
        // Allow constraints after type parameter list / parameter list, typically before body/brace/=>/terminator.
        // We’ll accept multiple clauses:
        //   where T : class, IDisposable
        //   where U : struct
        //
        // Return null when none are present.

        if (!PeekToken().IsKind(SyntaxKind.WhereKeyword))
            return List([]);

        var restoreNewlinesAsTokens = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false); // 'where' often starts on a new line; keep it easy.

        try
        {
            var clauses = new List<GreenNode>();

            while (PeekToken().IsKind(SyntaxKind.WhereKeyword))
            {
                var clause = ParseConstraintClause();
                clauses.Add(clause);

                // Optional: allow a newline/terminator between consecutive where-clauses,
                // but do not *require* it.
                //
                // If you want to be strict later, add a diagnostic here when the next token is not newline-like.
            }

            return List(clauses);
        }
        finally
        {
            SetTreatNewlinesAsTokens(restoreNewlinesAsTokens);
        }
    }

    private TypeParameterConstraintClauseSyntax ParseConstraintClause()
    {
        // where <TypeParameter> : <constraints>

        var whereKeyword = ReadToken(); // WhereKeyword

        // Parse type parameter name
        SyntaxToken identifier;
        if (CanTokenBeIdentifier(PeekToken()))
            identifier = ReadIdentifierToken();
        else
            identifier = ExpectToken(SyntaxKind.IdentifierToken);

        // If you modeled TypeParameter as IdentifierName:
        var typeParameter = IdentifierName(identifier);

        ConsumeTokenOrMissing(SyntaxKind.ColonToken, out var colonToken);

        // Parse constraints list: constraint (',' constraint)*
        var constraintNodes = new List<GreenNode>();

        // Small robustness: if ':' missing, still try to recover by parsing at least one constraint
        // when the next token looks constraint-ish.
        if (colonToken.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.CharacterExpected,
                    GetSpanOfLastToken(),
                    ":"));
        }

        // Reuse the same constraint parser you already use in inline constraints.
        // We’ll stop on tokens that clearly end a clause/declaration.
        while (true)
        {
            // Skip newlines between constraints (handy in "where T :\n class, ...")
            var t = PeekToken();
            while (IsNewLineLike(t))
            {
                ReadToken();
                t = PeekToken();
            }

            if (IsEndOfConstraintClause(t))
                break;

            var constraint = ParseTypeParameterConstraint();
            constraintNodes.Add(constraint);

            t = PeekToken();

            if (!t.IsKind(SyntaxKind.CommaToken))
                break;

            // consume comma
            var comma = ReadToken();
            constraintNodes.Add(comma);
        }

        var constraints = List(constraintNodes);

        return TypeParameterConstraintClause(whereKeyword, typeParameter, colonToken, constraints, Diagnostics);
    }

    public TypeParameterConstraintSyntax ParseTypeParameterConstraint()
    {
        var token = PeekToken();

        if (token.IsKind(SyntaxKind.ClassKeyword))
        {
            var classKeyword = ReadToken();
            return ClassConstraint(classKeyword);
        }

        if (token.IsKind(SyntaxKind.StructKeyword))
        {
            var structKeyword = ReadToken();
            return StructConstraint(structKeyword);
        }

        if (token.IsKind(SyntaxKind.NewKeyword))
        {
            var newKeyword = ReadToken();
            if (!ConsumeTokenOrMissing(SyntaxKind.OpenParenToken, out var openParen))
            {
                AddDiagnostic(
                          DiagnosticInfo.Create(
                              CompilerDiagnostics.CharacterExpected,
                              GetSpanOfLastToken(),
                              '('));
            }
            if (!ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParen))
            {
                AddDiagnostic(
                    DiagnosticInfo.Create(
                        CompilerDiagnostics.CharacterExpected,
                        GetSpanOfLastToken(),
                        "operator"));
            }
            return ConstructorConstraint(newKeyword, openParen, closeParen);
        }

        var type = new NameSyntaxParser(this).ParseTypeName();
        return TypeConstraint(type);
    }

    private static bool IsEndOfConstraintClause(SyntaxToken t)
    {
        // Tokens that end the constraint clause or the containing declaration.
        // Tune as needed (this is conservative and works well for recovery).
        return t.Kind is
            SyntaxKind.EndOfFileToken or
            SyntaxKind.OpenBraceToken or
            SyntaxKind.CloseBraceToken or
            SyntaxKind.SemicolonToken or
            SyntaxKind.NewLineToken or
            SyntaxKind.LineFeedToken or
            SyntaxKind.CarriageReturnToken or
            SyntaxKind.CarriageReturnLineFeedToken or
            SyntaxKind.FatArrowToken;
    }

    private static bool IsNewLineLike(SyntaxToken token)
    {
        return token.Kind is SyntaxKind.NewLineToken or SyntaxKind.LineFeedToken or SyntaxKind.CarriageReturnToken or SyntaxKind.CarriageReturnLineFeedToken;
    }
}
