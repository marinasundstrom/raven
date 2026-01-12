namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class PatternSyntaxParser : SyntaxParser
{
    public PatternSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    public PatternSyntax ParsePattern()
    {
        return ParseOrPattern();
    }

    private PatternSyntax ParseOrPattern()
    {
        var left = ParseAndPattern();

        while (ConsumeToken(SyntaxKind.OrToken, out var orKeyword))
        {
            var right = ParseAndPattern();
            left = BinaryPattern(SyntaxKind.OrPattern, left, orKeyword, right);
        }

        return left;
    }

    private PatternSyntax ParseAndPattern()
    {
        var left = ParseUnaryPattern();

        while (ConsumeToken(SyntaxKind.AndToken, out var andKeyword))
        {
            var right = ParseUnaryPattern();
            left = BinaryPattern(SyntaxKind.AndPattern, left, andKeyword, right);
        }

        return left;
    }

    private PatternSyntax ParseUnaryPattern()
    {
        if (ConsumeToken(SyntaxKind.NotKeyword, out var notKeyword))
        {
            var operand = ParseUnaryPattern(); // Right-associative
            return UnaryPattern(SyntaxKind.NotPattern, notKeyword, operand);
        }

        return ParsePrimaryPattern();
    }

    private PatternSyntax ParsePrimaryPattern()
    {
        // Relational pattern: > expr, >= expr, < expr, <= expr
        if (IsRelationalPatternStart(PeekToken()))
            return ParseRelationalPattern();

        if (PeekToken().IsKind(SyntaxKind.DotToken))
        {
            return ParseCasePattern(qualifier: null, dotToken: ReadToken());
        }

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseTuplePattern();
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            if (TryParsePropertyPatternClause(out var clause))
            {
                // Allow an optional designation after a property pattern: `{ ... } name` / `{ ... } (a, b)`
                VariableDesignationSyntax? designation3 = null;
                if (CanTokenBeIdentifier(PeekToken()) || PeekToken().IsKind(SyntaxKind.OpenParenToken))
                    designation3 = ParseDesignation();

                return PropertyPattern(null, clause, designation3);
            }

            return CreateMissingPattern();
        }

        if (PeekToken().Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword)
        {
            return ParseVariablePattern();
        }

        if (PeekToken().Kind is SyntaxKind.UnderscoreToken)
        {
            var underscoreToken = ReadToken();
            return DiscardPattern(underscoreToken);
        }

        var type = new NameSyntaxParser(this).ParseTypeName();

        if (type is LiteralTypeSyntax)
        {
            return DeclarationPattern(type, SingleVariableDesignation(MissingToken(SyntaxKind.None)));
            //return ConstantPattern(type);
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            if (TryParsePropertyPatternClause(out var clause))
            {
                // Allow an optional designation after a property pattern: `Type { ... } name` / `Type { ... } (a, b)`
                VariableDesignationSyntax? designation2 = null;
                if (CanTokenBeIdentifier(PeekToken()) || PeekToken().IsKind(SyntaxKind.OpenParenToken))
                    designation2 = ParseDesignation();

                return PropertyPattern(type, clause, designation2);
            }

            return DeclarationPattern(type, SingleVariableDesignation(MissingToken(SyntaxKind.None)));
        }

        if (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            return ParseCasePattern(type, dotToken);
        }

        // Optionally consume a variable designation
        VariableDesignationSyntax designation;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            designation = ParseDesignation();
        }
        else
        {
            // TODO: Investigate
            designation = SingleVariableDesignation(Token(SyntaxKind.None));
        }

        return DeclarationPattern(type, designation);
    }

    private PropertyPatternClauseSyntax ParsePropertyPatternClause()
    {
        var openBraceToken = ReadToken(); // {

        var elements = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseBraceToken))
        {
            elements.Add(ParsePropertySubpattern());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elements.Add(commaToken);

                // Allow trailing comma before }
                if (PeekToken().IsKind(SyntaxKind.CloseBraceToken))
                    break;

                elements.Add(ParsePropertySubpattern());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBraceToken, out var closeBraceToken);

        return PropertyPatternClause(
            openBraceToken,
            List(elements.ToArray()),
            closeBraceToken);
    }

    private bool TryParsePropertyPatternClause(out PropertyPatternClauseSyntax clause)
    {
        clause = null!;

        if (!IsPropertyPatternClauseStart())
            return false;

        var checkpoint = CreateCheckpoint("property-pattern-clause");
        var parsedClause = ParsePropertyPatternClause();

        if (!IsValidPropertyPatternClause(parsedClause))
        {
            checkpoint.Dispose();
            return false;
        }

        clause = parsedClause;
        return true;
    }

    private bool IsPropertyPatternClauseStart()
    {
        if (!PeekToken().IsKind(SyntaxKind.OpenBraceToken))
            return false;

        var offset = 1;
        var token = PeekToken(offset);

        if (token.IsKind(SyntaxKind.CloseBraceToken))
            return true;

        if (token.IsKind(SyntaxKind.DotToken))
        {
            offset++;
            token = PeekToken(offset);
        }

        if (!CanTokenBeIdentifier(token))
            return false;

        return PeekToken(offset + 1).IsKind(SyntaxKind.ColonToken);
    }

    private static bool IsValidPropertyPatternClause(PropertyPatternClauseSyntax clause)
    {
        if (clause.OpenBraceToken.IsMissing || clause.CloseBraceToken.IsMissing)
            return false;

        var properties = clause.Properties;

        for (int i = 0; i < properties.SlotCount; i++)
        {
            var element = properties[i];
            if (element is not PropertySubpatternSyntax subpattern)
                continue;

            if (subpattern.NameColon.Name.Identifier.IsMissing || subpattern.NameColon.ColonToken.IsMissing)
                return false;
        }

        return true;
    }

    private PropertySubpatternSyntax ParsePropertySubpattern()
    {
        ConsumeToken(SyntaxKind.DotToken, out _);

        SyntaxToken nameToken;

        if (CanTokenBeIdentifier(PeekToken()))
        {
            nameToken = ReadToken();
            if (nameToken.Kind != SyntaxKind.IdentifierToken)
            {
                nameToken = ToIdentifierToken(nameToken);
                UpdateLastToken(nameToken);
            }
        }
        else
        {
            nameToken = ExpectToken(SyntaxKind.IdentifierToken);
        }

        ConsumeTokenOrMissing(SyntaxKind.ColonToken, out var colonToken);

        var nameColon = NameColon(IdentifierName(nameToken), colonToken);

        // IMPORTANT: RHS is a *pattern*, not an expression
        var pattern = ParsePattern();

        return PropertySubpattern(nameColon, pattern);
    }

    private static PatternSyntax CreateMissingPattern()
    {
        return DiscardPattern(MissingToken(SyntaxKind.UnderscoreToken));
    }

    private PatternSyntax ParseVariablePattern()
    {
        var bindingKeyword = ReadToken();
        var designation = ParseDesignation();
        return VariablePattern(bindingKeyword, designation);
    }

    private TuplePatternSyntax ParseTuplePattern()
    {
        var openParenToken = ReadToken();

        var elementList = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            elementList.Add(ParseTuplePatternElement());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elementList.Add(commaToken);
                elementList.Add(ParseTuplePatternElement());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return TuplePattern(openParenToken, List(elementList.ToArray()), closeParenToken);
    }

    private CasePatternSyntax ParseCasePattern(TypeSyntax? qualifier, SyntaxToken dotToken)
    {
        var identifierToken = ReadToken();
        if (identifierToken.Kind != SyntaxKind.IdentifierToken)
        {
            identifierToken = ToIdentifierToken(identifierToken);
            UpdateLastToken(identifierToken);
        }

        var argumentList = ParseCasePatternArgumentList();
        var path = CasePatternPath(qualifier, dotToken, identifierToken);

        return CasePattern(path, argumentList);
    }

    private CasePatternArgumentListSyntax? ParseCasePatternArgumentList()
    {
        if (!PeekToken().IsKind(SyntaxKind.OpenParenToken))
            return null;

        var openParenToken = ReadToken();

        var arguments = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            arguments.Add(new PatternSyntaxParser(this).ParsePattern());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                arguments.Add(commaToken);
                arguments.Add(new PatternSyntaxParser(this).ParsePattern());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return CasePatternArgumentList(openParenToken, List(arguments.ToArray()), closeParenToken);
    }

    private VariableDesignationSyntax ParseDesignation()
    {
        VariableDesignationSyntax designation;

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            designation = ParseParenthesizedDesignation();
        }
        else
        {
            SyntaxToken identifier;
            if (CanTokenBeIdentifier(PeekToken()))
            {
                identifier = ReadIdentifierToken();
            }
            else
            {
                identifier = ExpectToken(SyntaxKind.IdentifierToken);
            }

            designation = SingleVariableDesignation(identifier);
        }

        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            var type = new NameSyntaxParser(this).ParseTypeName();
            var typeAnnotation = TypeAnnotationClause(colonToken, type);
            designation = TypedVariableDesignation(designation, typeAnnotation);
        }

        return designation;
    }

    private VariableDesignationSyntax ParseParenthesizedDesignation()
    {
        var openParenToken = ReadToken();

        var elements = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            elements.Add(ParseDesignation());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elements.Add(commaToken);
                elements.Add(ParseDesignation());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ParenthesizedVariableDesignation(openParenToken, List(elements.ToArray()), closeParenToken);
    }

    private TuplePatternElementSyntax ParseTuplePatternElement()
    {
        NameColonSyntax? nameColon = null;

        if (PeekToken(1).IsKind(SyntaxKind.ColonToken) && CanTokenBeIdentifier(PeekToken()))
        {
            var nameToken = ReadToken();
            if (nameToken.Kind != SyntaxKind.IdentifierToken)
            {
                nameToken = ToIdentifierToken(nameToken);
                UpdateLastToken(nameToken);
            }

            var colonToken = ReadToken();
            nameColon = NameColon(IdentifierName(nameToken), colonToken);
        }

        var pattern = new PatternSyntaxParser(this).ParsePattern();
        return TuplePatternElement(nameColon, pattern);
    }

    private static bool IsRelationalPatternStart(SyntaxToken token)
    {
        return token.Kind is
            SyntaxKind.GreaterThanToken or
            SyntaxKind.GreaterThanOrEqualsToken or
            SyntaxKind.LessThanToken or
            SyntaxKind.LessThanOrEqualsToken;
    }

    private PatternSyntax ParseRelationalPattern()
    {
        // >, >=, <, <=
        var operatorToken = ReadToken();

        // Parse RHS as an EXPRESSION (not a pattern)
        // This allows: > 30, > x + 1, > Foo(3), etc.
        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        var kind = GetRelationalPatternKind(operatorToken.Kind);
        return RelationalPattern(kind, operatorToken, expression);
    }

    private static SyntaxKind GetRelationalPatternKind(SyntaxKind operatorTokenKind)
    {
        return operatorTokenKind switch
        {
            SyntaxKind.GreaterThanToken => SyntaxKind.GreaterThanPattern,
            SyntaxKind.GreaterThanOrEqualsToken => SyntaxKind.GreaterThanOrEqualPattern,
            SyntaxKind.LessThanToken => SyntaxKind.LessThanPattern,
            SyntaxKind.LessThanOrEqualsToken => SyntaxKind.LessThanOrEqualPattern,
            _ => throw new InvalidOperationException($"Unexpected relational operator token: {operatorTokenKind}")
        };
    }
}
