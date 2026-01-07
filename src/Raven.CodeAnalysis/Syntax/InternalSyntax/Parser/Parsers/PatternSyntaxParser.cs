
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
            var clause = ParsePropertyPatternClause();
            return PropertyPattern(null, clause);
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

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            var clause = ParsePropertyPatternClause();
            return PropertyPattern(type, clause);
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

    private PropertySubpatternSyntax ParsePropertySubpattern()
    {
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
}
