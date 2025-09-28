
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
        var left = ParseUnaryPattern();

        while (ConsumeToken(SyntaxKind.OrToken, out var orKeyword))
        {
            var right = ParseUnaryPattern();
            left = BinaryPattern(SyntaxKind.OrPattern, left, orKeyword, right);
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
        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            return ParseTuplePattern();
        }

        var type = new NameSyntaxParser(this).ParseTypeName();

        // Optionally consume a variable designation
        VariableDesignationSyntax designation;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            designation = ParseDesignation();
        }
        else
        {
            designation = SingleVariableDesignation(MissingToken(SyntaxKind.IdentifierToken));
        }

        return DeclarationPattern(type, designation);
    }

    private TuplePatternSyntax ParseTuplePattern()
    {
        var openParenToken = ReadToken();

        var patternList = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            patternList.Add(new PatternSyntaxParser(this).ParsePattern());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                patternList.Add(commaToken);
                patternList.Add(new PatternSyntaxParser(this).ParsePattern());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return TuplePattern(openParenToken, List(patternList.ToArray()), closeParenToken);
    }

    private VariableDesignationSyntax ParseDesignation()
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
        return SingleVariableDesignation(identifier);
    }
}
