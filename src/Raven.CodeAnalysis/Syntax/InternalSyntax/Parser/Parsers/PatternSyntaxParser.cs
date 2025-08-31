
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;

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
        var type = new NameSyntaxParser(this).ParseTypeName();

        // Optionally consume a variable designation
        VariableDesignationSyntax? designation = null;
        if (CanTokenBeIdentifier(PeekToken()))
        {
            designation = ParseDesignation();
        }

        return DeclarationPattern(type, designation);
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
