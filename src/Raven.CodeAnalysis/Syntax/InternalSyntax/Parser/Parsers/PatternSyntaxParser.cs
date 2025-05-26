
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
        if (ConsumeToken(SyntaxKind.NotKeyword, out var notKeyword))
        {
            return UnaryPattern(notKeyword, ParsePatternCore());
        }

        return ParsePatternCore();
    }

    private PatternSyntax ParsePatternCore()
    {
        var type = new NameSyntaxParser(this).ParseTypeName();

        var designation = ParseDesignation();

        return DeclarationPattern(type, designation);
    }

    private VariableDesignationSyntax ParseDesignation()
    {
        if (!ConsumeToken(SyntaxKind.IdentifierToken, out var identifier))
        {
            // Error.
        }
        return SingleVariableDesignation(identifier);
    }
}