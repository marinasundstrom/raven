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
            return ParseMemberPattern(qualifier: null, dotToken: ReadToken());
        }

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            return ParsePositionalPattern();
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBracketToken))
        {
            return ParseCollectionPattern();
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            if (TryParsePropertyPatternClause(out var clause))
            {
                // Allow an optional designation after a property pattern: `{ ... } name` / `{ ... } (a, b)`
                VariableDesignationSyntax? designation2 = null;

                var next = PeekToken();
                var canStartDesignation =
                    CanTokenBeIdentifier(next) ||
                    next.IsKind(SyntaxKind.OpenParenToken) ||
                    next.Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword;

                if (canStartDesignation)
                    designation2 = ParseDesignation();

                return PropertyPattern(null, clause, designation2);
            }

            return CreateMissingPattern();
        }

        if (PeekToken().Kind == SyntaxKind.NullKeyword)
        {
            var type2 = new NameSyntaxParser(this).ParseTypeName();

            return ConstantPattern(type2);
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

        // Lower (or lower+upper) range pattern: `lo..hi` / `lo..` (e.g. `2..3`, `2..`, `x..y`)
        // We only take this fast-path when we can see the `..` token immediately after the lower bound.
        if (CanStartRangeBoundExpression(PeekToken()) && PeekToken(1).IsKind(SyntaxKind.DotDotToken))
        {
            var lowerBound = new ExpressionSyntaxParser(this, stopAtDotDotToken: true).ParseExpression();
            return ParseRangePattern(lowerBound);
        }

        // Upper-only range pattern: `..hi` (e.g. `..9`, `..100`)
        if (PeekToken().IsKind(SyntaxKind.DotDotToken))
        {
            return ParseRangePattern(lowerBound: null);
        }

        // Constant pattern (expression pattern): allow matching against an in-scope value
        // e.g. `{ Product: discountedProduct, Quantity: > 10 }`
        // We parse it as an expression pattern here and let binding decide if it is a type-name or a value.
        if (CanStartConstantPatternExpression(PeekToken()))
        {
            if (TryParseConstantPattern(out var constantPattern))
                return constantPattern;
        }

        var type = new NameSyntaxParser(this).ParseTypeName();

        if (type is LiteralTypeSyntax)
        {
            return ConstantPattern(type);
        }

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            var argumentList = ParseRecordPatternArgumentList();
            return RecordPattern(type, argumentList);
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            if (TryParsePropertyPatternClause(out var clause))
            {
                // Allow an optional designation after a property pattern: `Type { ... } name` / `Type { ... } (a, b)`
                VariableDesignationSyntax? designation2 = null;
                var next = PeekToken();
                var canStartDesignation =
                    CanTokenBeIdentifier(next) ||
                    next.IsKind(SyntaxKind.OpenParenToken) ||
                    next.Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword;

                if (canStartDesignation)
                    designation2 = ParseDesignation();

                return PropertyPattern(type, clause, designation2);
            }

            return DeclarationPattern(type, null);
        }

        if (ConsumeToken(SyntaxKind.DotToken, out var dotToken))
        {
            return ParseMemberPattern(type, dotToken);
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
            designation = SingleVariableDesignation(Token(SyntaxKind.None), Token(SyntaxKind.None));
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
            checkpoint.Rewind();
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
        var designation = ParseDesignation(allowBindingKeyword: false);
        return VariablePattern(bindingKeyword, designation);
    }

    private PositionalPatternSyntax ParsePositionalPattern()
    {
        var openParenToken = ReadToken();

        var elementList = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            elementList.Add(ParsePositionalPatternElement());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elementList.Add(commaToken);
                elementList.Add(ParsePositionalPatternElement());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return PositionalPattern(openParenToken, List(elementList.ToArray()), closeParenToken);
    }

    private PositionalPatternSyntax ParseCollectionPattern()
    {
        var openBracketToken = ReadToken();

        var elementList = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseBracketToken))
        {
            elementList.Add(ParsePositionalPatternElement());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elementList.Add(commaToken);

                if (PeekToken().IsKind(SyntaxKind.CloseBracketToken))
                    break;

                elementList.Add(ParsePositionalPatternElement());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);

        return PositionalPattern(openBracketToken, List(elementList.ToArray()), closeBracketToken);
    }

    private MemberPatternSyntax ParseMemberPattern(TypeSyntax? qualifier, SyntaxToken dotToken)
    {
        var identifierToken = ReadToken();
        if (identifierToken.Kind != SyntaxKind.IdentifierToken)
        {
            identifierToken = ToIdentifierToken(identifierToken);
            UpdateLastToken(identifierToken);
        }

        var argumentList = ParseMemberPatternArgumentList();
        var path = MemberPatternPath(qualifier, dotToken, identifierToken);

        return MemberPattern(path, argumentList);
    }

    private MemberPatternArgumentListSyntax? ParseMemberPatternArgumentList()
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

        return MemberPatternArgumentList(openParenToken, List(arguments.ToArray()), closeParenToken);
    }

    private RecordPatternArgumentListSyntax ParseRecordPatternArgumentList()
    {
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

        return RecordPatternArgumentList(openParenToken, List(arguments.ToArray()), closeParenToken);
    }

    private VariableDesignationSyntax ParseDesignation(bool allowBindingKeyword = true)
    {
        VariableDesignationSyntax designation;

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            designation = ParseParenthesizedDesignation(allowBindingKeyword);
        }
        else
        {
            SyntaxToken bindingKeyword = Token(SyntaxKind.None);
            if (allowBindingKeyword && PeekToken().Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword)
                bindingKeyword = ReadToken();

            SyntaxToken identifier;
            if (PeekToken().Kind == SyntaxKind.UnderscoreToken)
            {
                var underscore = ReadToken();
                identifier = ToIdentifierToken(underscore);
                UpdateLastToken(identifier);
            }
            else if (CanTokenBeIdentifier(PeekToken()))
            {
                identifier = ReadIdentifierToken();
            }
            else
            {
                identifier = ExpectToken(SyntaxKind.IdentifierToken);
            }

            designation = SingleVariableDesignation(bindingKeyword, identifier);
        }

        if (ConsumeToken(SyntaxKind.ColonToken, out var colonToken))
        {
            var type = new NameSyntaxParser(this).ParseTypeName();
            var typeAnnotation = TypeAnnotationClause(colonToken, type);
            designation = TypedVariableDesignation(designation, typeAnnotation);
        }

        return designation;
    }

    private VariableDesignationSyntax ParseParenthesizedDesignation(bool allowBindingKeyword)
    {
        var openParenToken = ReadToken();

        var elements = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            elements.Add(ParseDesignation(allowBindingKeyword));

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elements.Add(commaToken);
                elements.Add(ParseDesignation(allowBindingKeyword));
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return ParenthesizedVariableDesignation(openParenToken, List(elements.ToArray()), closeParenToken);
    }

    private PositionalPatternElementSyntax ParsePositionalPatternElement()
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
        return PositionalPatternElement(nameColon, pattern);
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

    // Range pattern: `lo..hi`, `lo..`, `..hi`
    // Called either with a pre-parsed lower bound (from TryParseConstantPattern) or null (for `..hi`).
    private PatternSyntax ParseRangePattern(ExpressionSyntax? lowerBound)
    {
        var dotDotToken = ReadToken(); // consume `..`

        // Parse upper bound if present (any primary expression that isn't another pattern start)
        ExpressionSyntax? upperBound = null;
        if (CanStartRangeBoundExpression(PeekToken()))
        {
            upperBound = new ExpressionSyntaxParser(this).ParseExpression();
        }

        return RangePattern(lowerBound, dotDotToken, upperBound);
    }

    private static bool CanStartRangeBoundExpression(SyntaxToken token)
    {
        // A range bound is a simple expression that can appear around `..` in a range pattern.
        // Keep this conservative: literals and identifiers (constants/values), plus unary `-` for negative literals.
        return token.Kind switch
        {
            SyntaxKind.NumericLiteralToken => true,
            SyntaxKind.CharacterLiteralToken => true,
            SyntaxKind.MinusToken => true, // negative literals like `..-1`
            SyntaxKind.IdentifierToken => true,
            _ => false
        };
    }

    // Helper for constant pattern expressions (identifiers, member access, etc.)
    private bool CanStartConstantPatternExpression(SyntaxToken token)
    {
        // We only attempt this for identifiers (locals/fields/params/constants) and member-access roots.
        // Other literals are handled elsewhere (e.g. relational patterns and existing literal handling).
        return CanTokenBeIdentifier(token);
    }

    private bool TryParseConstantPattern(out PatternSyntax constantPattern)
    {
        constantPattern = null!;

        if (LooksLikeTypePatternStart())
            return false;

        // Speculative parse to avoid stealing input from other pattern forms.
        var checkpoint = CreateCheckpoint("constant-pattern");

        // Parse as expression (NOT a pattern). This enables: `x`, `x.y`, `SomeType.StaticField`, etc.
        // NOTE: The expression parser will also consume `lo..hi` as a RangeExpression.
        var expr = new ExpressionSyntaxParser(this).ParseExpression();

        // If the expression is a range expression, convert it to a RangePatternSyntax.
        if (expr is RangeExpressionSyntax rangeExpr)
        {
            if (!IsPatternTerminator(PeekToken()))
            {
                checkpoint.Rewind();
                return false;
            }

            constantPattern = RangePattern(rangeExpr.LeftExpression, rangeExpr.DotDotToken, rangeExpr.RightExpression);
            return true;
        }

        if (!IsValidConstantPatternExpression(expr))
        {
            checkpoint.Rewind();
            return false;
        }

        // Only accept if the next token can legally terminate a pattern at this precedence level.
        // This avoids capturing type declarations like `Foo bar`.
        if (!IsPatternTerminator(PeekToken()))
        {
            checkpoint.Rewind();
            return false;
        }

        // NOTE: The binder should decide whether `expr` is a constant/readonly value, enum member,
        // static field, etc. If it binds to a type, it can be interpreted as a type/declaration pattern.
        constantPattern = ConstantPattern(expr);
        return true;
    }

    private bool LooksLikeTypePatternStart()
    {
        var next = PeekToken(1);
        return next.IsKind(SyntaxKind.OpenParenToken) ||
            next.IsKind(SyntaxKind.OpenBraceToken);
    }

    private static bool IsValidConstantPatternExpression(ExpressionSyntax expression)
    {
        return expression switch
        {
            IdentifierNameSyntax => true,
            MemberAccessExpressionSyntax => true,
            _ => false
        };
    }

    private bool IsPatternTerminator(SyntaxToken token)
    {
        // Tokens that end the current pattern or separate patterns.
        return token.Kind is
            SyntaxKind.CommaToken or
            SyntaxKind.CloseParenToken or
            SyntaxKind.CloseBraceToken or
            SyntaxKind.AndToken or
            SyntaxKind.OrToken;
    }
}
