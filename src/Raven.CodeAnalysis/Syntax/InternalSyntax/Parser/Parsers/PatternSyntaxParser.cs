namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class PatternSyntaxParser : SyntaxParser
{
    private readonly bool _allowImplicitDeconstructionElementBindings;
    private readonly bool _allowWholePatternDesignation;

    public PatternSyntaxParser(
        ParseContext parent,
        bool allowImplicitDeconstructionElementBindings = false,
        bool allowWholePatternDesignation = true) : base(parent)
    {
        _allowImplicitDeconstructionElementBindings = allowImplicitDeconstructionElementBindings;
        _allowWholePatternDesignation = allowWholePatternDesignation;
    }

    public PatternSyntax ParsePattern()
    {
        return ParseOrPattern();
    }

    private PatternSyntax ParseOrPattern()
    {
        var left = ParseAndPattern();

        while (ConsumeToken(SyntaxKind.OrToken, out var orKeyword) ||
               ConsumeToken(SyntaxKind.BarToken, out orKeyword))
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
        // Comparison pattern: == expr, != expr, > expr, >= expr, < expr, <= expr
        if (IsComparisonPatternStart(PeekToken()))
            return ParseComparisonPattern();

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
            return ParseSequencePattern();
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            if (TryParsePropertyPatternClause(out var clause))
            {
                return PropertyPattern(null, clause, ParseOptionalTrailingDesignation());
            }

            return CreateMissingPattern();
        }

        if (PeekToken().Kind == SyntaxKind.NullKeyword)
        {
            var nullToken = ReadToken();
            return ConstantPattern(LiteralExpression(SyntaxKind.NullLiteralExpression, nullToken));
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

        if (TryParseImplicitBindingPattern(out var implicitBindingPattern))
            return implicitBindingPattern;

        // Constant pattern (expression pattern): allow matching against an in-scope value
        // e.g. `{ Product: discountedProduct, Quantity: > 10 }`
        // We parse it as an expression pattern here and let binding decide if it is a type-name or a value.
        if (CanStartConstantPatternExpression(PeekToken()))
        {
            if (TryParseConstantPattern(out var constantPattern))
                return constantPattern;
        }

        var type = new NameSyntaxParser(this).ParseTypeName();

        if (PeekToken().IsKind(SyntaxKind.OpenParenToken))
        {
            var argumentList = ParseNominalDeconstructionPatternArgumentList();
            return NominalDeconstructionPattern(type, argumentList, ParseOptionalTrailingDesignation());
        }

        if (PeekToken().IsKind(SyntaxKind.OpenBraceToken))
        {
            if (TryParsePropertyPatternClause(out var clause))
            {
                return PropertyPattern(type, clause, ParseOptionalTrailingDesignation());
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
        var designation = ParseOptionalTrailingDesignation();

        return PositionalPattern(openParenToken, List(elementList.ToArray()), closeParenToken, designation);
    }

    private SequencePatternSyntax ParseSequencePattern()
    {
        var openBracketToken = ReadToken();

        var elementList = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseBracketToken))
        {
            elementList.Add(ParseSequencePatternElement());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                elementList.Add(commaToken);

                if (PeekToken().IsKind(SyntaxKind.CloseBracketToken))
                    break;

                elementList.Add(ParseSequencePatternElement());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseBracketToken, out var closeBracketToken);
        var designation = ParseOptionalTrailingDesignation();

        return SequencePattern(openBracketToken, List(elementList.ToArray()), closeBracketToken, designation);
    }

    private SequencePatternElementSyntax ParseSequencePatternElement()
    {
        if (!TryConsumeSequenceRestToken(out var dotDotToken))
        {
            return SequencePatternElement(SequencePatternPrefix(Token(SyntaxKind.None), Token(SyntaxKind.None)), ParseDeconstructionElementPattern());
        }

        var segmentLengthToken = Token(SyntaxKind.None);
        if (dotDotToken.Kind == SyntaxKind.DotDotToken &&
            PeekToken().Kind == SyntaxKind.NumericLiteralToken)
        {
            segmentLengthToken = ReadToken();
        }

        var pattern = ParseDeconstructionElementPattern();
        return SequencePatternElement(SequencePatternPrefix(dotDotToken, segmentLengthToken), pattern);
    }

    private bool TryConsumeSequenceRestToken(out SyntaxToken token)
    {
        if (ConsumeToken(SyntaxKind.DotDotDotToken, out token))
            return true;

        return ConsumeToken(SyntaxKind.DotDotToken, out token);
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
        var designation = ParseOptionalTrailingDesignation();

        return MemberPattern(path, argumentList, designation);
    }

    private MemberPatternArgumentListSyntax? ParseMemberPatternArgumentList()
    {
        if (!PeekToken().IsKind(SyntaxKind.OpenParenToken))
            return null;

        var openParenToken = ReadToken();

        var arguments = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            arguments.Add(ParseDeconstructionElementPattern());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                arguments.Add(commaToken);
                arguments.Add(ParseDeconstructionElementPattern());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return MemberPatternArgumentList(openParenToken, List(arguments.ToArray()), closeParenToken);
    }

    private NominalDeconstructionPatternArgumentListSyntax ParseNominalDeconstructionPatternArgumentList()
    {
        var openParenToken = ReadToken();

        var arguments = new List<GreenNode>();

        if (!PeekToken().IsKind(SyntaxKind.CloseParenToken))
        {
            arguments.Add(ParseDeconstructionElementPattern());

            while (ConsumeToken(SyntaxKind.CommaToken, out var commaToken))
            {
                arguments.Add(commaToken);
                arguments.Add(ParseDeconstructionElementPattern());
            }
        }

        ConsumeTokenOrMissing(SyntaxKind.CloseParenToken, out var closeParenToken);

        return NominalDeconstructionPatternArgumentList(openParenToken, List(arguments.ToArray()), closeParenToken);
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

        var pattern = ParseDeconstructionElementPattern();
        return PositionalPatternElement(nameColon, pattern);
    }

    private PatternSyntax ParseDeconstructionElementPattern()
    {
        if (_allowImplicitDeconstructionElementBindings &&
            CanTokenBeIdentifier(PeekToken()))
        {
            var identifier = ReadIdentifierToken();
            return VariablePattern(
                Token(SyntaxKind.None),
                SingleVariableDesignation(Token(SyntaxKind.None), identifier));
        }

        return new PatternSyntaxParser(this, _allowImplicitDeconstructionElementBindings, _allowWholePatternDesignation).ParsePattern();
    }

    private bool TryParseImplicitBindingPattern(out PatternSyntax pattern)
    {
        pattern = null!;

        if (!_allowImplicitDeconstructionElementBindings || !CanTokenBeIdentifier(PeekToken()))
            return false;

        var nextKind = PeekToken(1).Kind;
        if (nextKind is SyntaxKind.OpenParenToken or SyntaxKind.OpenBraceToken or SyntaxKind.DotToken)
            return false;

        // Preserve declaration/type-pattern forms like `Type name` by only taking this path
        // when the identifier is immediately followed by a pattern delimiter or a type annotation.
        if (CanTokenBeIdentifier(PeekToken(1)))
            return false;

        if (nextKind is not
            (SyntaxKind.ColonToken or
             SyntaxKind.EqualsToken or
             SyntaxKind.InKeyword or
             SyntaxKind.FatArrowToken or
             SyntaxKind.WhenKeyword or
             SyntaxKind.CommaToken or
             SyntaxKind.CloseParenToken or
             SyntaxKind.CloseBracketToken or
             SyntaxKind.CloseBraceToken or
             SyntaxKind.EndOfFileToken))
        {
            return false;
        }

        var designation = ParseDesignation(allowBindingKeyword: false);
        pattern = VariablePattern(Token(SyntaxKind.None), designation);
        return true;
    }

    private static bool IsComparisonPatternStart(SyntaxToken token)
    {
        return token.Kind is
            SyntaxKind.EqualsEqualsToken or
            SyntaxKind.NotEqualsToken or
            SyntaxKind.GreaterThanToken or
            SyntaxKind.GreaterThanOrEqualsToken or
            SyntaxKind.LessThanToken or
            SyntaxKind.LessThanOrEqualsToken;
    }

    private PatternSyntax ParseComparisonPattern()
    {
        // ==, !=, >, >=, <, <=
        var operatorToken = ReadToken();

        // Parse RHS as an EXPRESSION (not a pattern)
        // This allows: > 30, > x + 1, > Foo(3), etc.
        var expression = new ExpressionSyntaxParser(this).ParseExpression();

        var kind = GetComparisonPatternKind(operatorToken.Kind);
        return ComparisonPattern(kind, operatorToken, expression);
    }

    private VariableDesignationSyntax? ParseOptionalTrailingDesignation()
    {
        if (!_allowWholePatternDesignation)
            return null;

        var next = PeekToken();
        if (HasLeadingEndOfLineTrivia(next))
            return null;

        var canStartDesignation =
            !IsPatternTerminatorThatCannotStartDesignation(next.Kind) &&
            (CanTokenBeIdentifier(next) ||
             next.IsKind(SyntaxKind.OpenParenToken) ||
             next.Kind is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword);

        return canStartDesignation ? ParseDesignation() : null;
    }

    private static bool IsPatternTerminatorThatCannotStartDesignation(SyntaxKind kind)
    {
        return kind is SyntaxKind.InKeyword
            or SyntaxKind.EqualsToken
            or SyntaxKind.FatArrowToken
            or SyntaxKind.WhenKeyword;
    }

    private static SyntaxKind GetComparisonPatternKind(SyntaxKind operatorTokenKind)
    {
        return operatorTokenKind switch
        {
            SyntaxKind.EqualsEqualsToken => SyntaxKind.EqualsPattern,
            SyntaxKind.NotEqualsToken => SyntaxKind.NotEqualsPattern,
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
        var lessThanToken = ParseExclusiveRangeToken();

        // Parse upper bound if present (any primary expression that isn't another pattern start)
        ExpressionSyntax? upperBound = null;
        if (CanStartRangeBoundExpression(PeekToken()))
        {
            upperBound = new ExpressionSyntaxParser(this, allowLambdaExpressions: false).ParseExpression();
        }

        return RangePattern(lowerBound, dotDotToken, lessThanToken, upperBound);
    }

    private SyntaxToken ParseExclusiveRangeToken()
    {
        if (ConsumeToken(SyntaxKind.LessThanToken, out var lessThanToken))
            return lessThanToken;

        return Token(SyntaxKind.None);
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
        return token.Kind switch
        {
            SyntaxKind.IdentifierToken => true,
            SyntaxKind.TrueKeyword => true,
            SyntaxKind.FalseKeyword => true,
            SyntaxKind.NumericLiteralToken => true,
            SyntaxKind.StringLiteralToken => true,
            SyntaxKind.CharacterLiteralToken => true,
            SyntaxKind.MinusToken => true,
            _ => false
        };
    }

    private bool TryParseConstantPattern(out PatternSyntax constantPattern)
    {
        constantPattern = null!;

        if (LooksLikeTypePatternStart())
            return false;

        if (TryParseLiteralConstantPattern(out constantPattern))
            return true;

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

            constantPattern = RangePattern(rangeExpr.LeftExpression, rangeExpr.DotDotToken, rangeExpr.LessThanToken, rangeExpr.RightExpression);
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

    private bool TryParseLiteralConstantPattern(out PatternSyntax constantPattern)
    {
        constantPattern = null!;

        var current = PeekToken();
        if (!IsLiteralConstantStart(current))
            return false;

        var checkpoint = CreateCheckpoint("literal-constant-pattern");
        var expression = ParseLiteralConstantExpression();
        if (expression is null)
        {
            checkpoint.Rewind();
            return false;
        }

        if (!IsPatternTerminator(PeekToken()))
        {
            checkpoint.Rewind();
            return false;
        }

        constantPattern = ConstantPattern(expression);
        return true;
    }

    private bool LooksLikeTypePatternStart()
    {
        if (!CanTokenBeIdentifier(PeekToken()))
            return false;

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
            LiteralExpressionSyntax => true,
            PrefixOperatorExpressionSyntax unary
                when unary.OperatorToken.Kind == SyntaxKind.MinusToken &&
                     unary.Expression is LiteralExpressionSyntax => true,
            _ => false
        };
    }

    private static bool IsLiteralConstantStart(SyntaxToken token)
    {
        return token.Kind is
            SyntaxKind.NullKeyword or
            SyntaxKind.TrueKeyword or
            SyntaxKind.FalseKeyword or
            SyntaxKind.NumericLiteralToken or
            SyntaxKind.StringLiteralToken or
            SyntaxKind.CharacterLiteralToken or
            SyntaxKind.MinusToken;
    }

    private ExpressionSyntax? ParseLiteralConstantExpression()
    {
        if (PeekToken().Kind == SyntaxKind.MinusToken)
        {
            var minusToken = ReadToken();
            if (!PeekToken().IsKind(SyntaxKind.NumericLiteralToken))
                return null;

            var numericToken = ReadToken();
            var numeric = LiteralExpression(SyntaxKind.NumericLiteralExpression, numericToken);
            return PrefixOperatorExpression(SyntaxKind.UnaryMinusExpression, minusToken, numeric);
        }

        if (!IsLiteralConstantStart(PeekToken()))
            return null;

        var token = ReadToken();
        var kind = token.Kind switch
        {
            SyntaxKind.NullKeyword => SyntaxKind.NullLiteralExpression,
            SyntaxKind.TrueKeyword => SyntaxKind.TrueLiteralExpression,
            SyntaxKind.FalseKeyword => SyntaxKind.FalseLiteralExpression,
            SyntaxKind.NumericLiteralToken => SyntaxKind.NumericLiteralExpression,
            SyntaxKind.StringLiteralToken => SyntaxKind.StringLiteralExpression,
            SyntaxKind.CharacterLiteralToken => SyntaxKind.CharacterLiteralExpression,
            _ => SyntaxKind.None
        };

        return kind == SyntaxKind.None ? null : LiteralExpression(kind, token);
    }

    private bool IsPatternTerminator(SyntaxToken token)
    {
        // Tokens that end the current pattern or separate patterns.
        return token.Kind is
            SyntaxKind.CommaToken or
            SyntaxKind.CloseParenToken or
            SyntaxKind.OpenBraceToken or
            SyntaxKind.CloseBraceToken or
            SyntaxKind.FatArrowToken or
            SyntaxKind.WhenKeyword or
            SyntaxKind.AndToken or
            SyntaxKind.OrToken or
            SyntaxKind.BarToken;
    }
}
