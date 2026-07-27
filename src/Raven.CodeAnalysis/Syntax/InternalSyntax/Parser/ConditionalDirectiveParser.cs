namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal static class ConditionalDirectiveParser
{
    public static bool TryEvaluate(
        string text,
        IReadOnlySet<string> definedSymbols,
        out bool value,
        out int errorOffset,
        out string? error)
    {
        var parser = new ExpressionParser(text, definedSymbols);
        value = parser.ParseOrExpression();
        parser.SkipWhitespace();

        if (parser.Error is null && !parser.IsAtEnd)
            parser.Fail("unexpected token");

        errorOffset = parser.Position;
        error = parser.Error;
        return error is null;
    }

    private sealed class ExpressionParser
    {
        private readonly string _text;
        private readonly IReadOnlySet<string> _definedSymbols;

        public ExpressionParser(string text, IReadOnlySet<string> definedSymbols)
        {
            _text = text;
            _definedSymbols = definedSymbols;
        }

        public int Position { get; private set; }

        public string? Error { get; private set; }

        public bool IsAtEnd => Position >= _text.Length;

        public bool ParseOrExpression()
        {
            var value = ParseAndExpression();

            while (Error is null)
            {
                if (!TryConsumeOperator("or") && !TryConsumeOperator("||"))
                    break;

                var right = ParseAndExpression();
                value |= right;
            }

            return value;
        }

        private bool ParseAndExpression()
        {
            var value = ParseUnaryExpression();

            while (Error is null)
            {
                if (!TryConsumeOperator("and") && !TryConsumeOperator("&&"))
                    break;

                var right = ParseUnaryExpression();
                value &= right;
            }

            return value;
        }

        private bool ParseUnaryExpression()
        {
            SkipWhitespace();
            if (TryConsumeOperator("not") || TryConsumeOperator("!"))
                return !ParseUnaryExpression();

            return ParsePrimaryExpression();
        }

        private bool ParsePrimaryExpression()
        {
            SkipWhitespace();

            if (TryConsume("("))
            {
                var value = ParseOrExpression();
                SkipWhitespace();
                if (!TryConsume(")"))
                    Fail("expected ')'");
                return value;
            }

            if (!TryReadIdentifier(out var identifier))
            {
                Fail(IsAtEnd ? "expected an expression" : "expected a conditional symbol");
                return false;
            }

            return identifier switch
            {
                "true" => true,
                "false" => false,
                _ => _definedSymbols.Contains(identifier)
            };
        }

        public void SkipWhitespace()
        {
            while (!IsAtEnd && char.IsWhiteSpace(_text[Position]))
                Position++;
        }

        public void Fail(string message)
        {
            Error ??= message;
        }

        private bool TryReadIdentifier(out string identifier)
        {
            SkipWhitespace();
            var start = Position;
            if (IsAtEnd || !SyntaxFacts.IsIdentifierStartCharacter(_text[Position]))
            {
                identifier = string.Empty;
                return false;
            }

            Position++;
            while (!IsAtEnd && SyntaxFacts.IsIdentifierPartCharacter(_text[Position]))
                Position++;

            identifier = _text[start..Position];
            return true;
        }

        private bool TryConsumeOperator(string value)
        {
            var start = Position;
            SkipWhitespace();
            if (!TryConsume(value))
            {
                Position = start;
                return false;
            }

            if (char.IsLetter(value[0]) &&
                !IsAtEnd &&
                SyntaxFacts.IsIdentifierPartCharacter(_text[Position]))
            {
                Position = start;
                return false;
            }

            return true;
        }

        private bool TryConsume(string value)
        {
            if (Position + value.Length > _text.Length ||
                !_text.AsSpan(Position, value.Length).SequenceEqual(value))
            {
                return false;
            }

            Position += value.Length;
            return true;
        }
    }
}

internal sealed record ConditionalDirectiveInfo(
    ConditionalDirectiveKind Kind,
    SyntaxKind SyntaxKind,
    string ConditionText,
    bool IsBranchActive,
    bool BranchTaken,
    int KeywordOffset,
    int KeywordLength,
    int ConditionOffset,
    int ConditionLength);
