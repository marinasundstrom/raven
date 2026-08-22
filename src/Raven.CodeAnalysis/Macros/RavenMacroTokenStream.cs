using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Macros;

internal sealed class RavenMacroTokenStream : IMacroTokenStream
{
    private readonly BaseParseContext _parseContext;
    private readonly Dictionary<string, MacroKeyword> _keywords;
    private readonly List<SyntaxToken> _lookahead = [];
    private readonly CancellationToken _cancellationToken;

    public RavenMacroTokenStream(
        MacroTokenStreamContext context,
        ImmutableArray<MacroKeyword> keywords)
    {
        ArgumentNullException.ThrowIfNull(context);

        var lexer = new Lexer(new StringReader(context.BodyText));
        _parseContext = new BaseParseContext(lexer, context.ParseOptions);
        _keywords = CreateKeywordMap(keywords);
        _cancellationToken = context.CancellationToken;
    }

    public bool IsEndOfFile => PeekToken().Kind == SyntaxKind.EndOfFileToken;

    public SyntaxToken PeekToken(int offset = 0)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(offset);
        _cancellationToken.ThrowIfCancellationRequested();

        while (_lookahead.Count <= offset)
            _lookahead.Add(ReadTokenCore());

        return _lookahead[offset];
    }

    public SyntaxToken ReadToken()
    {
        _cancellationToken.ThrowIfCancellationRequested();

        if (_lookahead.Count == 0)
            return ReadTokenCore();

        var token = _lookahead[0];
        _lookahead.RemoveAt(0);
        return token;
    }

    private SyntaxToken ReadTokenCore()
    {
        var position = _parseContext.Position;
        var greenToken = _parseContext.ReadToken();
        var token = new SyntaxToken(greenToken, parent: null, position);

        if (token.CanBeIdentifier() &&
            _keywords.TryGetValue(token.ValueText, out var keyword))
        {
            token = token.WithRawKind(keyword.RawKind);
        }

        return token;
    }

    private static Dictionary<string, MacroKeyword> CreateKeywordMap(
        ImmutableArray<MacroKeyword> keywords)
    {
        var result = new Dictionary<string, MacroKeyword>(StringComparer.Ordinal);
        foreach (var keyword in keywords)
        {
            ArgumentNullException.ThrowIfNull(keyword);
            if (!result.TryAdd(keyword.Text, keyword))
                throw new ArgumentException($"Duplicate macro keyword '{keyword.Text}'.", nameof(keywords));
        }

        return result;
    }
}
