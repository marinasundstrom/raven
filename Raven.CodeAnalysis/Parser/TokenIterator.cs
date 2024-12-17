using System.Collections;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Parser;

public class TokenIterator(Tokenizer tokenizer) : IEnumerable<SyntaxToken>
{
    public IEnumerator<SyntaxToken> GetEnumerator()
    {
        SyntaxToken token;
        do
        {
            token = tokenizer.ReadToken();
            yield return token;
        } while (token.Kind != SyntaxKind.EndOfFileToken);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}