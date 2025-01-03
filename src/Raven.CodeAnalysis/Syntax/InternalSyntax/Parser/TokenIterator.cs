using System.Collections;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class TokenIterator(Tokenizer tokenizer) : IEnumerable<SyntaxToken>
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