using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public interface IMacroTokenStream
{
    bool IsEndOfFile { get; }

    SyntaxToken PeekToken(int offset = 0);

    SyntaxToken ReadToken();
}
