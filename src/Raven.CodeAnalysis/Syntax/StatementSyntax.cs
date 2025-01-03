using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

public abstract class StatementSyntax : SyntaxNode
{
    internal StatementSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public static partial class SyntaxFactory
{
    public static StatementSyntax ParseStatement(string text, int offset = 0, ParseOptions? options = default, bool consumeFullText = true)
    {
        var parser = new InternalSyntax.Parser.LanguageParser(null, options);

        return (StatementSyntax)parser.ParseStatement(SourceText.From(text), offset, consumeFullText)!.CreateRed(null, 0)!;
    }
}