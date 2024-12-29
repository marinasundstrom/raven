using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal class SourceLocation : Location
{
    internal SourceLocation(SyntaxTree sourceTree, TextSpan sourceSpan)
    {
        Kind = LocationKind.SourceFile;

        SourceTree = sourceTree;
        SourceSpan = sourceSpan;
    }

    public override FileLinePositionSpan GetLineSpan()
    {
        var text = SourceTree.GetText();

        var (line, col) = text.GetLineAndColumn(SourceSpan);
        var (line2, col2) = text.GetLineAndColumn(new TextSpan(SourceSpan.End, 0));

        return new FileLinePositionSpan(
            SourceTree.FilePath,
            new LinePosition(line - 1, col - 1),
            new LinePosition(line2 - 1, col2 - 1));
    }
}