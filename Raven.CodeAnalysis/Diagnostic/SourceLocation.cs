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

        return new FileLinePositionSpan(
            SourceTree.FilePath,
            new LinePosition(col - 1, line - 1),
            default);
    }
}