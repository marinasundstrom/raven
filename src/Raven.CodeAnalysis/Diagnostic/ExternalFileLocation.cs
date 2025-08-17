using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal class ExternalFileLocation : Location
{
    private readonly string _filePath;
    private readonly TextSpan _textSpan;
    private readonly LinePositionSpan _lineSpan;

    internal ExternalFileLocation(string filePath, TextSpan textSpan, LinePositionSpan lineSpan)
    {
        Kind = LocationKind.ExternalFile;

        _filePath = filePath;
        _textSpan = textSpan;
        _lineSpan = lineSpan;
    }

    public override FileLinePositionSpan GetLineSpan()
    {
        return new FileLinePositionSpan(_filePath, _lineSpan.Start, _lineSpan.End);
    }
}