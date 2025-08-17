namespace Raven.CodeAnalysis.Text;

public static class TextUtils
{
    public static List<int> ComputeLineStarts(string text)
    {
        var lineStarts = new List<int> { 0 };
        for (int i = 0; i < text.Length; i++)
        {
            if (text[i] == '\n' || (text[i] == '\r' && (i + 1 == text.Length || text[i + 1] != '\n')))
            {
                lineStarts.Add(i + 1);
            }
        }
        return lineStarts;
    }

    public static (int line, int column) GetLineAndColumn(List<int> lineStarts, int position)
    {
        int line = lineStarts.BinarySearch(position);
        if (line < 0)
            line = ~line - 1;

        int column = position - lineStarts[line];
        return (line + 1, column + 1); // 1-based indexing
    }
}