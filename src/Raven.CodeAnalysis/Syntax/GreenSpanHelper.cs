namespace Raven.CodeAnalysis.Syntax;

internal static class GreenSpanHelper
{
    public static void ComputeSpanAndFullSpan(
        this InternalSyntax.SyntaxList greenList,
        int position,
        out TextSpan span,
        out TextSpan fullSpan)
    {
        var firstIndex = 0;
        while (firstIndex < greenList.SlotCount && greenList[firstIndex] is null)
            firstIndex++;

        if (firstIndex == greenList.SlotCount)
        {
            span = fullSpan = new TextSpan(position, 0);
            return;
        }

        var lastIndex = greenList.SlotCount - 1;
        while (greenList[lastIndex] is null)
            lastIndex--;

        var firstGreen = greenList[firstIndex]!;
        var lastGreen = greenList[lastIndex]!;

        int fullWidth = greenList.FullWidth;
        int spanStart = position + firstGreen.GetLeadingTriviaWidth();
        int spanEnd = position + fullWidth - lastGreen.GetTrailingTriviaWidth();
        int spanWidth = spanEnd - spanStart;

        span = new TextSpan(spanStart, spanWidth);
        fullSpan = new TextSpan(position, fullWidth);
    }
}
