namespace Raven.CodeAnalysis.Syntax;

internal static class GreenSpanHelper
{
    public static void ComputeSpanAndFullSpan(
        this InternalSyntax.SyntaxList greenList,
        int position,
        out TextSpan span,
        out TextSpan fullSpan)
    {
        if (greenList.SlotCount == 0)
        {
            span = fullSpan = new TextSpan(position, 0);
            return;
        }

        var firstGreen = greenList[0];
        var lastGreen = greenList[greenList.SlotCount - 1];

        int fullWidth = greenList.FullWidth;
        int spanStart = position + firstGreen.GetLeadingTriviaWidth();
        int spanEnd = position + fullWidth - lastGreen.GetTrailingTriviaWidth();
        int spanWidth = spanEnd - spanStart;

        span = new TextSpan(spanStart, spanWidth);
        fullSpan = new TextSpan(position, fullWidth);
    }
}
