namespace Raven.CodeAnalysis.Syntax;

static class GreenNodeExtensions
{
    public static int CalculateWidth(this GreenNode node)
    {
        var items = node.GetChildren().ToArray();

        if (items is null || items.Length == 0)
            return 0;

        if (items.Length == 1)
        {
            return items[0]?.Width ?? 0;
        }

        var items2 = items
            .Where(item => item is not null);

        var value = items2.Sum(item => item.FullWidth);

        var f1 = node.GetFirstToken();
        var f2 = node.GetLastToken();

        return value - (f1?.LeadingTrivia.Width ?? 0) - (f2?.TrailingTrivia.Width ?? 0);
    }

    public static int CalculateFullWidth(this GreenNode node)
    {
        var items = node.GetChildren().ToArray();

        if (items is null || items.Length == 0)
            return 0;

        if (items.Length == 1)
        {
            return items[0]?.FullWidth ?? 0;
        }

        var value = items
            .Where(item => item is not null)
            .Sum(item => item.FullWidth);

        return value;
    }
}