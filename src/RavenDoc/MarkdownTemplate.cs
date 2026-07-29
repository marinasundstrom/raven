using System.Text.RegularExpressions;

internal static partial class MarkdownTemplate
{
    public static string Apply(
        string markdown,
        IReadOnlyDictionary<string, string> values)
    {
        if (string.IsNullOrEmpty(markdown) || values.Count == 0)
            return markdown;

        return PlaceholderRegex().Replace(markdown, match =>
        {
            var name = match.Groups["name"].Value;
            return values.TryGetValue(name, out var value)
                ? value
                : match.Value;
        });
    }

    public static bool IsValidValueName(string name)
        => ValueNameRegex().IsMatch(name);

    [GeneratedRegex(@"\{\{\s*(?<name>[A-Za-z_][A-Za-z0-9_.-]*)\s*\}\}")]
    private static partial Regex PlaceholderRegex();

    [GeneratedRegex(@"^[A-Za-z_][A-Za-z0-9_.-]*$")]
    private static partial Regex ValueNameRegex();
}
