using System.Net;
using System.Text.RegularExpressions;

internal static class DocumentationCodeCopy
{
    public static void Apply(string root, bool enabled)
    {
        foreach (var path in Directory.EnumerateFiles(root, "*.html", SearchOption.AllDirectories))
        {
            var html = File.ReadAllText(path);
            html = Regex.Replace(html, @"<!-- ravendoc-copy -->.*?<!-- /ravendoc-copy -->", "", RegexOptions.Singleline);
            if (enabled && html.Contains("<article", StringComparison.Ordinal))
            {
                var script = Path.GetRelativePath(Path.GetDirectoryName(path)!, Path.Combine(root, "copy-code.js")).Replace('\\', '/');
                html = html.Replace("</body>", $"<!-- ravendoc-copy --><script type=\"module\" src=\"{WebUtility.HtmlEncode(script)}\"></script><!-- /ravendoc-copy --></body>", StringComparison.Ordinal);
            }
            File.WriteAllText(path, html);
        }
    }
}
