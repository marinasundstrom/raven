using System.Text;

internal sealed class RavenDocContentTemplate
{
    public string RenderTypePage(RavenDocTypeContentTemplateModel page)
        => RenderBlocks(
            page.HeroHtml,
            RenderLines(page.MetadataLines),
            RenderLines(page.RelationshipLines),
            page.DocumentationMarkdown,
            RenderLines(page.MemberSections));

    public string RenderNamespacePage(RavenDocNamespaceContentTemplateModel page)
        => RenderBlocks(
            page.HeroHtml,
            page.DocumentationMarkdown,
            RenderLines(page.MemberSections));

    public string RenderMemberPage(RavenDocMemberContentTemplateModel page)
        => RenderBlocks(
            page.HeroHtml,
            RenderLines(page.MetadataLines),
            page.DocumentationMarkdown);

    public string RenderMemberGroupPage(RavenDocMemberGroupContentTemplateModel page)
    {
        var variants = new StringBuilder();
        variants.AppendLine("## Overloads / Variants");
        foreach (var variant in page.Variants)
        {
            variants.AppendLine();
            variants.AppendLine($"### {variant.Name}");
            variants.AppendLine();
            variants.AppendLine(variant.SignatureHtml);
            if (!string.IsNullOrWhiteSpace(variant.SourceMarkdown))
            {
                variants.AppendLine();
                variants.AppendLine(variant.SourceMarkdown);
            }

            variants.AppendLine();
            variants.AppendLine(variant.DocumentationMarkdown);
        }

        return RenderBlocks(
            page.HeroHtml,
            RenderLines(page.MetadataLines),
            variants.ToString());
    }

    private static string RenderBlocks(params string?[] blocks)
    {
        var builder = new StringBuilder();
        foreach (var block in blocks)
        {
            if (string.IsNullOrWhiteSpace(block))
                continue;

            if (builder.Length > 0)
                builder.AppendLine().AppendLine();
            builder.Append(block.TrimEnd());
        }

        return builder.ToString();
    }

    private static string RenderLines(IReadOnlyList<string> lines)
        => string.Join(Environment.NewLine, lines.Where(
            static line => !string.IsNullOrWhiteSpace(line)));
}

internal sealed record RavenDocTypeContentTemplateModel(
    string HeroHtml,
    IReadOnlyList<string> MetadataLines,
    IReadOnlyList<string> RelationshipLines,
    string? DocumentationMarkdown,
    IReadOnlyList<string> MemberSections);

internal sealed record RavenDocNamespaceContentTemplateModel(
    string HeroHtml,
    string? DocumentationMarkdown,
    IReadOnlyList<string> MemberSections);

internal sealed record RavenDocMemberContentTemplateModel(
    string HeroHtml,
    IReadOnlyList<string> MetadataLines,
    string DocumentationMarkdown);

internal sealed record RavenDocMemberGroupContentTemplateModel(
    string HeroHtml,
    IReadOnlyList<string> MetadataLines,
    IReadOnlyList<RavenDocMemberVariantTemplateModel> Variants);

internal sealed record RavenDocMemberVariantTemplateModel(
    string Name,
    string SignatureHtml,
    string? SourceMarkdown,
    string DocumentationMarkdown);
