using System.Collections.Generic;
using System.Linq;
using System.Text;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.LanguageServer;

using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

internal static class MacroExpansionDisplayService
{
    internal readonly record struct MacroExpansionDisplay(
        string MacroName,
        TextSpan Span,
        string PreviewText,
        string FullText);

    public static bool TryCreateForOffset(
        SourceText sourceText,
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        out MacroExpansionDisplay display)
    {
        display = default;

        var attribute = FindMacroAttribute(root, offset);
        if (attribute is null)
            return false;

        return TryCreateDisplay(semanticModel, attribute, out display);
    }

    public static bool TryCreateForRange(
        SourceText sourceText,
        SemanticModel semanticModel,
        SyntaxNode root,
        LspRange range,
        out MacroExpansionDisplay display)
    {
        display = default;

        var start = PositionHelper.ToOffset(sourceText, range.Start);
        var end = PositionHelper.ToOffset(sourceText, range.End);
        if (end < start)
            (start, end) = (end, start);

        var attribute = FindMacroAttribute(root, start)
            ?? FindMacroAttribute(root, start + ((end - start) / 2))
            ?? FindMacroAttribute(root, end)
            ?? FindMacroAttribute(root, end > start ? end - 1 : end)
            ?? root.DescendantNodes()
                .OfType<AttributeSyntax>()
                .FirstOrDefault(attributeSyntax =>
                    attributeSyntax.IsMacroAttribute() &&
                    attributeSyntax.Span.Start <= end &&
                    start <= attributeSyntax.Span.End);

        if (attribute is null)
            return false;

        return TryCreateDisplay(semanticModel, attribute, out display);
    }

    private static bool TryCreateDisplay(
        SemanticModel semanticModel,
        AttributeSyntax attribute,
        out MacroExpansionDisplay display)
    {
        display = default;

        var expansion = semanticModel.GetMacroExpansion(attribute);
        if (expansion is null)
            return false;

        var sections = new List<string>();
        sections.AddRange(expansion.IntroducedMembers.Select(static member => member.ToString()));

        if (expansion.ReplacementDeclaration is { } replacement)
            sections.Add(replacement.ToString());

        sections.AddRange(expansion.PeerDeclarations.Select(static declaration => declaration.ToString()));

        if (sections.Count == 0)
            return false;

        attribute.TryGetMacroName(out var macroName);
        var fullText = string.Join("\n\n", sections.Where(static section => !string.IsNullOrWhiteSpace(section)));
        var previewText = CreatePreview(fullText);

        display = new MacroExpansionDisplay(
            macroName ?? attribute.Name.ToString(),
            attribute.Span,
            previewText,
            fullText);
        return true;
    }

    private static AttributeSyntax? FindMacroAttribute(SyntaxNode root, int offset)
    {
        foreach (var candidateOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(candidateOffset);
            }
            catch
            {
                continue;
            }

            var attribute = token.Parent?.AncestorsAndSelf().OfType<AttributeSyntax>().FirstOrDefault(static attr => attr.IsMacroAttribute());
            if (attribute is not null)
                return attribute;
        }

        return null;
    }

    private static IEnumerable<int> NormalizeOffsets(int offset, int maxExclusive)
    {
        if (maxExclusive <= 0)
        {
            yield return 0;
            yield break;
        }

        var clamped = offset;
        if (clamped < 0)
            clamped = 0;
        if (clamped >= maxExclusive)
            clamped = maxExclusive - 1;

        yield return clamped;

        if (clamped > 0)
            yield return clamped - 1;
    }

    private static string CreatePreview(string fullText)
    {
        var lines = fullText
            .Replace("\r\n", "\n")
            .Split('\n');

        if (lines.Length <= 12)
            return fullText;

        var builder = new StringBuilder();
        for (var i = 0; i < 12; i++)
        {
            if (i > 0)
                builder.Append('\n');

            builder.Append(lines[i]);
        }

        builder.Append("\n...");
        return builder.ToString();
    }
}
