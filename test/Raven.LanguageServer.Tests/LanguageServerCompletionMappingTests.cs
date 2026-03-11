using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public class LanguageServerCompletionMappingTests
{
    [Fact]
    public void ToLspCompletion_MethodCompletion_UsesSnippetAndCaretPlaceholder()
    {
        var text = SourceText.From("counter.Inc");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "Increment",
            InsertionText: "Increment()",
            ReplacementSpan: new TextSpan(8, 3),
            CursorOffset: "Increment()".Length - 1);

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.InsertTextFormat.ShouldBe(InsertTextFormat.Snippet);
        mapped.InsertText.ShouldBe("Increment($0)");
        mapped.CommitCharacters.ShouldNotBeNull();
        mapped.CommitCharacters!.ShouldContain(".");
        mapped.CommitCharacters.ShouldContain("(");
    }

    [Fact]
    public void ToLspCompletion_PlainCompletion_UsesPlainText()
    {
        var text = SourceText.From("counter.Len");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "Length",
            InsertionText: "Length",
            ReplacementSpan: new TextSpan(8, 3),
            CursorOffset: null);

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.InsertTextFormat.ShouldBe(InsertTextFormat.PlainText);
        mapped.InsertText.ShouldBe("Length");
    }
}
