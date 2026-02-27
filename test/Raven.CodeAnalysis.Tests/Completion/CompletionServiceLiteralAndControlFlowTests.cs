using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceLiteralAndControlFlowTests
{
    [Fact]
    public void GetCompletions_InGotoStatement_IncludesLabels()
    {
        var code = """
func Main() {
label:
    goto
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.LastIndexOf("goto", StringComparison.Ordinal) + "goto".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "label");
    }

    [Fact]
    public void GetCompletions_ForEscapedIdentifier_UsesEscapedInsertion()
    {
        var code = """
val @if = 0;
@i
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        var escaped = Assert.Single(items.Where(i => i.DisplayText == "@if"));
        Assert.Equal("@if", escaped.InsertionText);
    }
}
