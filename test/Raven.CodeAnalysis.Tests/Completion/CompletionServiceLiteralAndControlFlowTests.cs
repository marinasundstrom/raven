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
    public void GetCompletions_OnLiteralUnionLocal_SuggestsAllMembers()
    {
        var code = "val response: \"כן\" | \"לא\" = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "\"כן\"");
        Assert.Contains(items, i => i.DisplayText == "\"לא\"");
    }

    [Fact]
    public void GetCompletions_OnLiteralUnionAssignment_SuggestsAllMembers()
    {
        var code = "val response: \"כן\" | \"לא\" = \"כן\";\nresponse = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "\"כן\"");
        Assert.Contains(items, i => i.DisplayText == "\"לא\"");
    }

    [Fact]
    public void GetCompletions_OnNumericLiteralUnionLocal_SuggestsAllMembers()
    {
        var code = "val flags: 0 | 1 | 2 = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "0");
        Assert.Contains(items, i => i.DisplayText == "1");
        Assert.Contains(items, i => i.DisplayText == "2");
    }

    [Fact]
    public void GetCompletions_OnNumericLiteralUnionAssignment_SuggestsAllMembers()
    {
        var code = "val flags: 0 | 1 | 2 = 0;\nflags = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "0");
        Assert.Contains(items, i => i.DisplayText == "1");
        Assert.Contains(items, i => i.DisplayText == "2");
    }

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
