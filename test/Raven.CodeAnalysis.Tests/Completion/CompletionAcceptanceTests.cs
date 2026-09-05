using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionAcceptanceTests
{
    public static IEnumerable<object[]> EditingScenarios()
    {
        var scenarios = new[]
        {
            ("    Con|", "Console", "    Console|"),
            ("\tCon|sole", "Console", "\tConsole|"),
            ("    Con|soel", "Console", "    Console|"),
            ("    System.Con|soel", "Console", "    System.Console|"),
            ("    /* keep */ Con| // keep", "Console", "    /* keep */ Console| // keep"),
            ("    @cl|", "@class", "    @class|"),
            ("    @cl|sas", "@class", "    @class|"),
            ("    Console.Wri|", "WriteLine", "    Console.WriteLine(|)"),
            ("    Console.Write|Lnie", "WriteLine", "    Console.WriteLine(|)"),
            ("    Console.Wri|()", "WriteLine", "    Console.WriteLine|()"),
            ("    Console.Wri|(", "WriteLine", "    Console.WriteLine|("),
            ("    Console.Write|Lnie(\"hello\")", "WriteLine", "    Console.WriteLine|(\"hello\")"),
            ("    Console.Wri|(\"hello\")", "WriteLine", "    Console.WriteLine|(\"hello\")"),
            ("    Console.Wri| /* keep */ (\"hello\")", "WriteLine", "    Console.WriteLine| /* keep */ (\"hello\")"),
            ("    Console.|", "WriteLine", "    Console.WriteLine(|)")
        };

        foreach (var newline in new[] { "\n", "\r\n" })
        {
            foreach (var (before, label, after) in scenarios)
                yield return new object[] { before, label, after, newline };
        }
    }

    [Theory]
    [MemberData(nameof(EditingScenarios))]
    public void AcceptCompletion_PreservesSourceAndPositionsCaret(
        string before, string label, string after, string newline)
    {
        var header = string.Join(newline, "import System.*", "", "func Main() {", "    let @class = 1", "");
        var markedSource = header + before + newline + "}";
        AssertAcceptedCompletion(markedSource, label, header + after + newline + "}");
    }

    [Theory]
    [InlineData("func Main() {\n    |", "return", "func Main() {\n    return|")]
    [InlineData("func Main() {\n    Con|", "Console", "func Main() {\n    Console|")]
    [InlineData("func Main() {\n    Console.|", "WriteLine", "func Main() {\n    Console.WriteLine(|)")]
    [InlineData("func Main() {\n    let count = 1\n    |\n}", "count", "func Main() {\n    let count = 1\n    count|\n}")]
    [InlineData("func Main() {\n    let text = \"hello\"\n    text.Length\n    |\n}", "return", "func Main() {\n    let text = \"hello\"\n    text.Length\n    return|\n}")]
    [InlineData("func Main() {\n    Console.WriteLine()\n    |\n}", "return", "func Main() {\n    Console.WriteLine()\n    return|\n}")]
    [InlineData("func Main() {\n    Console.\n        Wri|\n}", "WriteLine", "func Main() {\n    Console.\n        WriteLine(|)\n}")]
    [InlineData("func Main() {\n    Console.\n        |\n}", "WriteLine", "func Main() {\n    Console.\n        WriteLine(|)\n}")]
    [InlineData("func Main() {\n    let text: string? = \"hello\"\n    text?.Length\n    |\n}", "return", "func Main() {\n    let text: string? = \"hello\"\n    text?.Length\n    return|\n}")]
    [InlineData("class C {\n    func Run() {\n        let value = 1\n        |\n    }\n}", "self", "class C {\n    func Run() {\n        let value = 1\n        self|\n    }\n}")]
    public void AcceptCompletion_InIncompleteCode_PreservesSourceAndPositionsCaret(
        string before, string label, string after)
    {
        AssertAcceptedCompletion("import System.*\n" + before, label, "import System.*\n" + after);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AcceptCompletion_AcrossSuccessiveDocumentEdits_RemainsPredictable(bool closeBody)
    {
        const string header = "import System.*\n\nfunc Main() {\n    let count = 1\n    ";
        var footer = closeBody ? "\n}" : string.Empty;
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject("completion-edits",
            compilationOptions: new CompilationOptions(OutputKind.ConsoleApplication),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        var document = project.AddDocument("main.rvn", SourceText.From(header + footer));
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));

        var steps = new[]
        {
            ("Con|", "Console", "Console|"),
            ("Console.Wri|", "WriteLine", "Console.WriteLine(|)"),
            ("Console.WriteLine(count)\n    |", "count", "Console.WriteLine(count)\n    count|"),
            ("Console.WriteLine(count)\n    ret|", "return", "Console.WriteLine(count)\n    return|"),
            // Return to the previous text, as when undoing the last edit.
            ("Console.WriteLine(count)\n    |", "count", "Console.WriteLine(count)\n    count|")
        };

        foreach (var (before, label, after) in steps)
        {
            var markedSource = header + before + footer;
            var position = markedSource.IndexOf('|');
            var source = markedSource.Remove(position, 1);
            Assert.True(workspace.TryApplyChanges(
                workspace.CurrentSolution.WithDocumentText(document.Id, SourceText.From(source))));
            var compilation = workspace.GetCompilation(projectId);
            var tree = compilation.SyntaxTrees.Single();
            var result = new CompletionService().GetCompletionsWithMetrics(compilation, tree, position);
            Assert.False(result.UsedFallback, result.FailureType);
            var item = Assert.Single(result.Items, item => item.DisplayText == label);
            var completed = ApplyCompletion(source, item);
            Assert.Equal(header + after + footer, completed);
            Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(
                document.Id, SourceText.From(completed.Remove(completed.IndexOf('|'), 1)))));
        }
    }

    private static void AssertAcceptedCompletion(string markedSource, string label, string expected)
    {
        var position = markedSource.IndexOf('|');
        var source = markedSource.Remove(position, 1);
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var item = Assert.Single(compilation.GetCompletions(tree, position), item => item.DisplayText == label);
        Assert.Equal(expected, ApplyCompletion(source, item));
    }

    private static string ApplyCompletion(string source, CompletionItem item)
    {
        var completed = source.Remove(item.ReplacementSpan.Start, item.ReplacementSpan.Length)
            .Insert(item.ReplacementSpan.Start, item.InsertionText);
        var caret = item.ReplacementSpan.Start + (item.CursorOffset ?? item.InsertionText.Length);

        return completed.Insert(caret, "|");
    }
}
