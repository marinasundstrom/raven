using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

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
        var position = markedSource.IndexOf('|');
        var source = markedSource.Remove(position, 1);
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var item = Assert.Single(compilation.GetCompletions(tree, position), item => item.DisplayText == label);
        var completed = source.Remove(item.ReplacementSpan.Start, item.ReplacementSpan.Length)
            .Insert(item.ReplacementSpan.Start, item.InsertionText);
        var caret = item.ReplacementSpan.Start + (item.CursorOffset ?? item.InsertionText.Length);

        Assert.Equal(header + after + newline + "}", completed.Insert(caret, "|"));
    }
}
