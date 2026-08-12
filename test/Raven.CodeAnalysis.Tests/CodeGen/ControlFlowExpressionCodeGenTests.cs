using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class ControlFlowExpressionCodeGenTests
{
    [Fact]
    public void MatchArms_WithBreakContinueAndLabels_EmitAndRun()
    {
        const string source = """
import System.*

func Main() {
    var outerCount = 0
outer: loop {
        outerCount += 1
        var innerCount = 0
        loop {
            innerCount += 1
            let selected = match (outerCount, innerCount) {
                (1, 1) => continue
                (1, 2) => continue outer
                (2, 1) => break
                _ => break outer
            }
        }
        Console.WriteLine("inner")
    }
    Console.WriteLine(outerCount)
}
""";

        AssertOutput(source, ["inner", "3"]);
    }

    [Fact]
    public void MatchArms_WithYieldAndYieldBreak_EmitAndRun()
    {
        const string source = """
import System.*
import System.Collections.Generic.*

func Items(stop: bool) -> IEnumerable<int> {
    yield 1
    let item = match stop {
        true => yield break
        false => 2
    }
    yield item
    match item {
        2 => yield return 3
        _ => yield break
    }
    yield break
}

func Main() {
    for item in Items(false) {
        Console.WriteLine(item)
    }
    for item in Items(true) {
        Console.WriteLine(item)
    }
}
""";

        AssertOutput(source, ["1", "2", "3", "1"]);
    }

    private static void AssertOutput(string source, string[] expected)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
                "control-flow-expressions", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(SyntaxTree.ParseText(source))
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;
        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            entryPoint.Invoke(null, entryPoint.GetParameters().Length == 0 ? null : [Array.Empty<string>()]);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal(expected, output);
    }
}
