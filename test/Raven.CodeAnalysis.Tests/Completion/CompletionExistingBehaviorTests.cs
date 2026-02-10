using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionExistingBehaviorTests
{
    [Fact]
    public void GetCompletions_OnMethodMemberAccess_UsesInvocationInsertionAndCursorOffset()
    {
        var code = """
class Counter {
    public Increment() -> unit { }
}

val counter = new Counter();
counter.Inc
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var increment = Assert.Single(items.Where(i => i.DisplayText == "Increment"));

        Assert.Equal("Increment()", increment.InsertionText);
        Assert.Equal("Increment()".Length - 1, increment.CursorOffset);
    }

    [Fact]
    public void GetCompletions_OnMemberAccess_UsesMemberNameSpanAsReplacement()
    {
        var code = """
class Counter {
    public Increment() -> unit { }
}

val counter = new Counter();
counter.Inc
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var increment = Assert.Single(items.Where(i => i.DisplayText == "Increment"));

        var expectedSpanStart = code.LastIndexOf("Inc", StringComparison.Ordinal);
        Assert.Equal(new TextSpan(expectedSpanStart, 3), increment.ReplacementSpan);
    }

    [Fact]
    public void GetCompletions_OnIdentifierPrefix_UsesTokenSpanAsReplacement()
    {
        var code = """
ret
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var @return = Assert.Single(items.Where(i => i.DisplayText == "return"));

        Assert.Equal("return", @return.InsertionText);
        Assert.Equal(new TextSpan(0, 3), @return.ReplacementSpan);
    }
}
