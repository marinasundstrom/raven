using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionMemberAccessTests
{
    [Fact]
    public void GetCompletions_AfterDot_OnUserType_ReturnsOnlyStaticMembers()
    {
        var code = """
class Counter {
    public static Reset() -> unit { }
    public Increment() -> unit { }
}

Counter.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Reset");
        Assert.DoesNotContain(items, i => i.DisplayText == "Increment");
        Assert.DoesNotContain(items, i => i.DisplayText == "if");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnUserInstance_ReturnsOnlyInstanceMembers()
    {
        var code = """
class Counter {
    public static Reset() -> unit { }
    public Increment() -> unit { }
}

val counter = new Counter();
counter.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Increment");
        Assert.DoesNotContain(items, i => i.DisplayText == "Reset");
        Assert.DoesNotContain(items, i => i.DisplayText == "if");
    }

}
