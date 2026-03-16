using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServicePatternTests
{
    [Fact]
    public void GetCompletions_AfterDot_OnForPatternLocal_ReturnsInstanceMembers()
    {
        const string code = """
class C {
    func Run(points: (int, int)[]) -> int {
        for (val x, 0) in points {
            x.
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("x.", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "CompareTo");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnForIterationLocal_ReturnsInstanceMembers()
    {
        const string code = """
class C {
    func Run(points: int[]) -> int {
        for x in points {
            x.
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("x.", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "CompareTo");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnTupleLocal_ReturnsTupleMembers()
    {
        const string code = """
class C {
    func Run() -> int {
        val point = (2, 3)
        point.

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("point.", StringComparison.Ordinal) + "point.".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "Item1");
        items.ShouldContain(item => item.DisplayText == "Item2");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnTupleForIterationLocal_ReturnsTupleMembers()
    {
        const string code = """
class C {
    func Run(points: (int, int)[]) -> int {
        for x in points {
            x.
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("x.", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "Item1");
        items.ShouldContain(item => item.DisplayText == "Item2");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnIsPatternDeclarationLocal_ReturnsInstanceMembers()
    {
        const string code = """
class C {
    func Run(value: object) -> int {
        if value is string text {
            text.
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("text.", StringComparison.Ordinal) + 5;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnIfBindingPatternLocal_ReturnsInstanceMembers()
    {
        const string code = """
class Person(val Id: int, val Name: string)

class C {
    func Run(person: Person) -> int {
        if val Person(id, name) = person {
            name.
        }

        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("name.", StringComparison.Ordinal) + 5;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnSequenceRestPatternLocal_ReturnsInstanceMembers()
    {
        const string code = """
class C {
    func Run(values: int[]) -> int {
        return values match {
            [val head, ..val rest] => rest.
            _ => 0
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("rest.", StringComparison.Ordinal) + 5;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnFixedSizeSequenceRestPatternLocal_ReturnsArrayMembers()
    {
        const string code = """
class C {
    func Run(values: int[4]) -> int {
        return values match {
            [val first, val second, ...val rest] => rest.
            _ => 0
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();
        var position = code.IndexOf("rest.", StringComparison.Ordinal) + 5;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        items.ShouldContain(item => item.DisplayText == "Length");
    }
}
