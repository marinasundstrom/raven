using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServicePatternTests
{
    [Fact]
    public void GetCompletions_InEmptyPropertyPattern_ReturnsReceiverMembers()
    {
        const string code = """
record class ItemInfo(Size: int)
record class Foo(Item: ItemInfo, Count: int)

func Test(value: Foo) -> bool {
    return value is Foo {  }
}
""";

        var items = GetCompletions(code, code.IndexOf("{  }", StringComparison.Ordinal) + 2);

        items.ShouldContain(item => item.DisplayText == "Item");
        items.ShouldContain(item => item.DisplayText == "Count");
        items.ShouldNotContain(item => item.DisplayText == "Test");
    }

    [Fact]
    public void GetCompletions_InDottedPropertyPattern_ReturnsIntermediateMembers()
    {
        const string code = """
record class ItemInfo(Size: int, Name: string)
record class Foo(Item: ItemInfo, Count: int)

func Test(value: Foo) -> bool {
    return value is Foo { Item.Si: _ }
}
""";

        var position = code.IndexOf("Si:", StringComparison.Ordinal) + 2;
        var items = GetCompletions(code, position);

        items.ShouldContain(item => item.DisplayText == "Size");
        items.ShouldNotContain(item => item.DisplayText == "Count");
        items.Single(item => item.DisplayText == "Size").ReplacementSpan.Length.ShouldBe(2);
    }

    [Fact]
    public void GetCompletions_AfterDotInPropertyPattern_RecoversAndReturnsIntermediateMembers()
    {
        const string code = """
record class ItemInfo(Size: int, Name: string)
record class Foo(Item: ItemInfo)

func Test(value: Foo) -> bool {
    return value is Foo { Item. }
}
""";

        var position = code.IndexOf("Item.", StringComparison.Ordinal) + "Item.".Length;
        var items = GetCompletions(code, position);

        items.ShouldContain(item => item.DisplayText == "Size");
        items.ShouldContain(item => item.DisplayText == "Name");
    }

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
    public void GetCompletionsAsync_WithExistingSemanticModel_ReusesResolvedModel()
    {
        const string code = """
class C {
    func Run(values: int[]) -> int {
        for x in values {
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
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var items = service.GetCompletionsAsync(semanticModel, position).GetAwaiter().GetResult();

        items.ShouldContain(item => item.DisplayText == "CompareTo");
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
        return match values {
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
        return match values {
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

    private static List<CompletionItem> GetCompletions(string code, int position)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);
        var service = new CompletionService();

        return service.GetCompletions(compilation, syntaxTree, position).ToList();
    }
}
