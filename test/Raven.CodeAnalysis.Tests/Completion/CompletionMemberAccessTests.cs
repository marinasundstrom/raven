using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionMemberAccessTests
{
    [Theory]
    [InlineData("init()")]
    [InlineData("init(value: int)")]
    [InlineData("static init()")]
    [InlineData("func Test()")]
    public void QualifiedStaticMembersInsideExecutableBodies(string declaration)
    {
        var code = "class Example {\n    " + declaration + " {\n        System.Int32.\n    }\n}";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);
        var position = code.IndexOf("System.Int32.", StringComparison.Ordinal) + "System.Int32.".Length;
        var receiver = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.ToString() == "System.Int32");
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(receiver).Symbol);
        Assert.Equal(SpecialType.System_Int32, symbol.SpecialType);
        var items = compilation.GetCompletions(tree, position).ToList();
        Assert.Contains(items, item => item.DisplayText == "Parse");
        Assert.DoesNotContain(items, item => item.DisplayText == "CompareTo");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnUserType_ReturnsOnlyStaticMembers()
    {
        var code = """
class Counter {
    public static func Reset() -> unit { }
    public func Increment() -> unit { }
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
    public static func Reset() -> unit { }
    public func Increment() -> unit { }
}

let counter = Counter();
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

    [Fact]
    public void GetCompletions_AfterDot_OnParameterReceiver_ReturnsInstanceMembers()
    {
        var code = """
class Counter {
    public func Increment() -> unit { }
}

func Touch(counter: Counter) -> unit {
    counter.
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Increment");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnForLoopReceiver_ReturnsInstanceMembers()
    {
        var code = """
class Counter {
    public func Increment() -> unit { }
}

let counters = [Counter()]

for counter in counters {
    counter.
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Increment");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnUnionInstance_DoesNotReturnSynthesizedPayloadFields()
    {
        var code = """
union JsonValue(bool | double | string) {
}

let value = JsonValue(true)
value.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "HasValue");
        Assert.Contains(items, i => i.DisplayText == "Value");
        Assert.DoesNotContain(items, i => i.DisplayText == "<Tag>");
        Assert.DoesNotContain(items, i => i.DisplayText.EndsWith("Payload>", StringComparison.Ordinal));
    }

}
