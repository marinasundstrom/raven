using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionExistingBehaviorTests
{
    private static string ApplyCompletion(string text, CompletionItem item)
    {
        var before = text[..item.ReplacementSpan.Start];
        var after = text[item.ReplacementSpan.End..];
        return before + item.InsertionText + after;
    }

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

    [Fact]
    public void GetCompletions_AfterDot_OnInvocationReturningUnit_ReturnsNoMemberCompletions()
    {
        var code = """
class Logger {
    public WriteLine(text: string) -> unit { }
}

val logger = new Logger();
logger.WriteLine("Hello").
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, i => i.DisplayText == "IndexOf");
        Assert.DoesNotContain(items, i => i.DisplayText == "Length");
        Assert.Empty(items);
    }

    [Fact]
    public void GetCompletions_AfterDot_OnInvocationReturningString_ReturnsStringMembers()
    {
        var code = """
class Factory {
    public Create() -> string {
        return ""
    }
}

val factory = new Factory();
factory.Create().
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
        Assert.DoesNotContain(items, i => i.DisplayText == "if");
    }

    [Fact]
    public void GetCompletions_AfterDotWithoutPrefix_UsesEmptyReplacementSpanAfterDot()
    {
        var code = """
class Factory {
    public Create() -> string {
        return ""
    }
}

val factory = new Factory();
factory.Create().
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var length = Assert.Single(items.Where(i => i.DisplayText == "Length"));

        Assert.Equal(new TextSpan(position, 0), length.ReplacementSpan);
    }

    [Fact]
    public void GetCompletions_OnMethodMemberAccess_ApplyingCompletionProducesExpectedText()
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

        var updated = ApplyCompletion(code, increment);
        Assert.EndsWith("counter.Increment()", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_OnPropertyMemberAccess_DoesNotAppendParentheses()
    {
        var code = """
class Factory {
    public Create() -> string {
        return ""
    }
}

val factory = new Factory();
factory.Create().Len
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var length = Assert.Single(items.Where(i => i.DisplayText == "Length"));

        Assert.Equal("Length", length.InsertionText);

        var updated = ApplyCompletion(code, length);
        Assert.EndsWith("factory.Create().Length", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_OnEscapedKeywordMember_InsertsEscapedIdentifier()
    {
        var code = """
class Counter {
    public @if: int;
}

val counter = new Counter();
counter.i
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var escapedIf = Assert.Single(items.Where(i => i.DisplayText == "@if"));

        Assert.Equal("@if", escapedIf.InsertionText);

        var updated = ApplyCompletion(code, escapedIf);
        Assert.EndsWith("counter.@if", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_OnExtensionMethodPrefix_UsesInvocationInsertionAndMemberReplacementSpan()
    {
        var code = """
import Raven.MetadataFixtures.StaticExtensions.*;

val widget = new Widget(21);
widget.Dou
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithExtensionMethods);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var @double = Assert.Single(items.Where(i => i.DisplayText == "Double"));

        Assert.Equal("Double()", @double.InsertionText);
        Assert.Equal("Double()".Length - 1, @double.CursorOffset);

        var prefixStart = code.LastIndexOf("Dou", StringComparison.Ordinal);
        Assert.Equal(new TextSpan(prefixStart, 3), @double.ReplacementSpan);

        var updated = ApplyCompletion(code, @double);
        Assert.EndsWith("widget.Double()", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_OnExtensionMethodAfterDot_UsesEmptyReplacementSpan()
    {
        var code = """
import Raven.MetadataFixtures.StaticExtensions.*;

val widget = new Widget(21);
widget.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithExtensionMethods);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var @double = Assert.Single(items.Where(i => i.DisplayText == "Double"));

        Assert.Equal(new TextSpan(position, 0), @double.ReplacementSpan);

        var updated = ApplyCompletion(code, @double);
        Assert.EndsWith("widget.Double()", updated, StringComparison.Ordinal);
    }
}
