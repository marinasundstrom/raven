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
    public func Increment() -> unit { }
}

val counter = Counter();
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
    public func Increment() -> unit { }
}

val counter = Counter();
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
    public func WriteLine(text: string) -> unit { }
}

val logger = Logger();
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
    public func Create() -> string {
        return ""
    }
}

val factory = Factory();
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
    public func Create() -> string {
        return ""
    }
}

val factory = Factory();
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
    public void GetCompletions_AfterDot_DoesNotIncludeExtensionsFromNestedNamespaces()
    {
        var code = """
import System.*
import System.Collections.Generic.*

val numbers = List<int>();
numbers.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default
            .Concat([
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location),
            ])
            .ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        var service = new CompletionService();
        var position = code.LastIndexOf("numbers.", StringComparison.Ordinal) + "numbers.".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Add");
        Assert.DoesNotContain(items, i => i.DisplayText == "Where");
    }

    [Fact]
    public void GetCompletions_OnMethodMemberAccess_ApplyingCompletionProducesExpectedText()
    {
        var code = """
class Counter {
    public func Increment() -> unit { }
}

val counter = Counter();
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
    public func Create() -> string {
        return ""
    }
}

val factory = Factory();
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
    val @if: int = 0
}

val counter = Counter();
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

val widget = Widget(21);
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

val widget = Widget(21);
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

    [Fact]
    public void GetCompletions_OnMissingMemberNameInInvocationArgument_InsertsAfterDot()
    {
        var code = """
func WriteLine(value: int) -> unit { }

val x = 2
WriteLine(x.)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var toString = Assert.Single(items.Where(i => i.DisplayText == "ToString"));

        var dotIndex = code.LastIndexOf('.');
        Assert.Equal(new TextSpan(dotIndex + 1, 0), toString.ReplacementSpan);

        var updated = ApplyCompletion(code, toString);
        Assert.EndsWith("WriteLine(x.ToString())", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_AfterConditionalAccessWithoutPrefix_UsesEmptyReplacementSpanAfterDot()
    {
        var code = """
val text = ""
text?.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("?.", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var length = Assert.Single(items.Where(i => i.DisplayText == "Length"));

        Assert.Equal(new TextSpan(position, 0), length.ReplacementSpan);

        var updated = ApplyCompletion(code, length);
        Assert.EndsWith("text?.Length", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_OnConditionalAccessPrefix_UsesMemberNameSpanAsReplacement()
    {
        var code = """
val text = ""
text?.Len
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var length = Assert.Single(items.Where(i => i.DisplayText == "Length"));

        var expectedSpanStart = code.LastIndexOf("Len", StringComparison.Ordinal);
        Assert.Equal(new TextSpan(expectedSpanStart, 3), length.ReplacementSpan);

        var updated = ApplyCompletion(code, length);
        Assert.EndsWith("text?.Length", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_AfterConditionalAccess_OnResultCarrier_UsesOkPayloadMembers()
    {
        var code = """
union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

val result: Result<string, string> = .Ok("hello")
result?.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("?.", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
        Assert.DoesNotContain(items, i => i.DisplayText == "TryGetValue");
    }

    [Fact]
    public void GetCompletions_AfterConditionalAccess_OnOptionCarrier_UsesSomePayloadMembers()
    {
        var code = """
union Option<T> {
    Some(value: T)
    None
}

val option: Option<string> = .Some("hello")
option?.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("?.", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
        Assert.DoesNotContain(items, i => i.DisplayText == "TryGetValue");
    }

    [Fact]
    public void GetCompletions_AfterInvocationStart_InsertsAtCaret()
    {
        var code = """
func consume(value: int) -> unit { }

val x = 1
consume(
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('(') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var x = Assert.Single(items.Where(i => i.DisplayText == "x"));

        Assert.Equal(new TextSpan(position, 0), x.ReplacementSpan);

        var updated = ApplyCompletion(code, x);
        Assert.EndsWith("consume(x", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_AfterInvocationStart_OnLambdaVariable_InsertsAtCaret()
    {
        var code = """
val foo = (x: int) => x
val text = 1
foo(
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('(') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var foo = Assert.Single(items.Where(i => i.DisplayText == "foo"));
        var text = Assert.Single(items.Where(i => i.DisplayText == "text"));

        Assert.Equal(new TextSpan(position, 0), text.ReplacementSpan);
        Assert.False(string.IsNullOrWhiteSpace(foo.Description));
        Assert.Contains("int", foo.Description!, StringComparison.OrdinalIgnoreCase);

        var updated = ApplyCompletion(code, text);
        Assert.EndsWith("foo(text", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_AfterElementAccessStart_InsertsAtCaret()
    {
        var code = """
val obj = "abc"
val x = 1
obj[
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('[') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var x = Assert.Single(items.Where(i => i.DisplayText == "x"));

        Assert.Equal(new TextSpan(position, 0), x.ReplacementSpan);

        var updated = ApplyCompletion(code, x);
        Assert.EndsWith("obj[x", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_AfterConditionalInvocationStart_InsertsAtCaret()
    {
        var code = """
val x = 1
val text = ""
text?(
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("?(", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var x = Assert.Single(items.Where(i => i.DisplayText == "x"));

        Assert.Equal(new TextSpan(position, 0), x.ReplacementSpan);

        var updated = ApplyCompletion(code, x);
        Assert.EndsWith("text?(x", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_AfterConditionalElementAccessStart_InsertsAtCaret()
    {
        var code = """
val x = 1
val text = ""
text?[
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("?[", StringComparison.Ordinal) + 2;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var x = Assert.Single(items.Where(i => i.DisplayText == "x"));

        Assert.Equal(new TextSpan(position, 0), x.ReplacementSpan);

        var updated = ApplyCompletion(code, x);
        Assert.EndsWith("text?[x", updated, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_OnIndentedBlankLine_InsertsAtCaret()
    {
        var code = """
func Main() -> unit {
    
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.IndexOf("    ", StringComparison.Ordinal) + 4;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();
        var @return = Assert.Single(items.Where(i => i.DisplayText == "return"));

        Assert.Equal(new TextSpan(position, 0), @return.ReplacementSpan);

        var updated = ApplyCompletion(code, @return);
        Assert.Contains("\n    return\n", updated, StringComparison.Ordinal);
    }
}
