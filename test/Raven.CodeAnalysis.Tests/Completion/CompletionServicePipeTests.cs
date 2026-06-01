using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServicePipeTests
{
    [Fact]
    public void GetCompletions_AfterPipeIdentifier_ReturnsApplicableInScopeFunctions()
    {
        var code = """
public func Wrap(value: int) -> string {
    return value.ToString()
}

public func WantsString(value: string) -> string {
    return value
}

public func NoInput() -> string {
    return ""
}

func Main() {
    val source: int = 1
    val result = source |> W
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("W", StringComparison.Ordinal) + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "Wrap");
        Assert.DoesNotContain(items, static item => item.DisplayText == "WantsString");
        Assert.DoesNotContain(items, static item => item.DisplayText == "NoInput");
    }

    [Fact]
    public void GetCompletions_AfterPipeToken_ReturnsApplicableInScopeFunctions()
    {
        var code = """
public func Wrap(value: int) -> string {
    return value.ToString()
}

public func NoInput() -> string {
    return ""
}

func Main() {
    val source: int = 1
    val result = source |> 
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("|> ", StringComparison.Ordinal) + "|> ".Length;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "Wrap");
        Assert.DoesNotContain(items, static item => item.DisplayText == "NoInput");
        Assert.DoesNotContain(items, static item => item.DisplayText == "return");
    }

    [Fact]
    public void GetCompletions_AfterPipeIdentifier_IncludesApplicableExtensionMethods()
    {
        var code = """
import System.Collections.Generic.*
import System.Linq.*

val numbers = List<int>()
val query = numbers |> Wh
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

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf("Wh", StringComparison.Ordinal) + "Wh".Length;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "Where");
    }

    [Fact]
    public void GetCompletions_AfterPipeToken_OnMemberAccessReceiver_ReturnsApplicableExtensions()
    {
        var code = """
import System.Collections.Generic.*
import System.Linq.*

class User {
}

class Db {
    public var Users: List<User> { get; set; }
}

func Main() {
    val db = Db()
    val query = db.Users |> 
}
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

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf("|> ", StringComparison.Ordinal) + "|> ".Length;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "Where");
        Assert.Contains(items, static item => item.DisplayText == "Select");
    }
}
