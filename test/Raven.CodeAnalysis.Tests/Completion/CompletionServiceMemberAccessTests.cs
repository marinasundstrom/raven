using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceMemberAccessTests
{
    [Fact]
    public void GetCompletions_AfterDot_OnType_ReturnsStaticMembers()
    {
        var code = """
import System.*;

string.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "IsNullOrEmpty");
        Assert.DoesNotContain(items, i => i.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnVariable_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

val text = "";
text.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
        Assert.DoesNotContain(items, i => i.DisplayText == "IsNullOrEmpty");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnLiteralType_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

val literal: "foo" = "foo";
literal.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnProperty_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

Console.Out.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "WriteLine");
        Assert.DoesNotContain(items, i => i.DisplayText == "Synchronized");
    }

    [Fact]
    public void GetCompletions_AfterDot_IncludesExtensionMethods()
    {
        var code = """
import System.Collections.Generic.*;
import System.Linq.*;

val numbers = new List<int>();
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

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Where");
    }

    [Fact]
    public void GetCompletions_OnType_DoesNotIncludeAccessorMethods()
    {
        var code = """
import System.*;

Console.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, i => i.DisplayText == "get_Out");
        Assert.DoesNotContain(items, i => i.DisplayText == "set_Out");
        Assert.DoesNotContain(items, i => i.DisplayText == "add_CancelKeyPress");
        Assert.DoesNotContain(items, i => i.DisplayText == "remove_CancelKeyPress");
    }

    [Fact]
    public void GetCompletions_OnType_ExcludesInaccessibleMembers()
    {
        var code = """
class Container {
    private static Hidden() -> unit { }
    public static Visible() -> unit { }
}

Container.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Visible");
        Assert.DoesNotContain(items, i => i.DisplayText == "Hidden");
    }

    [Fact]
    public void GetCompletions_InInstanceMethod_IncludesSelf()
    {
        var code = """
class Counter {
    private value: int;

    public Increment(delta: int) -> int {
        sel
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf("sel", StringComparison.Ordinal) + "sel".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact]
    public void GetCompletions_InInstanceMethodWithoutIdentifier_IncludesSelf()
    {
        var code = """
class Counter {
    private value: int;

    public Increment(delta: int) -> int {
        /*caret*/
    }
}
""";

        var caret = code.IndexOf("/*caret*/", StringComparison.Ordinal);
        code = code.Remove(caret, "/*caret*/".Length);

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, caret).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact]
    public void GetCompletions_InNamedConstructor_IncludesSelf()
    {
        var code = """
class Counter {
    private value: int;

    public init WithValue(value: int) {
        /*caret*/
    }
}
""";

        var caret = code.IndexOf("/*caret*/", StringComparison.Ordinal);
        code = code.Remove(caret, "/*caret*/".Length);

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, caret).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact]
    public void GetCompletions_OnSelfMemberAccess_ReturnsInstanceMembers()
    {
        var code = """
class Counter {
    private value: int;

    public Increment(delta: int) -> int {
        self.
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "value");
    }
}
