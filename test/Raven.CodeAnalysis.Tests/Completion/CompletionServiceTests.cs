using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceTests
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

let text = "";
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

let numbers = new List<int>();
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
    public void GetCompletions_InImportDirective_ReturnsNamespacesAndTypesOnly()
    {
        var code = """
import System.Environment.
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
                MetadataReference.CreateFromFile(typeof(Environment).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "SpecialFolder");
        Assert.DoesNotContain(items, i => i.DisplayText == "GetFolderPath");
    }

    [Fact]
    public void GetCompletions_InAliasDirective_ReturnsMembers()
    {
        var code = """
alias W = System.Console.
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
    }

    [Fact]
    public void GetCompletions_InAliasDirective_ForNamespaceRoot_ReturnsMembers()
    {
        var code = """
alias S = System.
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

        Assert.Contains(items, i => i.DisplayText == "Console");
    }

    [Fact]
    public void GetCompletions_OnTypeAlias_ReturnsMembers()
    {
        var code = """
alias Terminal = System.Console
Terminal.
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
    }

    [Fact]
    public void GetCompletions_OnNamespaceAlias_ReturnsMembers()
    {
        var code = """
alias ST = System.Text
ST.
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
                MetadataReference.CreateFromFile(typeof(System.Text.StringBuilder).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "StringBuilder");
    }

    [Fact]
    public void GetCompletions_OnLiteralUnionLocal_SuggestsAllMembers()
    {
        var code = "let response: \"כן\" | \"לא\" = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "\"כן\"");
        Assert.Contains(items, i => i.DisplayText == "\"לא\"");
    }

    [Fact]
    public void GetCompletions_OnLiteralUnionAssignment_SuggestsAllMembers()
    {
        var code = "let response: \"כן\" | \"לא\" = \"כן\";\nresponse = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "\"כן\"");
        Assert.Contains(items, i => i.DisplayText == "\"לא\"");
    }

    [Fact]
    public void GetCompletions_InInstanceMethod_SuggestsSelfKeyword()
    {
        var code = """
        class Counter {
            public Increment() -> unit {
                sel
            }
        }
        """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.LastIndexOf("sel", StringComparison.Ordinal) + "sel".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact]
    public void GetCompletions_OnNumericLiteral_SuggestsLiteralValue()
    {
        var code = "let value: 100 = ";
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "100");
    }
}
