using System;
using System.IO;
using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
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
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "WriteLine");
        Assert.DoesNotContain(items, i => i.DisplayText == "Synchronized");
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
                MetadataReference.CreateFromFile(typeof(Environment).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "SpecialFolder");
        Assert.DoesNotContain(items, i => i.DisplayText == "GetFolderPath");
    }
}
