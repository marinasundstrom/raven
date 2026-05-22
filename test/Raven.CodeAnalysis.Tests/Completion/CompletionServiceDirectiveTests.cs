using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceDirectiveTests
{
    [Fact]
    public void GetCompletions_InImportDirective_ReturnsWildcardNamespacesAndTypesOnly()
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

        Assert.Equal("*", items[0].DisplayText);
        Assert.Equal("*", items[0].InsertionText);
        Assert.Equal("Import all accessible members", items[0].Description);
        Assert.Contains(items, i => i.DisplayText == "SpecialFolder");
        Assert.DoesNotContain(items, i => i.DisplayText == "GetFolderPath");
    }

    [Fact]
    public void GetCompletions_InImportDirective_WildcardPrefixReturnsWildcard()
    {
        var code = """
import System.*
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
        var position = code.LastIndexOf('*') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        var wildcard = Assert.Single(items.Where(i => i.DisplayText == "*"));
        Assert.Equal("*", wildcard.InsertionText);
        Assert.Equal("Import all accessible members", wildcard.Description);
    }

    [Fact]
    public void GetCompletions_InImportDirective_AfterNamespaceChainTrailingDot_ReturnsWildcardAndTypes()
    {
        var code = """
import System.Collections.Generic.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Equal("*", items[0].DisplayText);
        Assert.Contains(items, i => i.DisplayText == "List");
    }

    [Fact]
    public void GetCompletions_InImportDirective_AfterRootNamespaceTrailingDot_ReturnsWildcardAndMembers()
    {
        var code = """
import System.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Equal("*", items[0].DisplayText);
        Assert.Equal(new TextSpan(position, 0), items[0].ReplacementSpan);
        Assert.Contains(items, i => i.DisplayText == "String");
        Assert.All(items, item => Assert.Equal(new TextSpan(position, 0), item.ReplacementSpan));
    }

    [Fact]
    public void GetCompletions_InImportDirective_AfterMetadataNamespaceTrailingDot_ReturnsWildcardAndMembers()
    {
        var code = """
import System.Net.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Net.Http.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Net.Primitives.dll")),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Equal("*", items[0].DisplayText);
        Assert.Equal(new TextSpan(position, 0), items[0].ReplacementSpan);
        Assert.Contains(items, i => i.DisplayText == "Http");
        Assert.Contains(items, i => i.DisplayText == "IPAddress");
        Assert.All(items, item => Assert.Equal(new TextSpan(position, 0), item.ReplacementSpan));
    }

    [Theory]
    [InlineData("""
namespace App

import System.Collections.Generic.
""")]
    [InlineData("""
namespace App {
    import System.Collections.Generic.
}
""")]
    public void GetCompletions_InNamespaceScopedImportDirective_AfterNamespaceChainTrailingDot_ReturnsWildcardAndTypes(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Equal("*", items[0].DisplayText);
        Assert.Equal(new TextSpan(position, 0), items[0].ReplacementSpan);
        Assert.Contains(items, i => i.DisplayText == "List");
        Assert.All(items, item => Assert.Equal(new TextSpan(position, 0), item.ReplacementSpan));
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
}
