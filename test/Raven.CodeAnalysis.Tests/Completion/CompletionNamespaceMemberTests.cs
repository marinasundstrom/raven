using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionNamespaceMemberTests
{
    [Theory]
    [InlineData("import Samples.*\nfunc Main() {\n    Cre", "\n}")]
    [InlineData("func Main() {\n    Samples.Cre", "\n}")]
    public void GetCompletions_NamespaceFunctions_PreserveGenericArities(string before, string after)
    {
        var declarations = SyntaxTree.ParseText("""
namespace Samples
public func Create() {}
public func Create<T>() {}
public func Create<T, U>() {}
""");
        var tree = SyntaxTree.ParseText(before + after);
        var compilation = CreateCompilation(declarations, tree);
        var items = new CompletionService().GetCompletions(compilation, tree, before.Length).ToArray();

        foreach (var arity in new[] { 0, 1, 2 })
        {
            var item = Assert.Single(items, item => item.Symbol is IMethodSymbol method && method.Name == "Create" && method.Arity == arity);
            Assert.Equal(new[] { "Create", "Create<T>", "Create<T, U>" }[arity], item.DisplayText);
            Assert.Equal("Create()", item.InsertionText);
        }
    }

    [Fact]
    public void GetCompletions_InSameNamespace_IncludesSourceNamespaceFunctionAndConstMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Samples.App

public func Test() -> unit {
}

public const DefaultCount: int = 3
""");
        var mainSource = """
namespace Samples.App

func Main() -> unit {
    Tes
    Def
}
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(membersTree, mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Tes") + "Tes".Length).ToList();
        var constItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Def") + "Def".Length).ToList();

        Assert.Contains(functionItems, static item => item.DisplayText == "Test");
        Assert.Contains(constItems, static item => item.DisplayText == "DefaultCount");
    }

    [Fact]
    public void GetCompletions_InGlobalNamespace_IncludesSourceNamespaceFunctionMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
public func Test() -> unit {
}
""");
        var mainSource = """
func Main() -> unit {
    Tes
}
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(membersTree, mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Tes") + "Tes".Length).ToList();

        Assert.Contains(functionItems, static item => item.DisplayText == "Test");
    }

    [Fact]
    public void GetCompletions_InGlobalTopLevelProgram_IncludesSourceNamespaceFunctionMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
public func Test() -> unit {
}
""");
        var mainSource = """
Tes
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(membersTree, mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Tes") + "Tes".Length).ToList();

        Assert.Contains(functionItems, static item => item.DisplayText == "Test");
    }

    [Fact]
    public void GetCompletions_InNamedNamespace_DoesNotIncludeUnimportedGlobalFunctionMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
public func Test() -> unit {
}
""");
        var mainSource = """
namespace Samples.App

func Main() -> unit {
    Tes
}
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(membersTree, mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Tes") + "Tes".Length).ToList();

        Assert.DoesNotContain(functionItems, static item => item.DisplayText == "Test");
    }

    [Fact]
    public void GetCompletions_WhenNamespaceMemberImportsDisabled_OmitsSourceNamespaceFunctionAndConstMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Samples.App

public func Test() -> unit {
}

public const DefaultCount: int = 3
""");
        var mainSource = """
namespace Samples.App

func Main() -> unit {
    Tes
    Def
}
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(
            new CompilationOptions(OutputKind.ConsoleApplication).WithAllowNamespaceMemberImports(false),
            membersTree,
            mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Tes") + "Tes".Length).ToList();
        var constItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Def") + "Def".Length).ToList();

        Assert.DoesNotContain(functionItems, static item => item.DisplayText == "Test");
        Assert.DoesNotContain(constItems, static item => item.DisplayText == "DefaultCount");
    }

    [Fact]
    public void GetCompletions_WhenNamespaceMembersDisabled_OmitsSourceNamespaceFunctionAndConstMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Samples.App

public func Test() -> unit {
}

public const DefaultCount: int = 3
""");
        var mainSource = """
namespace Samples.App

func Main() -> unit {
    Tes
    Def
}
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(
            new CompilationOptions(OutputKind.ConsoleApplication).WithAllowNamespaceMembers(false),
            membersTree,
            mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Tes") + "Tes".Length).ToList();
        var constItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("Def") + "Def".Length).ToList();

        Assert.DoesNotContain(functionItems, static item => item.DisplayText == "Test");
        Assert.DoesNotContain(constItems, static item => item.DisplayText == "DefaultCount");
    }

    [Fact]
    public void GetCompletions_WithNamespaceMemberImport_IncludesImportedFunctionAndConstMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Samples.Shared

public func TestShared() -> unit {
}

public const SharedCount: int = 3
""");
        var mainSource = """
namespace Samples.App

import Samples.Shared.*

func Main() -> unit {
    TestS
    SharedC
}
""";
        var mainTree = SyntaxTree.ParseText(mainSource);
        var compilation = CreateCompilation(membersTree, mainTree);
        var service = new CompletionService();

        var functionItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("TestS") + "TestS".Length).ToList();
        var constItems = service.GetCompletions(compilation, mainTree, mainSource.IndexOf("SharedC") + "SharedC".Length).ToList();

        Assert.Contains(functionItems, static item => item.DisplayText == "TestShared");
        Assert.Contains(constItems, static item => item.DisplayText == "SharedCount");
    }

    private static Compilation CreateCompilation(params SyntaxTree[] syntaxTrees)
        => Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTrees)
            .AddReferences(TestMetadataReferences.Default);

    private static Compilation CreateCompilation(CompilationOptions options, params SyntaxTree[] syntaxTrees)
        => Compilation.Create("test", options)
            .AddSyntaxTrees(syntaxTrees)
            .AddReferences(TestMetadataReferences.Default);
}
