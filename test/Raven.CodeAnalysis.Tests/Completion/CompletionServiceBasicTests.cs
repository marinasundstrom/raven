using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceBasicTests
{
    [Theory]
    [InlineData("    ", "Console", "")]
    [InlineData("    ", "Con", "")]
    [InlineData("\t", "Con", "")]
    [InlineData("    /* keep */ ", "Con", " // keep too")]
    public void GetCompletions_AcceptingConsole_PreservesSurroundingTrivia(
        string leadingTrivia, string prefix, string trailingTrivia)
    {
        var code = "import System.*\n\nfunc Main() {\n" + leadingTrivia + prefix + trailingTrivia + "\n}";
        var position = code.LastIndexOf(prefix, System.StringComparison.Ordinal) + prefix.Length;
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var item = Assert.Single(new CompletionService().GetCompletions(compilation, syntaxTree, position),
            item => item.DisplayText == "Console");
        var completedCode = code.Remove(item.ReplacementSpan.Start, item.ReplacementSpan.Length)
            .Insert(item.ReplacementSpan.Start, item.InsertionText);

        Assert.Equal("import System.*\n\nfunc Main() {\n" + leadingTrivia + "Console" + trailingTrivia + "\n}",
            completedCode);
    }

    [Fact]
    public void GetCompletions_WithoutMetadataReferences_ReturnsBasicKeywordCompletions()
    {
        var code = "ret";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree);

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, code.Length).ToList();

        Assert.Contains(items, i => i.DisplayText == "return");
    }

    [Fact]
    public void GetCompletions_WithoutMetadataReferences_UsesPrefixFilteringForBasicKeywords()
    {
        var code = "va";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree);

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, code.Length).ToList();

        Assert.Contains(items, i => i.DisplayText == "var");
        Assert.DoesNotContain(items, i => i.DisplayText == "return");
    }

    [Fact]
    public void GetCompletions_WithWildcardImportedEnum_ReturnsEnumMembersAsValues()
    {
        var code = """
import DeviceType.*

enum DeviceType {
    Monitor
    CPU
}

func Pick() -> DeviceType {
    return Mo
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf("Mo", System.StringComparison.Ordinal) + "Mo".Length;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Monitor");
    }

    [Fact]
    public void GetCompletions_WithWildcardTypeImport_DoesNotReturnInstanceMembersAsValues()
    {
        var code = """
import Result.*

union Result {
    case Ok

    func Tap() -> () {
    }

    static func TryCreate() -> Result {
        return Ok()
    }
}

T
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('T') + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, static item => item.DisplayText == "Tap");
        Assert.DoesNotContain(items, static item => item.DisplayText == "ToString");
        Assert.DoesNotContain(items, static item => item.DisplayText == "TryGetValue");
        Assert.Contains(items, static item => item.DisplayText == "TryCreate");
    }

    [Fact]
    public void GetCompletions_WithWildcardTypeImport_StillReturnsImportedUnionCases()
    {
        var code = """
import Result.*

union Result {
    case Ok
    case Error(message: string)
}

O
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('O') + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.InsertionText == "Ok");
    }
}
