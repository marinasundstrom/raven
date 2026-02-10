using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceBasicTests
{
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
}
