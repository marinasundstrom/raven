using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceUnionCaseTests
{
    [Fact]
    public void GetCompletions_AfterDot_OnDiscriminatedUnionType_ShowsQualifiedCaseDisplayText()
    {
        var code = """
union Result {
    case Ok
    case Err(message: string)
}

Result.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        var ok = Assert.Single(items.Where(i => i.DisplayText == "Result.Ok"));
        var err = Assert.Single(items.Where(i => i.DisplayText == "Result.Err"));

        Assert.Equal("Ok", ok.InsertionText);
        Assert.Equal("Err", err.InsertionText);
    }

    [Fact]
    public void GetCompletions_OnValuePrefix_ShowsQualifiedCaseDisplayText()
    {
        var code = """
union ParseResult {
    case Ok
}

val value = O
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        var ok = Assert.Single(items.Where(i => i.DisplayText == "ParseResult.Ok"));
        Assert.Equal("Ok", ok.InsertionText);
    }
}
