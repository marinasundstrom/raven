using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionApiTests
{
    [Fact]
    public void SemanticModel_GetCompletions_DelegatesToCompletionService()
    {
        var code = "va";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var fromApi = semanticModel.GetCompletions(code.Length)
            .Select(item => item.DisplayText)
            .ToArray();

        var fromService = new CompletionService()
            .GetCompletions(compilation, syntaxTree, code.Length)
            .Select(item => item.DisplayText)
            .ToArray();

        Assert.Equal(fromService, fromApi);
        Assert.Contains("var", fromApi);
    }

    [Fact]
    public void Compilation_GetCompletions_ForTree_UsesSemanticModelApi()
    {
        var code = "ret";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var fromCompilation = compilation.GetCompletions(syntaxTree, code.Length)
            .Select(item => item.DisplayText)
            .ToArray();

        var fromSemanticModel = compilation.GetSemanticModel(syntaxTree)
            .GetCompletions(code.Length)
            .Select(item => item.DisplayText)
            .ToArray();

        Assert.Equal(fromSemanticModel, fromCompilation);
        Assert.Contains("return", fromCompilation);
    }
}
