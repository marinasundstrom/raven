using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class MetadataReferenceLoadingTests
{
    [Fact]
    public void GetTypeByMetadataName_LoadsReferences_WhenNoSyntaxTrees()
    {
        var referencePaths = TargetFrameworkResolver.GetReferenceAssemblyPaths();
        var references = referencePaths.Select(MetadataReference.CreateFromFile).ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(references);

        var consoleType = compilation.GetTypeByMetadataName("System.Console");
        Assert.NotNull(consoleType);

        var stringType = compilation.GetTypeByMetadataName("System.String");
        Assert.NotNull(stringType);
    }
}
