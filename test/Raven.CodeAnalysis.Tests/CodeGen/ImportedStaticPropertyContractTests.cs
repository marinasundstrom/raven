using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ImportedStaticPropertyContractTests
{
    [Theory]
    [InlineData("", false)]
    [InlineData("val Zero: int => 0", false)]
    [InlineData("static val Zero: string => \"wrong\"", false)]
    [InlineData("static val Zero: int => 0", true)]
    public void ImportedStaticPropertyRequiresMatchingStaticImplementation(string implementation, bool valid)
    {
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var contract = Compilation.Create("StaticPropertyContract", [SyntaxTree.ParseText("""
            public interface Identity {
                static val Zero: int { get; }
            }
            """)], TestMetadataReferences.Default, options);
        using var image = new MemoryStream();
        var result = contract.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var consumer = Compilation.Create("StaticPropertyConsumer", [SyntaxTree.ParseText(
            "public class Consumer : Identity {\n" + implementation + "\n}")],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(image.ToArray())], options);
        var errors = consumer.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (valid) Assert.Empty(errors);
        else Assert.Contains(errors, d => d.Id == "RAV0330");
    }
}
