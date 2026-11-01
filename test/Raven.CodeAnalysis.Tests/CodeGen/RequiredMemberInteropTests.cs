using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class RequiredMemberInteropTests
{
    [Theory]
    [InlineData("public record Value(Text: string)", "new Value(\"text\")")]
    [InlineData("[assembly: System.Runtime.CompilerServices.InternalsVisibleTo(\"RequiredConsumer\")] internal record Value(Text: string) { override func ToString() -> string => Text }", "new Value(\"text\")")]
    [InlineData("public record struct Value(Text: string)", "new Value(\"text\")")]
    [InlineData("public class Value { public required var Text: string { get; init; } }", "new Value { Text = \"text\" }")]
    public void CSharpCanConstructTypesWithRequiredMembers(string source, string construction)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var compilation = Compilation.Create("RequiredLibrary", [SyntaxTree.ParseText(source)],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        var consumer = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("RequiredConsumer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($"public class Consumer {{ public object Create() => {construction}; }}")],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path))
                .Append(Microsoft.CodeAnalysis.MetadataReference.CreateFromImage(image.ToArray())),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var result = new MemoryStream();
        var consumed = consumer.Emit(result);
        Assert.True(consumed.Success, string.Join(Environment.NewLine, consumed.Diagnostics));
        if (source.Contains("required"))
        {
            var missingInitializer = consumer.RemoveAllSyntaxTrees().AddSyntaxTrees(
                Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("public class Consumer { public object Create() => new Value(); }"));
            Assert.Contains(missingInitializer.GetDiagnostics(), diagnostic => diagnostic.Id == "CS9035");
        }
    }
    [Theory]
    [InlineData("", false)]
    [InlineData("[System.Obsolete]", false)]
    [InlineData("[System.Diagnostics.CodeAnalysis.SetsRequiredMembers]", true)]
    public void ImportedConstructorRecognizesOnlySetsRequiredMembers(string attribute, bool expected)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var library = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("RequiredContracts",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($"public class Imported {{ {attribute} public Imported() {{ }} }}")],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = library.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        var compilation = Compilation.Create("RequiredImport", [],
            paths.Select(path => MetadataReference.CreateFromFile(path)).Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var constructor = compilation.GetTypeByMetadataName("Imported")!.Constructors.Single();
        Assert.Equal(expected, constructor.SetsRequiredMembers);
    }
}
