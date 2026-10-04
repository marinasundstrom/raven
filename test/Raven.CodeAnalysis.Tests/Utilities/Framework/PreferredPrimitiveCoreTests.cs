using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class PreferredPrimitiveCoreTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitMetadataCoreOwnsPrimitivesDespiteAnotherReferenceWithTheSameNames(bool selectAlternate)
    {
        var corePath = TestMetadataReferences.Default.OfType<PortableExecutableReference>()
            .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
        using var core = AssemblyDefinition.ReadAssembly(corePath);
        core.Name.Name = "Selected.Core";
        core.Name.PublicKey = [];
        using var image = new MemoryStream();
        core.Write(image);
        var selected = selectAlternate ? "Selected.Core" : "System.Runtime";
        var compilation = Compilation.Create("PrimitiveConsumer", [SyntaxTree.ParseText("public class Consumer { static func Echo(value: bool) -> bool => value }")],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(image.ToArray())],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMetadataImportOptions(new MetadataImportOptions(selected)));
        foreach (var kind in new[] { SpecialType.System_Object, SpecialType.System_Boolean, SpecialType.System_Int32, SpecialType.System_Int64, SpecialType.System_String })
        {
            var primitive = compilation.GetSpecialType(kind);
            Assert.NotEqual(TypeKind.Error, primitive.TypeKind);
            Assert.Equal(selected, primitive.ContainingAssembly.Name);
            Assert.Same(primitive, compilation.GetSpecialType(kind));
        }
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
