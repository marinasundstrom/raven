using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetSpecialTypeTests
{
    [Theory]
    [InlineData("net10.0")]
    [InlineData("net11.0")]
    public void SpecialTypesUseTheReferenceContractDespiteSourceNameCollisions(string framework)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(framework));
        var compilation = Compilation.Create("ContractTypes",
            [SyntaxTree.ParseText("namespace System.Collections.Generic\nclass IEnumerable<T> {}")],
            references.Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime")));

        var expectedTypes = new (SpecialType SpecialType, string MetadataName)[]
        {
            (SpecialType.System_Object, "System.Object"),
            (SpecialType.System_Collections_Generic_IEnumerable_T, "System.Collections.Generic.IEnumerable`1"),
            (SpecialType.System_Collections_Generic_IEnumerator_T, "System.Collections.Generic.IEnumerator`1"),
            (SpecialType.System_Threading_Tasks_Task_T, "System.Threading.Tasks.Task`1"),
            (SpecialType.System_ValueTuple_T2, "System.ValueTuple`2"),
            (SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T, "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1")
        };

        foreach (var (specialType, metadataName) in expectedTypes)
        {
            var type = compilation.GetSpecialType(specialType);
            Assert.NotEqual(TypeKind.Error, type.TypeKind);
            Assert.Equal("System.Runtime", type.ContainingAssembly.Name);
            Assert.Same(type.ContainingAssembly.GetTypeByMetadataName(metadataName), type);
            Assert.Same(type, compilation.GetSpecialType(specialType));
        }

        var declaration = compilation.SyntaxTrees.Single().GetRoot().DescendantNodes()
            .OfType<ClassDeclarationSyntax>().Single();
        var sourceType = compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDeclaredSymbol(declaration);
        Assert.NotNull(sourceType);
        Assert.Same(compilation.Assembly, sourceType.ContainingAssembly);
        Assert.NotSame(sourceType, compilation.GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T));
    }
}
