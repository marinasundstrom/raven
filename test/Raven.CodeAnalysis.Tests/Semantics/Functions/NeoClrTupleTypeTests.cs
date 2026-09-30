using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NeoClrTupleTypeTests : CompilationTestBase
{
    [Theory]
    [InlineData("NeoCLR.CoreProbe", true, SpecialType.System_ValueTuple_T2)]
    [InlineData("OrdinaryLibrary", true, SpecialType.None)]
    [InlineData("NeoCLR.CoreProbe", false, SpecialType.None)]
    [InlineData("neoclr.coreprobe", true, SpecialType.None)]
    public void ImportedTupleRequiresRuntimeAssemblyAndValueType(
        string assemblyName, bool isValueType, SpecialType expected)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-tuple-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var declarations = $$"""
                namespace System {
                    public {{(isValueType ? "struct" : "class")}} Tuple<T1, T2> {
                        public T1 Item1;
                        public T2 Item2;
                    }
                }
                """;
            var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net10.0"))
                .Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path));
            var metadata = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create(assemblyName,
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(declarations)], references,
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            var path = Path.Combine(directory, assemblyName + ".dll");
            using (var output = File.Create(path))
            {
                var result = metadata.Emit(output);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }

            var (compilation, _) = CreateCompilation("",
                references: TestMetadataReferences.Default.Concat([MetadataReference.CreateFromFile(path)]).ToArray());
            var tuple = compilation.GetTypeByMetadataName("System.Tuple`2", assemblyName);
            Assert.NotNull(tuple);
            Assert.Equal(isValueType, tuple.IsValueType);
            Assert.Equal(expected, tuple.SpecialType);
            Assert.Equal(SpecialType.System_ValueTuple_T2,
                compilation.GetSpecialType(SpecialType.System_ValueTuple_T2).SpecialType);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
