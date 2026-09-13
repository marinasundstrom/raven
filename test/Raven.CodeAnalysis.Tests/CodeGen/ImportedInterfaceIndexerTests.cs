using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedInterfaceIndexerTests
{
    [Theory]
    [InlineData("DerivedWrite", false)]
    [InlineData("ReadAgain", true)]
    public void InheritedIndexersRespectMostDerivedAccessors(string receiver, bool readOnly)
    {
        WithContracts(references =>
        {
            var tree = SyntaxTree.ParseText($$"""
                import Contracts.*
                func Read(values: {{receiver}}<int>) -> int { return values[0] }
                func Write(values: {{receiver}}<int>) { values[0] = 42 }
                """);
            var compilation = Compilation.Create("IndexerConsumer", [tree], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (readOnly) Assert.Single(errors);
            else
            {
                Assert.Empty(errors);
                using var stream = new MemoryStream();
                var result = compilation.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
        });
    }

    [Fact]
    public void UnrelatedInheritedIndexersDoNotChooseAnArbitraryInterface()
    {
        WithContracts(references =>
        {
            var compilation = Compilation.Create("AmbiguousIndexer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Read(values: AmbiguousRead<int>) -> int { return values[0] }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error &&
                d.GetMessage().Contains("ambiguous", StringComparison.OrdinalIgnoreCase));
        });
    }

    private static void WithContracts(Action<MetadataReference[]> action)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-indexer-review", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "IndexerContracts.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            const string source = """
                namespace Contracts {
                    public interface Read<T> { T this[int index] { get; } }
                    public interface Write<T> : Read<T> { new T this[int index] { get; set; } }
                    public interface DerivedWrite<T> : Write<T> { }
                    public interface AlternativeRead<T> { T this[int index] { get; } }
                    public interface AmbiguousRead<T> : Read<T>, AlternativeRead<T> { }
                    public interface ReadAgain<T> : Write<T> { new T this[int index] { get; } }
                }
                """;
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("IndexerContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)],
                paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
            {
                var result = declarations.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            action(paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray());
        }
        finally { Directory.Delete(directory, true); }
    }
}
