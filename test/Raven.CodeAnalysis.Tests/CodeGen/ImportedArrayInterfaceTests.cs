using System.Runtime.Loader;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedArrayInterfaceTests
{
    [Fact]
    public void ImportedArrayParametersMatchSourceAndDispatchThroughInterface()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-array-contract", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "ArrayContracts.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("ArrayContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                    namespace Contracts {
                        public interface Reader { int Read(byte[] buffer); }
                    }
                    """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
            {
                var result = declarations.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            var compilation = Compilation.Create("ArrayConsumer", [SyntaxTree.ParseText("""
                namespace Consumer
                public class ByteReader : Contracts.Reader {
                    func Read(buffer: byte[]) -> int { return buffer.Length }
                }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            var reader = compilation.GetTypeByMetadataName("Consumer.ByteReader")!;
            var actual = reader.GetMembers("Read").OfType<IMethodSymbol>().Single().Parameters[0].Type;
            var expected = reader.Interfaces.Single().GetMembers("Read").OfType<IMethodSymbol>().Single().Parameters[0].Type;
            Assert.True(SymbolEqualityComparer.Default.Equals(expected, actual));
            Assert.True(SymbolEqualityComparer.Default.Equals(actual, expected));
            Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(expected), SymbolEqualityComparer.Default.GetHashCode(actual));
            Assert.Contains(actual, new HashSet<ISymbol>(SymbolEqualityComparer.Default) { expected });
            using var output = new MemoryStream();
            var emitted = compilation.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            var context = new AssemblyLoadContext("ArrayConsumer", isCollectible: true);
            try
            {
                var contracts = context.LoadFromAssemblyPath(path);
                output.Position = 0;
                var consumer = context.LoadFromStream(output);
                var instance = Activator.CreateInstance(consumer.GetType("Consumer.ByteReader")!);
                var read = contracts.GetType("Contracts.Reader")!.GetMethod("Read")!;
                Assert.Equal(3, read.Invoke(instance, [new byte[3]]));
            }
            finally { context.Unload(); }
        }
        finally { Directory.Delete(directory, true); }
    }
}
