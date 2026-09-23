using System.Runtime.Loader;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedUnitInterfaceTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConfiguredUnitMatchesImportedGenericInterfaceReturn(bool configured)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-unit-contract", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "UnitContracts.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("UnitContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                    namespace Contracts {
                        public interface Finisher { System.Collections.Generic.List<System.ValueTuple> Finish(); }
                    }
                    """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
            {
                var result = declarations.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            var compilation = Compilation.Create("UnitConsumer", [SyntaxTree.ParseText("""
                namespace Consumer
                import System.Collections.Generic.*
                public class UnitFinisher : Contracts.Finisher {
                    func Finish() -> List<unit> { return List<unit>() }
                }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                    .WithTargetCoreAssemblyName("System.Runtime")
                    .WithRuntimeUnitContract(configured ? new RuntimeUnitContract("System.Runtime", "System.ValueTuple") : null));
            if (!configured)
            {
                Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0330");
                return;
            }
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            var reader = compilation.GetTypeByMetadataName("Consumer.UnitFinisher")!;
            var actual = reader.GetMembers("Finish").OfType<IMethodSymbol>().Single().ReturnType;
            var expected = reader.Interfaces.Single().GetMembers("Finish").OfType<IMethodSymbol>().Single().ReturnType;
            Assert.True(SymbolEqualityComparer.Default.Equals(expected, actual));
            Assert.True(SymbolEqualityComparer.Default.Equals(actual, expected));
            Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(expected), SymbolEqualityComparer.Default.GetHashCode(actual));
            Assert.Contains(actual, new HashSet<ISymbol>(SymbolEqualityComparer.Default) { expected });
            using var output = new MemoryStream();
            var emitted = compilation.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            var context = new AssemblyLoadContext("UnitConsumer", isCollectible: true);
            try
            {
                var contracts = context.LoadFromAssemblyPath(path);
                output.Position = 0;
                var consumer = context.LoadFromStream(output);
                var instance = Activator.CreateInstance(consumer.GetType("Consumer.UnitFinisher")!);
                var finish = contracts.GetType("Contracts.Finisher")!.GetMethod("Finish")!;
                Assert.Empty(Assert.IsType<List<ValueTuple>>(finish.Invoke(instance, null)));
            }
            finally { context.Unload(); }
        }
        finally { Directory.Delete(directory, true); }
    }
}
