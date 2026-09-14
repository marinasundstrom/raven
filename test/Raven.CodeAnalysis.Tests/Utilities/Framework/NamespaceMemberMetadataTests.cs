using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class NamespaceMemberMetadataTests
{
    [Fact]
    public void ReferenceOnlyMarkerPreservesNamespaceFunctionsForSeparateConsumers()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-namespace-metadata-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            var markerPath = Path.Combine(directory, "NamespaceMarker.dll");
            var marker = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("NamespaceMarker",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                    namespace System.Runtime.CompilerServices {
                        public sealed class TopLevelAttribute : System.Attribute { }
                    }
                    """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(markerPath))
            {
                var result = marker.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            var references = paths.Append(markerPath).Select(MetadataReference.CreateFromFile).ToArray();
            var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
            var library = Compilation.Create("Utilities", [SyntaxTree.ParseText("""
                namespace Utilities
                public func Answer() -> int { return 42 }
                public const DefaultCount: int = 7
                """)], references, options);
            var libraryPath = Path.Combine(directory, "Utilities.dll");
            using (var stream = File.Create(libraryPath))
            {
                var result = library.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            using (var image = AssemblyDefinition.ReadAssembly(libraryPath))
            {
                var container = image.MainModule.Types.Single(t => t.Namespace == "Utilities");
                var attribute = Assert.Single(container.CustomAttributes.Where(a => a.AttributeType.FullName == "System.Runtime.CompilerServices.TopLevelAttribute"));
                Assert.Equal("NamespaceMarker", attribute.AttributeType.Scope.Name);
            }
            var consumerReferences = references.Append(MetadataReference.CreateFromFile(libraryPath)).ToArray();
            var source = "import Utilities.*\npublic class Consumer { public static func Read() -> int { return Answer() + DefaultCount } }";
            var tree = SyntaxTree.ParseText(source);
            var consumer = Compilation.Create("Consumer", [tree], consumerReferences, options);
            Assert.Empty(consumer.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var emitted = consumer.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            var items = new CompletionService().GetCompletions(consumer, tree, source.IndexOf("Answer()") + 3);
            Assert.Contains(items, item => item.DisplayText == "Answer");
            var constants = new CompletionService().GetCompletions(consumer, tree, source.IndexOf("DefaultCount") + 3);
            Assert.Contains(constants, item => item.DisplayText == "DefaultCount");
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
