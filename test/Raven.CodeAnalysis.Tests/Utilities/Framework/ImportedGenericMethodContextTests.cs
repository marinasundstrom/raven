using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedGenericMethodContextTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void ImportedGenericCallAcceptsSourceParameter(bool targetMetadata, bool typeParameter)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var directory = Path.Combine(Path.GetTempPath(), "raven-open-target-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var libraryPath = Path.Combine(directory, "GenericMethodContracts.dll");
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("GenericMethodContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public static class Helpers { public static T[] One<T>(T value) => new[] { value }; } }")],
                references.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(libraryPath))
            {
                var result = declarations.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
            if (targetMetadata)
                options = options.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                    .WithTargetCoreAssemblyName("System.Runtime");
            var source = typeParameter ? """
                import Contracts.*
                public class Cell<T> {
                    private field values: T[]
                    public init(value: T) { values = Helpers.One<T>(value) }
                    public func Read() -> T { return values[0] }
                }
                public class Example {
                    public static func Run() -> int { return Cell<int>(42).Read() }
                }
                """ : """
                import Contracts.*
                public class Example {
                    public static func Copy<T>(value: T) -> T { return Helpers.One<T>(value)[0] }
                    public static func Run() -> int { return Copy<int>(42) }
                }
                """;
            var tree = SyntaxTree.ParseText(source);
            var compilation = Compilation.Create("OpenTargetSignature", [tree], references.Append(libraryPath).Select(MetadataReference.CreateFromFile).ToArray(), options);
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var emitted = compilation.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
            Assert.Equal(42, loaded.Assembly.GetType("Example")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { Directory.Delete(directory, true); }
    }
}
