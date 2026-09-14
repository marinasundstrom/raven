using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetCoreGenericSignatureTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ImportedConstructedSignatureAcceptsSourceMethodParameter(bool targetMetadata)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var directory = Path.Combine(Path.GetTempPath(), "raven-open-target-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var libraryPath = Path.Combine(directory, "SignatureContracts.dll");
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("SignatureContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public class Box<T> { public Box(T value) { Value = value; } public T Value; } }")],
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
            var tree = SyntaxTree.ParseText("""
            import Contracts.*
            public class Example {
                public static func Identity<T>(source: Box<T>) -> Box<T> {
                    return source
                }
                public static func Run() -> int {
                    let item = Box<int>(42)
                    return Identity<int>(item).Value
                }
            }
            """);
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
