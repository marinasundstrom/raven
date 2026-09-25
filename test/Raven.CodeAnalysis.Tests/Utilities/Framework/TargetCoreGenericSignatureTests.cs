using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetCoreGenericSignatureTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExtensionSignatureRetainsNullableReferenceGenericArgument(bool targetMetadata)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        if (targetMetadata)
            options = options.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                .WithTargetCoreAssemblyName("System.Runtime");
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Collections.Generic.*

            public extension NullableItems for string {
                func Identity(items: List<object?>) -> List<object?> {
                    return items
                }
            }
            """);
        var compilation = Compilation.Create("NullableTargetSignature", [tree],
            references.Select(MetadataReference.CreateFromFile).ToArray(), options);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        var method = loaded.Assembly.GetType("NullableItems")!.GetMethod("Identity")!;
        Assert.Equal(typeof(List<object>), method.ReturnType);
        var values = new List<object?> { null, "value" };
        Assert.Same(values, method.Invoke(null, ["receiver", values]));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void ImportedConstructedSignatureAcceptsSourceMethodParameter(bool targetMetadata, bool sameNamespace)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var directory = Path.Combine(Path.GetTempPath(), "raven-open-target-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var libraryPath = Path.Combine(directory, "SignatureContracts.dll");
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("SignatureContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public class Box<T> { public Box(T value) { Value = value; } public T Value; public T GetValue() => Value; } }")],
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
            var tree = SyntaxTree.ParseText((sameNamespace ? "namespace Contracts\n" : "import Contracts.*\n") + """
            public class Example {
                public static func Identity<T>(source: Box<T>) -> Box<T> {
                    return source
                }
                public static func Read<T>(source: Box<T>) -> T {
                    return Identity<T>(source).GetValue()
                }
                public static func Run() -> int {
                    let item = Box<int>(42)
                    return Read<int>(item)
                }
            }
            """);
            var compilation = Compilation.Create("OpenTargetSignature", [tree], references.Append(libraryPath).Select(MetadataReference.CreateFromFile).ToArray(), options);
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var emitted = compilation.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
            Assert.Equal(42, loaded.Assembly.GetType(sameNamespace ? "Contracts.Example" : "Example")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { Directory.Delete(directory, true); }
    }
}
