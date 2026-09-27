using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ImportedTypeArityTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void SimpleStaticReceiverSelectsNonGenericTypeRegardlessOfMetadataOrder(bool genericFirst)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-type-arity", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "ArityContracts.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            const string generic = "public class Task<T> { }";
            const string nonGeneric = "public static class Task { public static int Run() => 42; }";
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("ArityContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(
                    "namespace Contracts { " + (genericFirst ? generic + nonGeneric : nonGeneric + generic) + " }")],
                paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
            {
                var result = declarations.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            var compilation = Compilation.Create("ArityConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                public class LocalRunner {
                    func Run() -> int { return 17 }
                }
                public class Example {
                    static func Run() -> int { return Task.Run() }
                    static func Identity(value: Task<int>) -> Task<int> { return value }
                    static func Shadow() -> int {
                        let Task = LocalRunner()
                        return Task.Run()
                    }
                    static func Parameter(Task: LocalRunner) -> int { return Task.Run() }
                }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using var output = new MemoryStream();
            var emitted = compilation.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
            var example = loaded.Assembly.GetType("Example")!;
            Assert.Equal(42, example.GetMethod("Run")!.Invoke(null, null));
            Assert.Equal(17, example.GetMethod("Shadow")!.Invoke(null, null));
            var runner = Activator.CreateInstance(loaded.Assembly.GetType("LocalRunner")!);
            Assert.Equal(17, example.GetMethod("Parameter")!.Invoke(null, [runner]));
            var identity = example.GetMethod("Identity")!;
            Assert.Equal(typeof(int), Assert.Single(identity.ReturnType.GenericTypeArguments));
            Assert.Equal(identity.ReturnType, identity.GetParameters().Single().ParameterType);
        }
        finally
        {
            Directory.Delete(directory, true);
        }
    }
}
