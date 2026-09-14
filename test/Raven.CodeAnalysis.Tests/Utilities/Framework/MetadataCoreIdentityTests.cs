using System.Reflection;
using System.Runtime.Loader;

using Mono.Cecil;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MetadataCoreIdentityTests
{
    [Theory]
    [InlineData("net10.0", "struct", false)]
    [InlineData("net10.0", "struct", true)]
    [InlineData("net11.0", "struct", false)]
    [InlineData("net11.0", "struct", true)]
    [InlineData("net10.0", "class", false)]
    [InlineData("net10.0", "class", true)]
    [InlineData("net11.0", "class", false)]
    [InlineData("net11.0", "class", true)]
    public void ImportedTypeKeepsItsCategoryAndExecutes(string framework, string category, bool retarget)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-core-identity", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var libraryName = $"TypeContracts_{Guid.NewGuid():N}";
        var path = Path.Combine(directory, libraryName + ".dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(framework));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create(libraryName,
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
                namespace Contracts {
                    public {{category}} Payload<T> {
                        public T Value;
                        public Payload(T value) { Value = value; }
                        public T Get() => Value;
                    }
                }
                """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var references = paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray();
            var compilation = Compilation.Create("TypeConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                public class Consumer {
                    public static func Copy(value: Payload<int>) -> Payload<int> {
                        let copy = value
                        return copy
                    }
                    public static func Run() -> int {
                        let value = Payload<int>(42)
                        return Copy(value).Get()
                    }
                }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            var coreIdentity = AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"));
            Assert.Equal(coreIdentity.FullName, compilation.CoreAssembly.GetName().FullName);
            var symbol = Assert.IsAssignableFrom<PENamedTypeSymbol>(compilation.GetTypeByMetadataName("Contracts.Payload`1"));
            Assert.Equal(category == "struct", symbol.IsValueType);
            Assert.Equal(symbol.IsValueType, symbol.GetTypeInfo().IsValueType);
            using var output = new MemoryStream();
            var result = retarget
                ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
                : compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using (var assembly = AssemblyDefinition.ReadAssembly(output))
            {
                var method = assembly.MainModule.GetType("Consumer").Methods.Single(m => m.Name == "Copy");
                Assert.Equal(category == "struct", method.ReturnType.IsValueType);
                Assert.Equal(category == "struct", Assert.Single(method.Parameters).ParameterType.IsValueType);
                Assert.Equal(libraryName, method.ReturnType.Scope.Name);
                Assert.All(method.Body.Variables.Where(v => v.VariableType.FullName.Contains("Payload")),
                    local => Assert.Equal(category == "struct", local.VariableType.IsValueType));
            }
            using var implementation = new MemoryStream();
            var implementationResult = declarations.Emit(implementation);
            Assert.True(implementationResult.Success, string.Join("\n", implementationResult.Diagnostics));
            implementation.Position = 0;
            output.Position = 0;
            var context = new AssemblyLoadContext("CoreIdentity", isCollectible: true);
            try
            {
                context.LoadFromStream(implementation);
                var loaded = context.LoadFromStream(output);
                Assert.Equal(42, loaded.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
            }
            finally
            {
                context.Unload();
            }
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
