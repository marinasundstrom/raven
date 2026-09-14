using System.Reflection;
using System.Runtime.Loader;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class InterfaceMetadataEmissionTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void ImportedGenericInterfacePreservesMethodImplAndDispatch(bool targetMetadata, bool explicitImplementation)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-interface-metadata", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "IterationContracts.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("IterationContracts",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                namespace Contracts {
                    public interface ICursor<T> { T Current { get; } }
                    public interface ISource<T> { ICursor<T> GetIterator(); }
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
            var source = """
                import Contracts.*
                public class Numbers : ISource<int> {
                    public func GetIterator() -> ICursor<int> {
                        return Position()
                    }
                }
                public class Position : ICursor<int> {
                    public val Current: int {
                        get => 42
                    }
                }
                public class Consumer {
                    public static func Run() -> int {
                        let source: ISource<int> = Numbers()
                        return source.GetIterator().Current
                    }
                }
                """;
            if (explicitImplementation)
            {
                source = source.Replace("public func GetIterator", "func ISource<int>.GetIterator", StringComparison.Ordinal)
                    .Replace("public val Current", "val ICursor<int>.Current", StringComparison.Ordinal);
            }
            var compilation = Compilation.Create("IterationConsumer", [SyntaxTree.ParseText(source)],
                references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = targetMetadata
                ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
                : compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using (var assembly = AssemblyDefinition.ReadAssembly(output))
            {
                var module = assembly.MainModule;
                Assert.DoesNotContain(module.Types, t => t.Name.StartsWith("<RavenMetadata", StringComparison.Ordinal));
                var implementation = module.Types.Single(t => t.Name == "Numbers").Methods.Single(m => m.Name.EndsWith("GetIterator", StringComparison.Ordinal));
                var declaration = Assert.Single(implementation.Overrides);
                var owner = Assert.IsType<GenericInstanceType>(declaration.DeclaringType);
                Assert.Equal("Contracts.ISource`1<System.Int32>", owner.FullName);
                Assert.Equal("IterationContracts", owner.Scope.Name);
                Assert.Equal(MetadataType.Int32, Assert.Single(owner.GenericArguments).MetadataType);
                Assert.Equal("GetIterator", declaration.Name);
                var returnType = Assert.IsType<GenericInstanceType>(declaration.ReturnType);
                Assert.Equal("Contracts.ICursor`1", returnType.ElementType.FullName);
                Assert.Equal("IterationContracts", returnType.Scope.Name);
                var parameter = Assert.IsType<GenericParameter>(Assert.Single(returnType.GenericArguments));
                Assert.Equal(0, parameter.Position);
                Assert.Equal(GenericParameterType.Type, parameter.Type);
                var getter = module.Types.Single(t => t.Name == "Position").Methods.Single(m => m.Name.EndsWith("get_Current", StringComparison.Ordinal));
                var getterDeclaration = Assert.Single(getter.Overrides);
                Assert.Equal("Contracts.ICursor`1<System.Int32>", getterDeclaration.DeclaringType.FullName);
                Assert.Equal("IterationContracts", getterDeclaration.DeclaringType.Scope.Name);
                Assert.IsType<GenericParameter>(getterDeclaration.ReturnType);
            }

            using var implementationImage = new MemoryStream();
            var implementationResult = declarations.Emit(implementationImage);
            Assert.True(implementationResult.Success, string.Join("\n", implementationResult.Diagnostics));
            implementationImage.Position = 0;
            output.Position = 0;
            var context = new AssemblyLoadContext("InterfaceMetadata", isCollectible: true);
            try
            {
                context.LoadFromStream(implementationImage);
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
