using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetMetadataEmissionTests
{
    [Fact]
    public void RetargetedEmissionConstructsReferenceOnlyGenericDelegate()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-delegate", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetCallbacks.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetCallbacks",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public delegate T Callback<T>(); }")],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("CallbackConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Number() -> int { return 42 }
                func Make() -> Callback<int> { return Number }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, metadataImportOptions: new MetadataImportOptions("System.Runtime")));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            var make = assembly.MainModule.Types.SelectMany(t => t.Methods).Single(m => m.Name == "Make");
            Assert.Equal("Contracts.Callback`1<System.Int32>", make.ReturnType.FullName);
            Assert.Equal("TargetCallbacks", make.ReturnType.Scope.Name);
            var constructor = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
                .Where(m => m.Name == ".ctor" && m.DeclaringType.FullName == make.ReturnType.FullName));
            Assert.Equal("TargetCallbacks", constructor.DeclaringType.Scope.Name);
            Assert.Equal(new[] { "System.Object", "System.IntPtr" }, constructor.Parameters.Select(p => p.ParameterType.FullName));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }

    [Fact]
    public void RetargetedEmissionPreservesReferenceOnlyGenericTypesAndMembers()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-metadata", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetContracts.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetContracts",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                [assembly: System.Runtime.CompilerServices.ReferenceAssembly]
                namespace System.Runtime.CompilerServices { public sealed class UnionAttribute : System.Attribute { } }
                namespace Contracts {
                    public static class Cases {
                        public struct Item<T> { public Item(T value) { Value = value; } public T Value { get; } }
                    }
                    [System.Runtime.CompilerServices.Union]
                    public struct Container<T> {
                        public Container(Cases.Item<T> item) { }
                        public object Value => default;
                        public bool TryGetValue(out Cases.Item<T> item) { item = default; return false; }
                    }
                }
                """)],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using (var stream = File.Create(path))
        {
            var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        }

        var source = """
            import Contracts.*
            func Echo(value: Container<int>) -> Container<int> {
                let copy = value
                return copy
            }
            func Wrap(value: Cases.Item<int>) -> Container<int> { return Container<int>(value) }
            func Make(value: int) -> Cases.Item<int> { return Cases.Item<int>(value) }
            func Read(value: Cases.Item<int>) -> int { return value.Value }
            func ReadContainer(value: Container<int>) -> int {
                if value is Cases.Item<int> item { return item.Value }
                return 0
            }
            """;
        var compilation = Compilation.Create("TargetConsumer", [SyntaxTree.ParseText(source)],
            paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                metadataImportOptions: new MetadataImportOptions("System.Runtime")));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))));
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var assembly = AssemblyDefinition.ReadAssembly(output);
        Assert.DoesNotContain(assembly.MainModule.Types, t => t.Name.StartsWith("<RavenMetadata"));
        var methods = assembly.MainModule.Types.SelectMany(t => t.Methods).ToArray();
        var echo = methods.Single(m => m.Name == "Echo");
        var carrier = Assert.IsType<GenericInstanceType>(echo.ReturnType);
        Assert.Equal("Contracts.Container`1", carrier.ElementType.FullName);
        Assert.Equal("TargetContracts", carrier.Scope.Name);
        Assert.Equal("System.Int32", Assert.Single(carrier.GenericArguments).FullName);
        Assert.Equal(carrier.FullName, Assert.Single(echo.Parameters).ParameterType.FullName);
        Assert.Contains(echo.Body.Variables, v => v.VariableType.FullName == carrier.FullName);
        var constructor = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
            .Where(m => m.Name == ".ctor" && m.DeclaringType.FullName == "Contracts.Cases/Item`1<System.Int32>"));
        Assert.True(constructor.DeclaringType.IsValueType);
        Assert.Equal(0, Assert.IsType<GenericParameter>(Assert.Single(constructor.Parameters).ParameterType).Position);
        var wrapper = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
            .Where(m => m.Name == ".ctor" && m.DeclaringType.FullName == "Contracts.Container`1<System.Int32>"));
        var wrappedCase = Assert.IsType<GenericInstanceType>(Assert.Single(wrapper.Parameters).ParameterType);
        Assert.Equal("Contracts.Cases/Item`1", wrappedCase.ElementType.FullName);
        Assert.Equal(0, Assert.IsType<GenericParameter>(Assert.Single(wrappedCase.GenericArguments)).Position);
        var getter = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>().Where(m => m.Name == "get_Value"));
        Assert.Equal("Contracts.Cases/Item`1<System.Int32>", getter.DeclaringType.FullName);
        Assert.True(getter.DeclaringType.IsValueType);
        Assert.Equal("TargetContracts", getter.DeclaringType.Scope.Name);
        Assert.Equal(0, Assert.IsType<GenericParameter>(getter.ReturnType).Position);
        var extractor = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>().Where(m => m.Name == "TryGetValue"));
        var outputCase = Assert.IsType<GenericInstanceType>(Assert.IsType<ByReferenceType>(Assert.Single(extractor.Parameters).ParameterType).ElementType);
        Assert.Equal("Contracts.Cases/Item`1", outputCase.ElementType.FullName);
        Assert.True(outputCase.IsValueType);
        Assert.Equal(0, Assert.IsType<GenericParameter>(Assert.Single(outputCase.GenericArguments)).Position);
    }
}
