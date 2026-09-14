using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedUnionEmissionTests
{
    [Theory]
    [InlineData(false, ".Ok(let number)", ".Error(_)")]
    [InlineData(true, ".Ok(let number)", ".Error(_)")]
    [InlineData(false, "Ok(let number)", "Error(_)")]
    [InlineData(true, "Ok(let number)", "Error(_)")]
    [InlineData(false, "Choice.Ok<int>(let number)", "Choice.Error<string>(_)")]
    [InlineData(true, "Choice.Ok<int>(let number)", "Choice.Error<string>(_)")]
    public void ImportedMemberUnionDestructuresThroughItsContract(bool targetMetadata, string success, string failure)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-member-pattern", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var libraryName = $"MemberContracts_{Guid.NewGuid():N}";
        var path = Path.Combine(directory, libraryName + ".dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create(libraryName,
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                namespace Contracts {
                    public static class Choice {
                        public struct Ok<T> {
                            public T Value;
                            public Ok(T value) { Value = value; }
                            public void Deconstruct(out T value) { value = Value; Value = default; }
                        }
                        public struct Error<E> {
                            public E Value;
                            public Error(E value) { Value = value; }
                            public void Deconstruct(out E value) { value = Value; }
                        }
                    }
                    [System.Runtime.CompilerServices.Union]
                    public struct Choice<T,E> {
                        private object value;
                        public object Value => value;
                        public bool HasValue(int marker) => value != null && marker == 42;
                        public void Touch(ref int marker) { marker++; }
                        public Choice(Choice.Ok<T> value) { this.value = value; }
                        public Choice(Choice.Error<E> value) { this.value = value; }
                        public bool TryGetValue(out Choice.Ok<T> result) {
                            if (value is Choice.Ok<T> found) { result = found; return true; }
                            result = default; return false;
                        }
                        public bool TryGetValue(out Choice.Error<E> result) {
                            if (value is Choice.Error<E> found) { result = found; return true; }
                            result = default; return false;
                        }
                    }
                }
                """)], paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var references = paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray();
            var compilation = Compilation.Create("MemberConsumer", [SyntaxTree.ParseText($$"""
                import Contracts.*
                import Contracts.Choice.*
                public class Consumer {
                    public static func Pick(value: Choice<int, string>) -> int {
                        return match value {
                            {{success}} => number
                            {{failure}} => -1
                        }
                    }
                    public static func Run(value: int) -> int {
                        if value < 0 {
                            return Pick(Choice<int, string>(Choice.Error<string>("failed")))
                        }
                        let choice = Choice<int, string>(Choice.Ok<int>(value))
                        var marker = 41
                        choice.Touch(ref marker)
                        if choice.HasValue(marker) == false {
                            return -100
                        }
                        return Pick(choice) + Pick(choice)
                    }
                }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = targetMetadata
                ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
                : compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using (var assembly = AssemblyDefinition.ReadAssembly(output))
            {
                Assert.DoesNotContain(assembly.MainModule.Types, type => type.Name.StartsWith("<RavenMetadata"));
                var methods = assembly.MainModule.GetMemberReferences().OfType<MethodReference>().ToArray();
                Assert.All(methods.Where(method => method.Name == "TryGetValue" || method.Name == "HasValue"),
                    method => Assert.Equal(MetadataType.Boolean, method.ReturnType.MetadataType));
                Assert.All(methods.Where(method => method.Name == "Deconstruct" || method.Name == "Touch"),
                    method => Assert.Equal(MetadataType.Void, method.ReturnType.MetadataType));
                var touch = Assert.Single(methods.Where(method => method.Name == "Touch"));
                var parameter = Assert.IsType<ByReferenceType>(Assert.Single(touch.Parameters).ParameterType);
                Assert.Equal(MetadataType.Int32, parameter.ElementType.MetadataType);
            }
            using var loaded = TestAssemblyLoader.LoadFromStream(output, references);
            var run = loaded.Assembly.GetType("Consumer")!.GetMethod("Run")!;
            Assert.Equal(84, run.Invoke(null, [42]));
            Assert.Equal(-1, run.Invoke(null, [-1]));
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ImportedUnionCasePatternKeepsClosedCarrierLocals(bool targetMetadata)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-pattern", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var libraryName = $"UnionContract_{Guid.NewGuid():N}";
        var libraryPath = Path.Combine(directory, libraryName + ".dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        try
        {
            var library = Compilation.Create(libraryName, [SyntaxTree.ParseText("""
                public union Outcome<T, E> {
                    case Ok(value: T)
                    case Error(error: E)
                }
                public class Factory {
                    public static func Create(value: int) -> Outcome<int, string> {
                        if value < 0 {
                            return .Error("failed")
                        }
                        return .Ok(value)
                    }
                }
                """)], paths.Select(MetadataReference.CreateFromFile).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(libraryPath))
            {
                var emitted = library.Emit(stream);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("Consumer", [SyntaxTree.ParseText("""
                public class Consumer {
                    public static func Run(value: int) -> int {
                        let result = Factory.Create(value)
                        return Pick(result)
                    }
                    public static func Pick(value: Outcome<int, string>) -> int {
                        return match value {
                            .Ok(let number) => number
                            .Error(_) => -1
                        }
                    }
                }
                """)], paths.Append(libraryPath).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = targetMetadata
                ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
                : compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using var assembly = AssemblyDefinition.ReadAssembly(output);
            var caseGetter = Assert.Single(assembly.MainModule.GetMemberReferences().OfType<MethodReference>()
                .Where(method => method.Name == "get_Error"));
            Assert.Equal("Outcome/Error`1<System.String>", caseGetter.DeclaringType.FullName);
            var locals = assembly.MainModule.GetType("Consumer").Methods.Single(m => m.Name == "Pick").Body.Variables;
            Assert.Contains(locals, v => v.VariableType.FullName == "Outcome`2<System.Int32,System.String>");
            Assert.DoesNotContain(locals, v => v.VariableType.FullName.Contains("RavenMetadata"));
            using var loaded = TestAssemblyLoader.LoadFromStream(output,
                paths.Append(libraryPath).Select(MetadataReference.CreateFromFile).ToArray());
            var run = loaded.Assembly.GetType("Consumer")!.GetMethod("Run")!;
            Assert.Equal(42, run.Invoke(null, [42]));
            Assert.Equal(-1, run.Invoke(null, [-1]));
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
