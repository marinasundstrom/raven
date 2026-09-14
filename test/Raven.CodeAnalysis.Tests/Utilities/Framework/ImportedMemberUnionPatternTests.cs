using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedMemberUnionPatternTests
{
    [Theory]
    [InlineData(".Ok(let number)", ".Error(_)", true, true)]
    [InlineData(".Ok(let number)", ".Error(_)", false, true)]
    [InlineData("Ok(let number)", "Error(_)", true, true)]
    [InlineData("Ok(let number)", "Error(_)", false, false)]
    [InlineData("Choice.Ok<int>(let number)", "Choice.Error<string>(_)", false, true)]
    public void ImportedMemberUnionDestructuresThroughItsContract(
        string success, string failure, bool importCases, bool valid)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-member-pattern", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "MemberContracts.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("MemberContracts",
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
                {{(importCases ? "import Contracts.Choice.*" : "")}}
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
                        return Pick(choice) + Pick(choice)
                    }
                }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (!valid)
            {
                Assert.Contains(errors, diagnostic => diagnostic.Id == "RAV2102");
                return;
            }
            Assert.Empty(errors);
            using var output = new MemoryStream();
            var result = compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
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
}
