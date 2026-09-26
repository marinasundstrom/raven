using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ImportedEmptyUnionCaseTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GenericConsumerMatchesNongenericCompanionCase(bool targetMetadata)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-empty-case", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var reference = Path.Combine(directory, "MaybeLibrary.dll");
        try
        {
            var producer = Compilation.Create("MaybeLibrary", [SyntaxTree.ParseText("""
                public union Maybe<T> {
                    case Item(T)
                    case Empty
                }
                public class Factory {
                    public static func Create() -> Maybe<int> => .Empty
                }
                """)], paths.Select(MetadataReference.CreateFromFile).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(reference))
            {
                var emitted = producer.Emit(stream);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var references = paths.Append(reference).Select(MetadataReference.CreateFromFile).ToArray();
            var consumer = Compilation.Create("MaybeConsumer", [SyntaxTree.ParseText("""
                public class Consumer {
                    public static func IsEmpty<T>(value: Maybe<T>) -> bool => value is .Empty
                    public static func Run() -> bool => IsEmpty(Factory.Create())
                }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(consumer.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var result = targetMetadata
                ? consumer.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
                : consumer.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, references);
            Assert.Equal(true, loaded.Assembly.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { Directory.Delete(directory, recursive: true); }
    }
}
