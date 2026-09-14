using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class EmptyArrayMetadataTests
{
    [Theory]
    [InlineData("net10.0", false, false)]
    [InlineData("net10.0", false, true)]
    [InlineData("net10.0", true, false)]
    [InlineData("net10.0", true, true)]
    [InlineData("net11.0", false, false)]
    [InlineData("net11.0", false, true)]
    [InlineData("net11.0", true, false)]
    [InlineData("net11.0", true, true)]
    public void EmptyArraysUseOnlyAvailableTargetMembers(string framework, bool targetMetadata, bool omitFactory)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-empty-array", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(framework));
            var runtime = paths.Single(path => Path.GetFileName(path) == "System.Runtime.dll");
            var modifiedRuntime = Path.Combine(directory, "System.Runtime.dll");
            if (omitFactory)
            {
                using var reference = AssemblyDefinition.ReadAssembly(runtime);
                var array = reference.MainModule.GetType("System.Array");
                var factories = array.Methods.Where(method => method.Name == "Empty").ToArray();
                Assert.NotEmpty(factories);
                foreach (var method in factories)
                    array.Methods.Remove(method);
                reference.Write(modifiedRuntime);
            }
            var references = paths.Select(path => MetadataReference.CreateFromFile(omitFactory && path == runtime ? modifiedRuntime : path)).ToArray();
            var compilation = Compilation.Create("EmptyArrayConsumer", [SyntaxTree.ParseText("""
                import System.Collections.Generic.*
                public class Item {}
                public class Consumer {
                    public static func Ints() -> int[] { return [] }
                    public static func Items() -> Item[] { return [] }
                    public static func Nested() -> string[][] { return [] }
                    public static func Iterable() -> IEnumerable<string> { return [] }
                }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
            var factorySymbols = compilation.GetTypeByMetadataName("System.Array")!.GetMembers("Empty");
            Assert.Equal(omitFactory, factorySymbols.IsEmpty);
            using var output = new MemoryStream();
            var emitted = targetMetadata
                ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(runtime)))
                : compilation.Emit(output);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            output.Position = 0;
            using (var assembly = AssemblyDefinition.ReadAssembly(output))
            {
                var usesFactory = assembly.MainModule.GetMemberReferences().Any(member =>
                    member.DeclaringType.FullName == "System.Array" && member.Name == "Empty");
                Assert.Equal(!omitFactory, usesFactory);
            }
            using var loaded = TestAssemblyLoader.LoadFromStream(output, references);
            var consumer = loaded.Assembly.GetType("Consumer")!;
            foreach (var method in new[] { "Ints", "Items", "Nested", "Iterable" })
            {
                var result = Assert.IsAssignableFrom<Array>(consumer.GetMethod(method)!.Invoke(null, null));
                Assert.Equal(0, result.Length);
            }
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
