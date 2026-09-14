using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class VoidInvocationEmissionTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ReturningVoidInvocationDoesNotDiscardMissingValue(bool targetMetadata)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var references = paths.Select(MetadataReference.CreateFromFile).ToArray();
        var compilation = Compilation.Create("VoidConsumer", [SyntaxTree.ParseText("""
            public class Consumer {
                public static func Run() {
                    return System.GC.KeepAlive("alive")
                }
            }
            """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var output = new MemoryStream();
        var emitted = targetMetadata
            ? compilation.Emit(output, null, new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"))))
            : compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, references);
        Assert.Null(loaded.Assembly.GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
    }
}
