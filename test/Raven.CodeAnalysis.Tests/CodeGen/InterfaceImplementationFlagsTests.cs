using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class InterfaceImplementationFlagsTests
{
    [Fact]
    public void NullableReferenceAnnotations_PreserveInterfaceDispatch()
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("NullableInterfaceDispatch", [SyntaxTree.ParseText("""
            interface Boundary { func Echo(value: string) -> string? }
            class Echoer : Boundary { func Echo(value: string?) -> string => "ok" }
            """)], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)).AddReferences(references);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(new MemoryStream(output.ToArray()), references);
        var boundary = loaded.Assembly.GetType("Boundary", true)!;
        var type = loaded.Assembly.GetType("Echoer", true)!;
        var instance = Activator.CreateInstance(type);
        Assert.Equal("ok", boundary.GetMethod("Echo")!.Invoke(instance, ["input"]));
        Assert.True(type.GetMethod("Echo")!.IsVirtual);
    }

    [Fact]
    public void NullableValues_DoNotMatchNonNullableInterfaceParameters()
    {
        var compilation = Compilation.Create("NullableValueInterface", [SyntaxTree.ParseText("""
            interface Boundary { func Accept(value: int) -> bool }
            class Consumer : Boundary { func Accept(value: int?) -> bool => true }
            """)], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)).AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void AbstractAndVirtualInterfaceImplementationsPreserveDispatchFlags()
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var compilation = Compilation.Create("AbstractConsumer", [SyntaxTree.ParseText("""
            interface Reader { func Read() -> int }
            abstract class Base : Reader { abstract func Read() -> int }
            class Derived : Base { override func Read() -> int { return 42 } }
            open class VirtualReader : Reader { virtual func Read() -> int { return 7 } }
            """)], paths.Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var assembly = AssemblyDefinition.ReadAssembly(output);
        var abstractMethod = assembly.MainModule.GetType("Base").Methods.Single(m => m.Name == "Read");
        Assert.True(abstractMethod.IsAbstract);
        Assert.True(abstractMethod.IsVirtual);
        Assert.False(abstractMethod.IsFinal);
        Assert.False(abstractMethod.HasBody);
        var virtualMethod = assembly.MainModule.GetType("VirtualReader").Methods.Single(m => m.Name == "Read");
        Assert.True(virtualMethod.IsVirtual);
        Assert.True(virtualMethod.IsNewSlot);
        Assert.False(virtualMethod.IsFinal);
        var implementation = assembly.MainModule.GetType("Derived").Methods.Single(m => m.Name == "Read");
        Assert.True(implementation.IsVirtual);
        Assert.False(implementation.IsNewSlot);
    }

}
