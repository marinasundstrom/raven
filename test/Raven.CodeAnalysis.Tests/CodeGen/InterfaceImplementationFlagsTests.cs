using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class InterfaceImplementationFlagsTests
{
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
