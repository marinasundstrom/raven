using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class GenericInterfaceImplementationTests
{
    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void GenericOwnerImplementsInheritedConstructedInterface(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public interface Root<T> { func Echo(value: T) -> T }
            public interface Middle<T> : Root<T> { }
            public open class Echoer<Unused, T> : Middle<T> {
                func Echo(value: T) -> T => value
            }
            func Main() -> int {
                let contract: Root<int> = Echoer<bool, int>()
                return contract.Echo(42)
            }
            """);
        var compilation = Compilation.Create("GenericInterface" + optimization, [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var symbol = compilation.GetTypeByMetadataName("Echoer`2")!;
        EmissionCapabilities Profile(bool enabled) => new(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public], [Accessibility.Public],
            allowsGenericClassOwners: true, allowsGenericInterfaceDeclarations: true, allowsInterfaceSignatures: true,
            allowsConstructedInterfaceImplementations: enabled);
        Assert.False(SourceTypePlan.TryCreate(symbol, out _, Profile(false)));
        Assert.True(SourceTypePlan.TryCreate(symbol, out _, Profile(true)));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var loaded = Assembly.Load(image.ToArray());
        Assert.Equal(42, loaded.EntryPoint!.Invoke(null, null));
        var owner = loaded.GetType("Echoer`2")!.MakeGenericType(typeof(bool), typeof(int));
        Assert.False(owner.IsSealed);
        Assert.All(owner.GetInterfaces(), i => Assert.Equal(typeof(int), Assert.Single(i.GenericTypeArguments)));
    }
    [Fact]
    public void RecursiveInterfaceArgumentDoesNotRecurseThroughOwnerAdmission()
    {
        var tree = SyntaxTree.ParseText("""
            public interface EchoContract<T> { func Echo(value: T) -> T }
            public class Box<T> : EchoContract<Box<T>> {
                func Echo(value: Box<T>) -> Box<T> => value
            }
            func Main() -> int {
                let box = Box<int>()
                let contract: EchoContract<Box<int>> = box
                if contract.Echo(box) == box { return 42 }
                return 1
            }
            """);
        var compilation = Compilation.Create("RecursiveInterface", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(SourceTypePlan.TryCreate(compilation.GetTypeByMetadataName("Box`1")!, out _, ReflectionEmitCapabilities.Shared));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

}
