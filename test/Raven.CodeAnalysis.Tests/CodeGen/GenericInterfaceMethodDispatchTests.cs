using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class GenericInterfaceMethodDispatchTests
{
    [Theory]
    [InlineData(false, "T")]
    [InlineData(false, "U")]
    [InlineData(true, "T")]
    [InlineData(true, "U")]
    public void MethodTypeParametersMatchByPositionAcrossInterfaceOwners(bool inherited, string parameterName)
    {
        var source = $$"""
            public class Box<T>(val Value: T) { }
            public interface Root {
                func Read<T>(value: Box<T>) -> T
                func Read(value: int) -> int
            }
            public interface Derived : Root { }
            public class Reader : {{(inherited ? "Derived" : "Root")}} {
                func Read<{{parameterName}}>(value: Box<{{parameterName}}>) -> {{parameterName}} => value.Value
                func Read(value: int) -> int => value + 1
            }
            func Main() -> int {
                let reader: Root = Reader()
                return reader.Read<int>(Box<int>(40)) + reader.Read(1)
            }
            """;
        var compilation = Compilation.Create("GenericMethodDispatch" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var map = assembly.GetType("Reader")!.GetInterfaceMap(assembly.GetType("Root")!);
        Assert.Equal(2, map.TargetMethods.Length);
        Assert.Single(map.TargetMethods.Where(method => method.IsGenericMethodDefinition));
    }

    [Fact]
    public void GenericArityDistinguishesOtherwiseIdenticalOverloads()
    {
        var tree = SyntaxTree.ParseText("""
            public interface Contract { func Read<T>() -> int }
            public class Reader : Contract {
                func Read() -> int => 0
                func Read<U>() -> int => 42
            }
            func Main() -> int {
                let reader: Contract = Reader()
                return reader.Read<int>()
            }
            """);
        var compilation = Compilation.Create("GenericArityDispatch", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

}
