using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class DefaultInterfaceLambdaTests
{
    [Theory]
    [InlineData("func Create(offset: int) -> Func<int, int> => value => value + offset", "Create", 42)]
    [InlineData("val Transform: Func<int, int> => value => value + 1", "get_Transform", 3)]
    [InlineData("val Transform: Func<int, int> => value => value + Offset", "get_Transform", 42)]
    public void DefaultBodyLambdaExecutesOnItsClosureCarrier(string member, string accessor, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.*
            interface Contract {
                val Offset: int { get; }
                {{member}}
            }
            class Implementation : Contract {
                val Offset: int => 40
            }
            """);
        var compilation = Compilation.Create("DefaultInterfaceLambda", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var instance = Activator.CreateInstance(assembly.GetType("Implementation")!);
        var method = assembly.GetType("Contract")!.GetMethod(accessor)!;
        var function = Assert.IsAssignableFrom<Delegate>(method.Invoke(instance, accessor == "Create" ? [40] : null));
        Assert.Equal(expected, function.DynamicInvoke(2));
    }
}
