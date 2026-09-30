using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedLinearBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void StaticArithmeticAndCallsExecuteOnBothGeneratorPaths(OptimizationLevel optimization)
    {
        const string source = """
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    return Twice(value - 1) + 4
                }
                public static func Twice(value: int) -> int {
                    return value * 2
                }
            }
            """;
        var compilation = Create(source, optimization);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!,
            (IBlockOperation)model.GetOperation(method.Body!)!, _ => false, out var lowered, out var failure));
        Assert.NotNull(lowered);
        Assert.Null(failure);
        var assembly = Emit(compilation);
        Assert.Equal(42, assembly.GetType("Arithmetic")!.GetMethod("Value")!.Invoke(null, [20]));
        Assert.Equal(unchecked(int.MaxValue * 2), assembly.GetType("Arithmetic")!.GetMethod("Twice")!.Invoke(null, [int.MaxValue]));
    }

    [Fact]
    public void UnsupportedBodyIsRejectedBeforeBuildingAndUsesGeneralDotNetGenerator()
    {
        const string source = """
            public static class Arithmetic {
                public static func Divide(value: int) -> int {
                    return (value + 2) / 2
                }
            }
            """;
        var compilation = Create(source, OptimizationLevel.Release);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.False(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!,
            (IBlockOperation)model.GetOperation(method.Body!)!, _ => false, out var lowered, out var failure));
        Assert.Null(lowered);
        Assert.NotNull(failure);
        Assert.Equal("(value + 2) / 2", failure.Syntax.ToString());
        Assert.Equal(42, Emit(compilation).GetType("Arithmetic")!.GetMethod("Divide")!.Invoke(null, [82]));
    }

    private static Compilation Create(string source, OptimizationLevel optimization)
        => Compilation.Create("SharedBody" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));

    private static Assembly Emit(Compilation compilation)
    {
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        return Assembly.Load(output.ToArray());
    }
}
