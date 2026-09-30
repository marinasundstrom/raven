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

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void UnitFunctionsAndClassHelpersKeepVoidSignatures(OptimizationLevel optimization)
    {
        const string source = """
            func Main() {
                Greet()
            }
            func Greet() {
                Helpers.Finish(42)
                return
            }
            public static class Helpers {
                public static func Finish(value: int) {
                }
            }
            """;
        var compilation = Compilation.Create("SharedUnit" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        foreach (var declaration in compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            var symbol = (IMethodSymbol)model.GetDeclaredSymbol(declaration)!;
            Assert.True(LinearMethodBody.TryLower(symbol, (IBlockOperation)model.GetOperation(declaration.Body!)!,
                _ => false, out _, out var failure), failure?.Detail);
        }
        var assembly = Emit(compilation);
        Assert.Equal(typeof(void), assembly.EntryPoint!.ReturnType);
        Assert.Null(assembly.EntryPoint.Invoke(null, null));
        Assert.Equal(typeof(void), assembly.GetType("Helpers")!.GetMethod("Finish")!.ReturnType);
    }

    [Fact]
    public void Int32AssemblyFunctionsCallHelpersThroughTheSharedPath()
    {
        const string source = """
            func Main() -> int {
                return Twice(20) + 2
            }
            func Twice(value: int) -> int {
                return value * 2
            }
            """;
        var compilation = Compilation.Create("SharedFunctions", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
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
