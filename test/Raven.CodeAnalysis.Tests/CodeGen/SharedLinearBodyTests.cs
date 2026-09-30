using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
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
            model, method.Body!, _ => false, out var lowered, out var failure));
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
            model, method.Body!, _ => false, out var lowered, out var failure));
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
                Helpers.Finish(value: 42)
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
            Assert.True(LinearMethodBody.TryLower(symbol, model, declaration.Body!,
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

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ImplicitValueReturnUsesCompilerLowering(OptimizationLevel optimization)
    {
        const string source = """
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    (value + 1) * 2
                }
            }
            """;
        var compilation = Create(source, optimization);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!,
            model, method.Body!, _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Arithmetic")!.GetMethod("Value")!.Invoke(null, [20]));
    }

    [Fact]
    public void ForwardCallsKeepOverloadsAndOwnersDistinctAcrossEmissions()
    {
        const string source = """
            func Main() -> int {
                Value(5) + Value(5) + Alpha.Value(10) + Beta.Value(10) + Alpha.Value()
            }
            func Value(value: int) -> int {
                value
            }
            public static class Alpha {
                public static func Value(value: int) -> int {
                    value + 1
                }
                public static func Value() -> int {
                    9
                }
            }
            public static class Beta {
                public static func Value(value: int) -> int {
                    value + 2
                }
            }
            """;
        var compilation = Compilation.Create("CallableIdentities", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void InitializedLocalsAndAssignmentExecute(OptimizationLevel optimization)
    {
        const string source = """
            public static class Arithmetic {
                public static func Value(value: int) -> int {
                    let start = value
                    var result = start * 2
                    result = result + 2
                    result
                }
            }
            """;
        var compilation = Create(source, optimization);
        var method = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(method.SyntaxTree);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure), failure?.Detail);
        Assert.Equal(42, Emit(compilation).GetType("Arithmetic")!.GetMethod("Value")!.Invoke(null, [20]));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void BranchesAndLoopUseLoweredControlFlow(OptimizationLevel optimization)
    {
        const string source = """
            func Main() -> int {
                Accumulate(6)
            }
            func Accumulate(limit: int) -> int {
                var index = 0
                var result = 0
                while index < limit {
                    if index < 3 {
                        result = result + 5
                    } else {
                        result = result + 9
                    }
                    index = index + 1
                }
                return result
            }
            """;
        var compilation = Compilation.Create("Flow" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model, syntax.Body!,
                _ => false, out _, out var failure), failure?.Detail);
        }
        Assert.Equal(42, Emit(compilation).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void NegatedComparisonsAndLoopExitsExecute(OptimizationLevel optimization)
    {
        const string source = """
            func Main() -> int {
                var index = 0
                var result = 0
                while true {
                    index = index + 1
                    if index == 3 {
                        continue
                    }
                    if index >= 7 {
                        break
                    }
                    if !(index != 6) {
                        result = result + 6
                    } else {
                        if index <= 5 {
                            result = result + index
                        }
                    }
                }
                return result * 2 + 6
            }
            """;
        var compilation = Compilation.Create("LoopExits" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        var tree = compilation.SyntaxTrees[0]; var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model, syntax.Body!,
            _ => false, out _, out var failure), failure?.Detail);
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
