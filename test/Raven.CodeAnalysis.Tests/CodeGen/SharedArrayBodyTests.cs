using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedArrayBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ArrayIterationSharesLoweringAndHonorsNestedControlFlow(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public static class Iteration {
                public static func Run() -> int {
                    let items: int[] = [1, 2, 3]
                    let empty: int[] = []
                    var total = 0
                    outer: for item in items {
                        for inner in items {
                            if inner == 2 { continue outer }
                            total = total + item
                        }
                    }
                    for item in empty { total = 100 }
                    for item in items {
                        if item == 2 { break }
                        total = total + item
                    }
                    var index = 0
                    while index < 2 {
                        index = index + 1
                        for item in items {
                            if item == 2 { break }
                            total = total + item
                        }
                    }
                    return total
                }
            }
            """);
        var compilation = Compilation.Create("ArrayIteration", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(9, Assembly.Load(image.ToArray()).GetType("Iteration")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void RetainedEnumeratorLoopOwnsBreakInsideLoweredArrayLoop(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public static class MixedLoops {
                public static func Run() -> int {
                    let outer: int[] = [1, 2, 3]
                    let inner: List<int> = [1, 2, 3]
                    var total = 0
                    for item in outer {
                        for value in inner {
                            if value == 2 { continue }
                            total = total + item
                            if value == 3 { break }
                        }
                    }
                    return total
                }
            }
            """);
        var compilation = Compilation.Create("MixedArrayIteration", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(12, Assembly.Load(image.ToArray()).GetType("MixedLoops")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ArraysPreserveStorageIdentityAndBounds(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public class Item {
                var Number: int
                init(number: int) { Number = number }
            }
            class Holder {
                var Items: Item[]
                init(items: Item[]) { Items = items }
            }
            public static class Arrays {
                public static func Identity(items: Item[]) -> Item[] => items
                public static func Read(items: int[], index: int) -> int => items[index]
                public static func Run() -> int {
                    let items: Item[] = [Item(1), Item(2)]
                    let holder = Holder(Identity(items))
                    let alias = holder.Items
                    alias[0].Number = 40
                    let empty: int[] = []
                    return items[0].Number + alias.Length + empty.Length
                }
            }
            """);
        var compilation = Compilation.Create("SharedArrays", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            var symbol = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Arrays")!;
        Assert.Equal(42, type.GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(42, type.GetMethod("Read")!.Invoke(null, [new[] { 42 }, 0]));
        Assert.IsType<IndexOutOfRangeException>(Assert.Throws<TargetInvocationException>(() => type.GetMethod("Read")!.Invoke(null, [Array.Empty<int>(), 0])).InnerException);
    }

    [Theory]
    [InlineData("public static func Read(items: int[]) -> int { items[0] }", false)]
    [InlineData("public static func Read() -> int { let items: int[] = [42]; return items[0] }", false)]
    [InlineData("public static func Read(items: int[2]) -> int { 42 }", true)]
    [InlineData("public static func Read(items: int[][]) -> int { 42 }", true)]
    [InlineData("public static func Read(items: int[]) -> int { let copy: int[] = [...items]; return copy[0] }", true)]
    public void ArrayCapabilitiesAndUnsupportedShapesRejectAPlan(string method, bool arrays)
    {
        var tree = SyntaxTree.ParseText("public static class C { " + method + " }");
        var compilation = Compilation.Create("ArrayAdmission", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var profile = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public], [Accessibility.Public], allowsArrays: arrays);
        if (SourceCallablePlan.TryCreate(symbol, out var plan, profile))
        {
            Assert.False(plan!.TryLowerBody(compilation, _ => false, out var body, out var failure, profile));
            Assert.Null(body);
            Assert.NotNull(failure);
        }
    }
}
