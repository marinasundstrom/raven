using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedGenericBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void GenericBodiesSharePlanningAndForwardExactTypes(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            func Identity<T>(value: T) -> T {
                let copy = value
                return copy
            }
            func Forward<U>(value: U) -> U => Identity<U>(value)
            func Choose<T>(flag: bool, first: T, second: T) -> T => if flag { first } else { second }
            class Helpers {
                static func First<T>(values: T[]) -> T => values[0]
            }
            func Main() -> int {
                let values: int[] = [42]
                if Forward<long>(5000000000L) != 5000000000L { return 1 }
                return Choose(false, 1, Forward<int>(Helpers.First<int>(values)))
            }
            """);
        var compilation = Compilation.Create("SharedGenerics", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var declarations = tree.GetRoot().DescendantNodes().Where(n => n is FunctionStatementSyntax or MethodDeclarationSyntax);
        foreach (var syntax in declarations)
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            if (method.IsGenericMethod)
            {
                var denied = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                    Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
                    [Accessibility.Public], [Accessibility.Internal], allowsArrays: true);
                Assert.False(plan.IsSupportedBy(denied));
            }
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void StaticGenericOwnersKeepTypeAndMethodScopesIndependent(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            static class Helpers<Element> {
                static func SelectValue<Result>(ignored: Element, value: Result) -> Result => value
                static func Forward<Other>(ignored: Element, value: Other) -> Other => SelectValue<Other>(ignored, value)
                static func Empty() -> Element => default(Element)
                static func First(values: Element[]) -> Element => values[0]
            }
            func Main() -> int {
                let values: int[] = [42]
                if Helpers<int>.Forward<long>(1, 5000000000L) != 5000000000L { return 1 }
                if Helpers<int>.Empty() != 0 { return 2 }
                return Helpers<int>.First(values)
            }
            """);
        var compilation = Compilation.Create("GenericOwners", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            var noOwners = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
                [Accessibility.Public], [Accessibility.Internal], allowsArrays: true, allowsGenericMethods: true);
            Assert.False(plan.IsSupportedBy(noOwners));
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

    [Fact]
    public void GenericArgumentsRequireCapabilitiesEvenWhenAbsentFromSignature()
    {
        var tree = SyntaxTree.ParseText("""
            func Marker<T>() -> int => 42
            func Main() -> int => Marker<int[]>()
            """);
        var compilation = Compilation.Create("GenericCapabilities", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Last();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var noArrays = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
            [Accessibility.Public], [Accessibility.Internal], allowsGenericMethods: true);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, noArrays));
        Assert.False(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, noArrays));
        Assert.Contains("call signature", failure!.Detail);
    }
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void GenericInstanceBodiesShareReceiverAndParameterPlanning(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Receiver {
                private var number: int = 0
                func Remember<T>(value: T, next: int) -> T {
                    number = next
                    let copy = value
                    return copy
                }
                func Forward<U>(value: U, next: int) -> U => Remember<U>(value, next)
                val Number: int => number
            }
            func Main() -> int {
                let receiver = Receiver()
                let alias = receiver.Forward(receiver, 42)
                return alias.Number
            }
            """);
        var compilation = Compilation.Create("GenericReceivers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            var staticGenericsOnly = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
                [Accessibility.Public], [Accessibility.Internal], allowsGenericMethods: true);
            Assert.False(plan.IsSupportedBy(staticGenericsOnly));
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }
}
