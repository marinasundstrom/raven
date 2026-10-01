using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedInterfaceDeclarationTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void InterfaceContractPlanMatchesCliMetadata(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            namespace Example
            public interface Comparer<T> {
                func Compare(left: T, right: T) -> int
            }
            """);
        var compilation = Compilation.Create("InterfacePlan", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceInterfacePlan.TryCreate(type, ReflectionEmitCapabilities.Shared, out var plan));
        Assert.Single(plan!.Methods);
        Assert.Equal("Example", plan.Namespace);
        Assert.Equal("Comparer`1", plan.Name);
        var noInterfaces = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>());
        Assert.False(SourceInterfacePlan.TryCreate(type, noInterfaces, out _));
        using var image = new MemoryStream(); var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var emitted = Assembly.Load(image.ToArray()).GetType("Example.Comparer`1")!;
        Assert.True(emitted.IsInterface);
        var method = emitted.MakeGenericType(typeof(int)).GetMethod("Compare")!;
        Assert.True(method.IsAbstract);
        Assert.True(method.IsVirtual);
        Assert.Null(method.GetMethodBody());
        Assert.All(method.GetParameters(), p => Assert.Equal(typeof(int), p.ParameterType));
    }

    [Theory]
    [InlineData("public interface Contract<out T> { func Get() -> T }")]
    [InlineData("public interface Base { }\npublic interface Contract : Base { }")]
    [InlineData("public interface Contract { func Get<T>(value: T) -> T }")]
    public void BroaderInterfaceShapesRemainOutsideTheBoundedPlan(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("OtherInterfaces", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>().Last();
        var type = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.False(SourceInterfacePlan.TryCreate(type, ReflectionEmitCapabilities.Shared, out _));
    }
}
