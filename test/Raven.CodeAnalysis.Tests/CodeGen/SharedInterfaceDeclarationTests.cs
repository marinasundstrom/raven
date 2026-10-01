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

    [Fact]
    public void AbstractPropertyAndInheritedInterfaceKeepTheirContracts()
    {
        var tree = SyntaxTree.ParseText("""
            public interface Disposable { func Dispose() }
            public interface Iterator<T> : Disposable {
                func MoveNext() -> bool
                val Current: T { get }
            }
            """);
        var compilation = Compilation.Create("IteratorContract", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>().Last();
        var type = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceInterfacePlan.TryCreate(type, ReflectionEmitCapabilities.Shared, out var plan));
        Assert.Single(plan!.Properties);
        Assert.Single(plan.BaseInterfaces);
        using var image = new MemoryStream(); var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var emitted = Assembly.Load(image.ToArray()).GetType("Iterator`1")!.MakeGenericType(typeof(int));
        Assert.Equal("Disposable", emitted.GetInterfaces().Single().Name);
        Assert.Equal(typeof(int), emitted.GetProperty("Current")!.PropertyType);
        Assert.True(emitted.GetProperty("Current")!.GetMethod!.IsAbstract);
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void InterfaceSignaturesDefaultsAndStorageUseSharedReferenceTypes(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public interface Iterator<T> { val Current: T { get } }
            public interface Iterable<T> { func GetIterator() -> Iterator<T> }
            public static class Flow {
                static func Empty() -> Iterator<int>? => default(Iterator<int>)
                static func Echo(value: Iterator<int>?) -> Iterator<int>? => value
                static func Roundtrip() -> Iterator<int>? {
                    let values: Iterator<int>?[] = [Empty()]
                    let value = Echo(values[0])
                    values[0] = value
                    return values[0]
                }
            }
            """);
        var compilation = Compilation.Create("InterfaceValues", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Where(m => m.Body is not null || m.ExpressionBody is not null))
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), $"{method.Name}: {failure?.Detail}: {failure?.Syntax}");
        }
        var iterator = (INamedTypeSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>().First())!;
        Assert.True(CallableSignature.TryType(iterator, false, out var valueType));
        var noInterfaces = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), allowsGenericInterfaceDeclarations: true, allowsRootClassSignatures: true);
        Assert.False(noInterfaces.Allows(valueType));
        using var image = new MemoryStream(); var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var loaded = Assembly.Load(image.ToArray());
        Assert.Null(loaded.GetType("Flow")!.GetMethod("Roundtrip")!.Invoke(null, null));
        Assert.Equal(loaded.GetType("Iterator`1")!.MakeGenericType(typeof(int)),
            loaded.GetType("Iterable`1")!.MakeGenericType(typeof(int)).GetMethod("GetIterator")!.ReturnType);
    }

    [Theory]
    [InlineData("public interface Contract<out T> { func Get() -> T }")]
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
