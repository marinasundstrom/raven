using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class SharedInterfaceDispatchTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void InheritedInterfaceDispatchUsesSharedCallPlan(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public interface Value { func Get(offset: int) -> int }
            public interface Derived : Value { }
            public class First : Derived { func Get(offset: int) -> int => 19 + offset }
            public class Second : Derived { func Get(offset: int) -> int => 23 + offset }
            func Apply(value: Value) -> int => value.Get(0)
            func Main() -> int {
                let values: Value[] = [First(), Second()]
                return Apply(values[0]) + Apply(values[1])
            }
            """);
        var compilation = Compilation.Create("InterfaceDispatch", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>())
        {
            var symbol = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        var first = (INamedTypeSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First())!;
        Assert.True(SourceTypePlan.TryCreate(first, out _, ReflectionEmitCapabilities.Shared));
        var disabled = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            [EmissionDeclarationKind.RootClass], [Accessibility.Public], [Accessibility.Public], allowsRootClassSignatures: true);
        Assert.False(SourceTypePlan.TryCreate(first, out _, disabled));
        using var image = new MemoryStream(); var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }
}
