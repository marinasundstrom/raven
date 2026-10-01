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
            class Helpers {
                static func First<T>(values: T[]) -> T => values[0]
            }
            func Main() -> int {
                let values: int[] = [42]
                if Forward<long>(5000000000L) != 5000000000L { return 1 }
                return Forward<int>(Helpers.First<int>(values))
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
}
