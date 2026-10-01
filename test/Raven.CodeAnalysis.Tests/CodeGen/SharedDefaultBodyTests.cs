using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedDefaultBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void TypedDefaultsSharePlanningAndClearGenericArrays(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Defaults {
                func Empty<T>() -> T => default(T)
                func Clear<T>(values: T[]) {
                    var index = 0
                    while index < values.Length {
                        values[index] = default(T)
                        index = index + 1
                    }
                }
            }
            func Main() -> int {
                let helper = Defaults()
                let values: int[] = [1, 2, 3]
                helper.Clear(values)
                return values[0] + values[1] + values[2] + 42
            }
            """);
        var compilation = Compilation.Create("TypedDefaults", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            var noDefault = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(),
                Enum.GetValues<LinearInstructionKind>().Where(k => k != LinearInstructionKind.DefaultValue),
                Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
                [Accessibility.Public], [Accessibility.Internal], allowsArrays: true, allowsGenericMethods: true, allowsGenericInstanceMethods: true);
            Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out _, noDefault));
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var type = assembly.GetType("Defaults")!;
        var helper = Activator.CreateInstance(type);
        var empty = type.GetMethod("Empty")!;
        foreach (var argument in new[] { typeof(int), typeof(long), typeof(bool), typeof(string), typeof(int[]), type })
            Assert.Equal(argument.IsValueType ? Activator.CreateInstance(argument) : null, empty.MakeGenericMethod(argument).Invoke(helper, null));
    }
}
