using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedPropertyAccessorTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ComputedAndExplicitAccessorsShareBodies(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Gauge {
                private var amount: int
                init(amount: int) {
                    self.amount = amount
                    Offset = 0
                }
                var Offset: int {
                    get => field
                    set => field = value + 1
                }
                val Doubled: int => amount * 2
                var Amount: int {
                    get { return self.amount }
                    set {
                        if value < 0 { amount = 0 } else { amount = value }
                    }
                }
                val Adjusted: int {
                    get => amount + 1
                    private set => amount = value - 1
                }
                func Reset(value: int) { Adjusted = value }
            }
            func Main() -> int {
                let gauge = Gauge(3)
                if gauge.Doubled != 6 { return 1 }
                gauge.Amount = -1
                if gauge.Amount != 0 { return 2 }
                gauge.Offset = 41
                if gauge.Offset != 42 { return 13 }
                gauge.Reset(22)
                if gauge.Adjusted != 22 { return 3 }
                return gauge.Doubled
            }
            """);
        var compilation = Compilation.Create("ComputedProperties", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>())
        {
            var property = (IPropertySymbol)model.GetDeclaredSymbol(syntax)!;
            foreach (var accessor in new[] { property.GetMethod, property.SetMethod }.OfType<IMethodSymbol>())
            {
                Assert.True(SourceCallablePlan.TryCreate(accessor, out var plan, ReflectionEmitCapabilities.Shared));
                Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            }
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var gaugeType = assembly.GetType("Gauge")!;
        Assert.Equal(2, gaugeType.GetFields(BindingFlags.NonPublic | BindingFlags.Instance).Length);
        Assert.Equal(4, gaugeType.GetProperties().Length);
        Assert.Null(gaugeType.GetProperty("Doubled")!.SetMethod);
        Assert.True(gaugeType.GetProperty("Adjusted")!.GetSetMethod(true)!.IsPrivate);
    }
}
