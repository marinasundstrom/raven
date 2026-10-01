using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class NominalLocalEmissionTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ObjectLocalsPreserveAliasingAndRequireTargetAdmission(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Order {
                var Number: int
                var Pending: bool
                init(number: int, pending: bool) {
                    Number = number
                    Pending = pending
                }
            }
            func Main() -> int {
                let original = Order(41, true)
                let alias = original
                alias.Number = 42
                alias.Pending = false
                if original.Pending { return 1 }
                return original.Number
            }
            """);
        var compilation = Compilation.Create("NominalLocals", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(function)!;
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, ReflectionEmitCapabilities.Shared));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        var denied = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public, Accessibility.Internal],
            [Accessibility.Public, Accessibility.Internal, Accessibility.Private], [Accessibility.Public, Accessibility.Internal]);
        Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out failure, denied));
        Assert.Contains("local type", failure!.Detail);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        Assert.Equal(42, Assembly.Load(output.ToArray()).EntryPoint!.Invoke(null, null));
    }
}
