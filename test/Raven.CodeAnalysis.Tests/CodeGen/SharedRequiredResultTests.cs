using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedRequiredResultTests
{
    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void MatchValueRetainsItsRequiredResult(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int {
                let value = 2
                let result = match value {
                    _ => 42
                }
                return result
            }
            """);
        var compilation = Compilation.Create("RequiredResult" + optimization, [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        // Exercise the shared planner with pattern admission; ordinary .NET emission
        // currently retains its existing pattern backend.
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>(), Enum.GetValues<EmissionDeclarationKind>(),
            [Accessibility.Public, Accessibility.Internal], [Accessibility.Public], [Accessibility.Internal],
            allowsManagedReferences: true, allowsCasePatterns: true);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }
}
