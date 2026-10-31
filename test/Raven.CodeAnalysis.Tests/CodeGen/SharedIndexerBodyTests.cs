using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedIndexerBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void AssignmentEvaluatesReceiverIndicesAndValueOnceInOrder(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Cell {
                private var number: int = 0
                var self[first: int, second: int]: int {
                    get => number
                    set => number = value
                }
            }
            class Evaluation {
                private val cell: Cell = Cell()
                private var trace: int = 0
                func Receiver() -> Cell {
                    trace = trace * 10 + 1
                    return cell
                }
                func Index() -> int {
                    trace = trace * 10 + 2
                    return 0
                }
                func Value() -> int {
                    trace = trace * 10 + 3
                    return 42
                }
                val Trace: int => trace
                val Result: int => cell[0, 0]
            }
            func Main() -> int {
                let evaluation = Evaluation()
                evaluation.Receiver()[evaluation.Index(), evaluation.Index()] = evaluation.Value()
                if evaluation.Trace != 1223 { return 1 }
                return evaluation.Result
            }
            """);
        var compilation = Compilation.Create("IndexedEvaluation", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void IndexedAccessorsSharePlanningAndPreserveOverloads(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Buffer {
                private val data: int[] = [1, 2, 3]
                var self[index: int]: int {
                    get => data[index]
                    set => data[index] = value
                }
                val self[index: long]: int => data[0]
                var self[row: int, column: int]: int {
                    get => data[row + column]
                    set => data[row + column] = value
                }
            }
            func Main() -> int {
                let buffer = Buffer()
                buffer[0, 1] = 40
                buffer[0] = 2
                if buffer[0L] != 2 { return 1 }
                return buffer[1] + buffer[0]
            }
            """);
        var compilation = Compilation.Create("SharedIndexers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<IndexerDeclarationSyntax>())
        {
            var property = (IPropertySymbol)model.GetDeclaredSymbol(syntax)!;
            foreach (var accessor in new[] { property.GetMethod, property.SetMethod }.OfType<IMethodSymbol>())
            {
                Assert.True(SourceCallablePlan.TryCreate(accessor, out var plan, ReflectionEmitCapabilities.Shared));
                Assert.Equal(EmissionDeclarationKind.IndexerAccessor, plan!.DeclarationKind);
                Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
                var denied = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                    Enum.GetValues<EmissionDeclarationKind>().Where(k => k != EmissionDeclarationKind.IndexerAccessor),
                    [Accessibility.Internal, Accessibility.Public], [Accessibility.Public], allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsArrays: true);
                Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out _, denied));
            }
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var type = assembly.GetType("Buffer")!;
        Assert.Equal(3, type.GetProperties().Length);
        Assert.Null(type.GetProperty("Item", [typeof(long)])!.SetMethod);
    }
}
