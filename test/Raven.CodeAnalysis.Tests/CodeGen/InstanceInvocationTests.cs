using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class InstanceInvocationTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release, false)]
    [InlineData(OptimizationLevel.Debug, false)]
    [InlineData(OptimizationLevel.Release, true)]
    [InlineData(OptimizationLevel.Debug, true)]
    public void InstanceCallsPreserveReceiverArgumentsAndPrivateCalls(OptimizationLevel optimization, bool privateStorage)
    {
        var source = """
            class Counter {
                var Number: int
                init(number: int) { Number = number }
                public func Next() -> int {
                    let previous = Number
                    Number = Add(Number, 1)
                    return previous
                }
                private func Add(left: int, right: int) -> int => left + right
                public func Combine(left: int, right: int) -> int => left * 10 + right
                public func Reset(number: int) { Number = number }
                public func Increment(amount: int) -> int {
                    Number = Add(Number, amount)
                    return Number
                }
            }
            func Main() -> int {
                let counter = Counter(1)
                if counter.Combine(counter.Next(), counter.Next()) != 12 { return 1 }
                if counter.Number != 3 { return 2 }
                counter.Reset(40)
                return counter.Increment(2)
            }
            """;
        if (privateStorage)
            source = source.Replace("var Number: int", "private var Number: int\n public func Read() -> int => self.Number")
                .Replace("counter.Number", "counter.Read()");
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("InstanceCalls", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().Where(n => n is FunctionStatementSyntax or MethodDeclarationSyntax))
        {
            var symbol = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        if (privateStorage)
        {
            var counter = assembly.GetType("Counter")!;
            Assert.Empty(counter.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance));
            Assert.Single(counter.GetFields(BindingFlags.NonPublic | BindingFlags.Instance));
        }
    }
}
