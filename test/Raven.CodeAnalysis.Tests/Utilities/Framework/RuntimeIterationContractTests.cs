using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RuntimeIterationContractTests
{
    private static readonly RuntimeIterationContract Contract = new("IterationContracts",
        "Contracts.Iterable`1", "Contracts.Iterator`1");

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SelectedContractBindsInheritedIterableAndInfersElement(bool renamed)
    {
        WithContracts("bool", (references, _) =>
        {
            var contract = renamed ? Contract with { AcquisitionMethod = "Open", AdvanceMethod = "Advance", CurrentProperty = "Item" } : Contract;
            var compilation = Create(references, contract);
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            var tree = compilation.SyntaxTrees.Single();
            var loop = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().Single();
            var bound = Assert.IsType<BoundForStatement>(compilation.GetSemanticModel(tree).GetBoundNode(loop));
            Assert.Equal(SpecialType.System_Int32, bound.Iteration.ElementType.SpecialType);
            Assert.Equal(contract.AcquisitionMethod, bound.Iteration.GetEnumeratorMethod!.Name);
            Assert.Equal("Iterable", bound.Iteration.GetEnumeratorMethod.ContainingType!.Name);
            Assert.Equal(contract.AdvanceMethod, bound.Iteration.MoveNextMethod!.Name);
        }, renamed);
    }

    [Theory]
    [InlineData("wrong-assembly")]
    [InlineData("wrong-member")]
    [InlineData("wrong-signature")]
    public void MalformedContractReportsDiagnosticWithoutFallback(string scenario)
    {
        WithContracts(scenario == "wrong-signature" ? "int" : "bool", (references, _) =>
        {
            var contract = scenario switch
            {
                "wrong-assembly" => Contract with { AssemblyName = "Missing" },
                "wrong-member" => Contract with { AcquisitionMethod = "Missing" },
                _ => Contract
            };
            var diagnostics = Create(references, contract).GetDiagnostics();
            Assert.Contains(diagnostics, d => d.Id == "RAVT001" && d.Severity == DiagnosticSeverity.Error);
        });
    }

    [Fact]
    public void DefaultTargetDoesNotRecognizeAlternativeProtocol()
    {
        WithContracts("bool", (references, _) => Assert.Contains(Create(references, null).GetDiagnostics(),
            d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void OptionCopiesRetainContractAndItCanBeReset()
    {
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, runtimeIterationContract: Contract)
            .WithOutputKind(OutputKind.ConsoleApplication).WithRunAnalyzers(false)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"));
        Assert.Equal(Contract, options.RuntimeIterationContract);
        Assert.Null(options.WithRuntimeIterationContract(null).RuntimeIterationContract);
    }

    private static Compilation Create(MetadataReference[] references, RuntimeIterationContract? contract) =>
        Compilation.Create("Consumer", [SyntaxTree.ParseText("""
            import Contracts.*
            func Read(values: List<int>) {
                for value in values { let copy = value }
            }
            """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                runtimeIterationContract: contract));

    private static void WithContracts(string advanceType, Action<MetadataReference[], string> action, bool renamed = false)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-iteration", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "IterationContracts.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            var source = $"namespace Contracts {{ public interface Iterable<T> {{ Iterator<T> {(renamed ? "Open" : "GetIterator")}(); }} public interface Iterator<T> {{ {advanceType} {(renamed ? "Advance" : "MoveNext")}(); T {(renamed ? "Item" : "Current")} {{ get; }} }} public interface List<T> : Iterable<T> {{ }} }}";
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("IterationContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)],
                paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
            {
                var result = declarations.Emit(stream);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }
            action(paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(), path);
        }
        finally { Directory.Delete(directory, true); }
    }
}
