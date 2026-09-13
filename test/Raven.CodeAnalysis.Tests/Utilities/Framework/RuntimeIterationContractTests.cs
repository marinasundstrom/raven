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
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, runtimeIterationContract: Contract with { ArrayShapeTypeName = "Contracts.ArrayShape`1" })
            .WithOutputKind(OutputKind.ConsoleApplication).WithRunAnalyzers(false)
            .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"));
        Assert.Equal(Contract with { ArrayShapeTypeName = "Contracts.ArrayShape`1" }, options.RuntimeIterationContract);
        Assert.Null(options.WithRuntimeIterationContract(null).RuntimeIterationContract);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void VectorInterfaceProjectionIsAnExplicitTargetCapability(bool enabled)
    {
        WithContracts("bool", (references, _) =>
        {
            var compilation = Compilation.Create("ArrayConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Pass(values: int[]) -> Iterable<int> { return values }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                    runtimeIterationContract: Contract with { ArraysImplementIterable = enabled }));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (enabled) Assert.Empty(errors); else Assert.NotEmpty(errors);
            var vector = Assert.IsAssignableFrom<IArrayTypeSymbol>(compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Int32)));
            Assert.Equal(enabled, vector.AllInterfaces.Any(i => i.Name == "Iterable"));
            var rectangular = Assert.IsAssignableFrom<IArrayTypeSymbol>(compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Int32), 2));
            Assert.DoesNotContain(rectangular.AllInterfaces, i => i.Name == "Iterable");
        });
    }

    [Theory]
    [InlineData("Contracts.ArrayShape`1", "IterationContracts", true)]
    [InlineData("Contracts.WrongArity`2", "IterationContracts", false)]
    [InlineData("Contracts.ValueShape`1", "IterationContracts", false)]
    [InlineData("Contracts.Missing`1", "IterationContracts", false)]
    [InlineData("Contracts.Iterable`1", "IterationContracts", false)]
    [InlineData("Contracts.ArrayShape`1", "Missing", false)]
    public void ArrayShapeProjectsDeclaredInterfaces(string shapeName, string assemblyName, bool valid)
    {
        WithContracts("bool", (references, _) =>
        {
            var contract = Contract with { ArrayShapeTypeName = shapeName, AssemblyName = assemblyName, ArraysImplementIterable = true };
            var compilation = Compilation.Create("ShapeConsumer", [SyntaxTree.ParseText("""
                import Contracts.*
                func Pass(values: int[]) -> View<int> { return values }
                func Read() -> View<int> { return Factory.Values() }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                    runtimeIterationContract: contract));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
            if (valid) Assert.Empty(errors); else Assert.NotEmpty(errors);
            var vector = Assert.IsAssignableFrom<IArrayTypeSymbol>(compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Int32)));
            Assert.Equal(valid, vector.AllInterfaces.Any(i => i.Name == "View"));
            Assert.Equal(valid, vector.AllInterfaces.Any(i => i.Name == "Iterable"));
            if (valid)
                Assert.All(vector.AllInterfaces.Where(i => i.Name is "View" or "Iterable"),
                    i => Assert.Equal(SpecialType.System_Int32, i.TypeArguments.Single().SpecialType));
            Assert.DoesNotContain(vector.AllInterfaces, i => i.Name == "IList" && i.Arity == 1);
            var rectangular = Assert.IsAssignableFrom<IArrayTypeSymbol>(compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Int32), 2));
            Assert.DoesNotContain(rectangular.AllInterfaces, i => i.Name == "View");
        });
    }

    [Fact]
    public void ConfiguredArrayShapeAndVectorShareSourceAndMetadataSemantics()
    {
        WithContracts("bool", (references, _) =>
        {
            var tree = SyntaxTree.ParseText("""
                import Contracts.*
                func Pass(values: ArrayShape<int>) -> int[] { return values }
                func Reverse(values: int[]) -> ArrayShape<int> { return values }
                func Imported() -> int[] { return Factory.Shaped() }
                func Element(values: ArrayShape<int>) -> int {
                    values[0] = 42
                    return values[0]
                }
                func Iterator(values: ArrayShape<int>) -> Contracts.Iterator<int> {
                    return values.GetIterator()
                }
                func Shape() -> System.Type { return typeof(ArrayShape<int>) }
                func Nested(values: ArrayShape<ArrayShape<int>>) -> int[][] { return values }
                """);
            var compilation = Compilation.Create("UnifiedArrays", [tree], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                    runtimeIterationContract: Contract with { ArrayShapeTypeName = "Contracts.ArrayShape`1" }));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var stream = new MemoryStream();
            var emitted = compilation.Emit(stream);
            Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            stream.Position = 0;
            using var image = Mono.Cecil.AssemblyDefinition.ReadAssembly(stream);
            var functions = image.MainModule.Types.SelectMany(t => t.Methods).ToArray();
            foreach (var name in new[] { "Pass", "Reverse" })
            {
                var method = Assert.Single(functions, m => m.Name == name);
                Assert.Equal("System.Int32[]", method.Parameters[0].ParameterType.FullName);
                Assert.Equal("System.Int32[]", method.ReturnType.FullName);
            }
        });
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ArrayShapeAliasIsOptInAndRetainsArrayInvariance(bool configured)
    {
        WithContracts("bool", (references, _) =>
        {
            var compilation = Compilation.Create("AliasPolicy", [SyntaxTree.ParseText("""
                import Contracts.*
                func Pass(values: ArrayShape<string>) -> string[] { return values }
                func Widen(values: ArrayShape<string>) -> object[] { return values }
                """)], references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary,
                    runtimeIterationContract: configured ? Contract with { ArrayShapeTypeName = "Contracts.ArrayShape`1" } : null)
                    .WithAllowArrayCovariance(false));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            Assert.Equal(configured ? 1 : 2, errors.Length);
        });
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
            var source = $$"""
                namespace Contracts {
                    public class WrongArity<T, U> { }
                    public struct ValueShape<T> { }
                    public interface View<T> : Iterable<T> { }
                    public abstract class ArrayShape<T> : View<T> {
                        public abstract Iterator<T> {{(renamed ? "Open" : "GetIterator")}}();
                    }
                    public static class Factory {
                        public static int[] Values() => null;
                        public static ArrayShape<int> Shaped() => null;
                    }
                    public interface Iterable<T> {
                        Iterator<T> {{(renamed ? "Open" : "GetIterator")}}();
                    }
                    public interface Iterator<T> {
                        {{advanceType}} {{(renamed ? "Advance" : "MoveNext")}}();
                        T {{(renamed ? "Item" : "Current")}} { get; }
                    }
                    public interface List<T> : Iterable<T> { }
                }
                """;
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
