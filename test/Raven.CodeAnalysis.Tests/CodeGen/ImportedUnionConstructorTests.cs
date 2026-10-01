using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ImportedUnionConstructorTests
{
    public static TheoryData<OptimizationLevel, bool, string> Cases
    {
        get
        {
            var cases = new TheoryData<OptimizationLevel, bool, string>();
            foreach (var optimization in new[] { OptimizationLevel.Debug, OptimizationLevel.Release })
                foreach (var reverse in new[] { false, true })
                    foreach (var expression in new[] { "None()", ".None()", "None", ".None" })
                        cases.Add(optimization, reverse, expression);
            return cases;
        }
    }

    [Theory]
    [MemberData(nameof(Cases))]
    public void ExplicitCarrierConstructionPreservesSelectedCase(OptimizationLevel optimization, bool reverse, string emptyExpression)
    {
        var declarations = reverse ? "case None\ncase Some(value: T)" : "case Some(value: T)\ncase None";
        var library = Compilation.Create("ImportedChoice", [SyntaxTree.ParseText($$"""
            namespace Example {
                public union Choice<T> {
                    {{declarations}}
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var libraryResult = library.Emit(libraryImage);
        Assert.True(libraryResult.Success, string.Join("; ", libraryResult.Diagnostics));
        var directory = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var libraryPath = Path.Combine(directory, "ImportedChoice.dll");
            File.WriteAllBytes(libraryPath, libraryImage.ToArray());
            var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(libraryPath)).ToArray();
            var tree = SyntaxTree.ParseText($$"""
                import Example.*
                import Example.Choice.*
                class Item {
                    public val Value: int = 42
                }
                class Runner {
                    public static func Run(empty: bool) -> int {
                        let choice = if empty { Choice<Item>({{emptyExpression}}) } else { Choice<Item>(Some<Item>(Item())) }
                        return match choice {
                            Some(let item) => item.Value
                            None => -1
                        }
                    }
                }
                """);
            var compilation = Compilation.Create("ChoiceConsumer", [tree], references,
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
            var model = compilation.GetSemanticModel(tree);
            foreach (var invocation in tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
                         .Where(n => n.ToString().StartsWith("Choice<Item>(")))
            {
                var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
                Assert.Equal(invocation.ToString().Contains("None") ? "None" : "Some", selected.Parameters.Single().Type.Name);
            }
            using var image = new MemoryStream();
            var result = compilation.Emit(image);
            Assert.True(result.Success, string.Join("; ", result.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
            var run = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
            Assert.Equal(-1, run.Invoke(null, [true]));
            Assert.Equal(42, run.Invoke(null, [false]));
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
