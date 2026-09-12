using Raven.CodeAnalysis.Syntax;

using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.Tests;

public class RuntimePropagationContractTests
{
    [Theory]
    [InlineData("selected", true)]
    [InlineData("default", false)]
    [InlineData("wrong-assembly", false)]
    [InlineData("wrong-name", false)]
    [InlineData("missing-extraction", false)]
    public void TargetSelectionIsExplicitAndDoesNotFallBack(string scenario, bool accepted)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-propagation", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "PropagationContracts.dll");
            var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
            var source = """
                namespace Contracts {
                    public interface Propagatable<S,O,R> {
                        bool TryGetOutput(out O output);
                        bool TryGetResidual(out R residual);
                    }
                    public struct Attempt : Propagatable<Attempt,int,string> {
                        public bool TryGetOutput(out int output) { output = 42; return true; }
                        public bool TryGetResidual(out string residual) { residual = "failure"; return false; }
                        public static Attempt FromResidual(string residual) => default;
                    }
                }
                """;
            if (scenario == "missing-extraction") source = source.Replace("bool TryGetOutput(out O output);", "");
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("PropagationContracts",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)],
                paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var contract = scenario == "default" ? null : new RuntimePropagationContract(
                scenario == "wrong-assembly" ? "Missing" : "PropagationContracts",
                scenario == "wrong-name" ? "Contracts.Missing`3" : "Contracts.Propagatable`3");
            var compilation = Compilation.Create("Consumer", [SyntaxTree.ParseText("""
                func Test(value: Contracts.Attempt) -> Contracts.Attempt {
                    let output = value?
                    return Contracts.Attempt.FromResidual("failure")
                }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, runtimePropagationContract: contract));
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (!accepted) { Assert.NotEmpty(errors); return; }
            Assert.Empty(errors);
            using var image = new MemoryStream();
            var emission = compilation.Emit(image);
            Assert.True(emission.Success, string.Join("\n", emission.Diagnostics));
            image.Position = 0;
            using var reader = new PEReader(image);
            var metadata = reader.GetMetadataReader();
            foreach (var handle in metadata.MethodDefinitions)
            {
                var method = metadata.GetMethodDefinition(handle);
                if (method.RelativeVirtualAddress != 0)
                    Assert.Empty(reader.GetMethodBody(method.RelativeVirtualAddress).ExceptionRegions);
            }

            var tree = compilation.SyntaxTrees.Single();
            var propagation = tree.GetRoot().DescendantNodes().OfType<PropagateExpressionSyntax>().Single();
            Assert.Equal(SpecialType.System_Int32, compilation.GetSemanticModel(tree).GetTypeInfo(propagation).Type!.SpecialType);
        }
        finally { Directory.Delete(directory, true); }
    }

    [Fact]
    public void CopiesPreserveTheContractAndResetRestoresDefault()
    {
        var contract = new RuntimePropagationContract("Target", "System.Propagatable`3");
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, runtimePropagationContract: contract)
            .WithRunAnalyzers(false).WithOutputKind(OutputKind.ConsoleApplication)
            .WithRuntimeIterationContract(null).WithMetadataImportOptions(null);
        Assert.Equal(contract, options.RuntimePropagationContract);
        Assert.Null(options.WithRuntimePropagationContract(null).RuntimePropagationContract);
    }
}
