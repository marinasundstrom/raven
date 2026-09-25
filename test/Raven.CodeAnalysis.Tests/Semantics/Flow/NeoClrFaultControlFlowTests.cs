using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NeoClrFaultControlFlowTests : CompilationTestBase
{
    [Theory]
    [InlineData("NeoCLR.CoreProbe", true, "System.Fault", true)]
    [InlineData("NeoCLR.CoreProbe", true, "Fault", true)]
    [InlineData("OrdinaryLibrary", true, "System.Fault", false)]
    [InlineData("NeoCLR.CoreProbe", false, "System.ArbitraryContainer.Fault", false)]
    public void FaultCall_OnlyRuntimeNamespaceFunctionTerminates(
        string assemblyName, bool namespaceMember, string call, bool terminates)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-fault-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var declarations = $$"""
                namespace System.Runtime.CompilerServices {
                    public sealed class TopLevelAttribute : System.Attribute { }
                }
                namespace System {
                    {{(namespaceMember ? "[System.Runtime.CompilerServices.TopLevel]" : "")}}
                    public static class ArbitraryContainer {
                        public static void Fault(string message) { }
                    }
                }
                """;
            var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net10.0"))
                .Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path));
            var metadata = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create(assemblyName,
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(declarations)], references,
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            var path = Path.Combine(directory, assemblyName + ".dll");
            using (var output = File.Create(path))
            {
                var result = metadata.Emit(output);
                Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            }

            var (compilation, tree) = CreateCompilation($$"""
                import System.*
                func Stop() -> int {
                    {{call}}("stopped")
                }
                {{(terminates ? "func StopOut(out value: int) { " + call + "(\"stopped\") }" : "")}}
                func After() {
                    {{call}}("stopped")
                    let unreachable = 42
                }
                func Branch(flag: bool) -> int {
                    if flag {
                        {{call}}("stopped")
                    } else {
                        return 1
                    }
                }
                """, references: TestMetadataReferences.Default.Concat([MetadataReference.CreateFromFile(path)]).ToArray());
            var diagnostics = compilation.GetDiagnostics();
            Assert.Equal(terminates ? 1 : 0, diagnostics.Count(d => d.Id == CompilerDiagnostics.UnreachableCodeDetected.Id));
            Assert.Equal(terminates ? 0 : 1, diagnostics.Count(d => d.Id == CompilerDiagnostics.NotAllCodePathsReturnAValue.Id));
            Assert.Equal(terminates ? 0 : 1, diagnostics.Count(d => d.Id == "RAV1503"));
            Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error &&
                d.Id != CompilerDiagnostics.NotAllCodePathsReturnAValue.Id && d.Id != "RAV1503");

            var stop = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().First();
            var body = stop.DescendantNodes().OfType<BlockStatementSyntax>().First();
            var flow = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);
            Assert.True(flow.Succeeded);
            Assert.Equal(!terminates, flow.EndPointIsReachable);

            if (terminates)
            {
                using var output = new MemoryStream();
                var emitted = compilation.Emit(output);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
