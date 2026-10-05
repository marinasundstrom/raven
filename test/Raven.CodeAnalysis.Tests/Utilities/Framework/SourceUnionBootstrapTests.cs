using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SourceUnionBootstrapTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void SourceUnionEmptyCaseBindsOverLegacyBootstrap(bool importCases, bool queryFirst)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var seed = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("UnionBootstrap",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
                namespace Contracts {
                    public static class Choice {
                        public struct Empty { }
                    }
                    [System.Runtime.CompilerServices.Union]
                    public struct Choice<T> {
                        public object Value => null;
                        public Choice(Choice.Empty value) { }
                        public bool TryGetValue(out Choice.Empty value) { value = default; return false; }
                    }
                }
                """)], paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var seedImage = new MemoryStream();
        var seedResult = seed.Emit(seedImage);
        Assert.True(seedResult.Success, string.Join("\n", seedResult.Diagnostics));
        var directory = Path.Combine(Path.GetTempPath(), "union-bootstrap-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var seedPath = Path.Combine(directory, "UnionBootstrap.dll");
            File.WriteAllBytes(seedPath, seedImage.ToArray());
            var tree = SyntaxTree.ParseText($$"""
                namespace Contracts
                import Contracts.*
                {{(importCases ? "import Contracts.Choice.*" : "")}}
                public union Choice<T> {
                    case Item(T)
                    case Empty
                    static func Create() -> Choice<T> => Empty
                }
                public class Consumer {
                    static func Run() -> bool => Choice<int>.Create() is .Empty
                }
                """);
            var compilation = Compilation.Create("UnionReplacement", [tree],
                paths.Append(seedPath).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            if (queryFirst)
            {
                var expression = tree.GetRoot().DescendantNodes().OfType<ArrowExpressionClauseSyntax>().First().Expression;
                var type = compilation.GetSemanticModel(tree).GetTypeInfo(expression).Type;
                Assert.NotNull(type);
                Assert.NotEqual(TypeKind.Error, type.TypeKind);
            }
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
            using var output = new MemoryStream();
            var result = compilation.Emit(output);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
            Assert.Equal(true, loaded.Assembly.GetType("Contracts.Consumer")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { Directory.Delete(directory, true); }
    }
}
