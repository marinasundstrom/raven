using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests.Metadata;

public sealed class AttributedCustomUnionTests : CompilationTestBase
{
    [Theory]
    [InlineData("struct")]
    [InlineData("class")]
    public void TypedCaseCarrierLoadsAsUnionWithoutBoxedValue(string kind)
    {
        var directory = Path.Combine(Path.GetTempPath(), "custom-union", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var source = $$"""
                namespace System.Runtime.CompilerServices { public sealed class UnionAttribute : System.Attribute { } }
                namespace Independent {
                    [System.Runtime.CompilerServices.Union]
                    public {{kind}} Choice {
                        public struct First { public First() { } }
                        public struct Second { }
                        public Choice(First value) { }
                        public Choice(Second value) { }
                        public bool IsFirst => false;
                        public bool IsSecond => false;
                        public First GetFirst() => default;
                        public Second GetSecond() => default;
                    }
                    [System.Runtime.CompilerServices.Union]
                    public struct Unrelated {
                        public Unrelated(int value) { }
                        public struct Nested { }
                    }
                    public interface First { }
                    public class Ordinary { }
                    public struct Unmarked {
                        public struct Case { }
                        public Unmarked(Case value) { }
                        public bool IsCase => false;
                        public Case GetCase() => default;
                    }
                }
                """;
            var version = TargetFrameworkResolver.ResolveVersion("net10.0");
            var references = TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path));
            var assemblyPath = Path.Combine(directory, "Independent.dll");
            var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("Independent",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)], references,
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            using (var output = File.Create(assemblyPath))
            {
                var emitted = declarations.Emit(output);
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var (compilation, _) = CreateCompilation("", references:
                TestMetadataReferences.Default.Concat([MetadataReference.CreateFromFile(assemblyPath)]).ToArray());
            var symbol = compilation.GetTypeByMetadataName("Independent.Choice");
            var union = Assert.IsAssignableFrom<IUnionSymbol>(symbol);
            Assert.Equal(new[] { "First", "Second" }, union.DeclaredCaseTypes.Select(type => type.Name).Order().ToArray());
            Assert.Equal(2, union.MemberTypes.Length);
            Assert.Empty(union.GetMembers("Value"));
            Assert.False(compilation.GetTypeByMetadataName("Independent.Unrelated") is IUnionSymbol);
            Assert.False(compilation.GetTypeByMetadataName("Independent.Unmarked") is IUnionSymbol);
            var site = Path.Combine(directory, "site");
            DocumentationGenerator.ProcessAssembly(compilation, symbol!.ContainingAssembly, site);
            var html = File.ReadAllText(Path.Combine(site, "Independent/Choice/index.html"));
            Assert.Contains("symbol-icon--union", html);
            Assert.Contains($"union {kind} Choice", html);
            Assert.Contains("id=\"cases\"", html);
            Assert.Contains("case First", html);
            Assert.False(Directory.Exists(Path.Combine(site, "Independent/Choice/First")));
            Assert.True(File.Exists(Path.Combine(site, "Independent/First/index.html")));
            var ordinaryPage = File.ReadAllText(Path.Combine(site, "Independent/Ordinary/index.html"));
            Assert.Contains("symbol-icon--class", ordinaryPage);
            Assert.Contains("symbol-icon--class", html);
            Assert.DoesNotContain("symbol-icon--type", ordinaryPage);
            var xrefs = DocumentationGenerator.ExportXrefs(site);
            Assert.Equal("Independent/Choice/index.html", xrefs["M:Independent.Choice+First..ctor"]);
            Assert.Equal("Independent/Choice/index.html", xrefs["M:Independent.Choice..ctor"]);
        }
        finally { Directory.Delete(directory, true); }
    }
}
