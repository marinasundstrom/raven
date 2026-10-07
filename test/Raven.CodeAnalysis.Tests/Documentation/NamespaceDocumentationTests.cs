using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class NamespaceDocumentationTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, "Samples")]
    [InlineData(true, "Samples")]
    [InlineData(false, "System")]
    [InlineData(true, "System")]
    public void NamespaceCommentsSurviveColdQueriesAndSyntaxReplacement(bool blockScoped, string namespaceName)
    {
        var source = blockScoped
            ? "/// First overview\nnamespace Samples { public class Widget { } }"
            : "/// First overview\nnamespace Samples\npublic class Widget { }";
        source = source.Replace("Samples", namespaceName);
        var (compilation, tree) = CreateCompilation(source);
        var declaration = tree.GetRoot().DescendantNodes().OfType<BaseNamespaceDeclarationSyntax>().Single();
        var symbol = compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration).ShouldBeAssignableTo<INamespaceSymbol>();
        symbol.GetDocumentationComment()!.Content.ShouldContain("First overview");
        var replacement = SyntaxTree.ParseText(source.Replace("First overview", "Updated overview"), tree.Options);
        var updated = Compilation.Create("Updated", [replacement], TestMetadataReferences.Default, compilation.Options);
        var updatedDeclaration = replacement.GetRoot().DescendantNodes().OfType<BaseNamespaceDeclarationSyntax>().Single();
        var updatedSymbol = updated.GetSemanticModel(replacement).GetDeclaredSymbol(updatedDeclaration)!;
        updatedSymbol.GetDocumentationComment()!.Content.ShouldContain("Updated overview");
        symbol.GetDocumentationComment()!.Content.ShouldContain("First overview");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NamespaceDocumentationRoundTripsThroughSidecarsAndRavenDoc(bool markdown)
    {
        var tree = SyntaxTree.ParseText("/// Namespace overview\nnamespace Samples\npublic class Widget { }",
            new ParseOptions { DocumentationMode = true, DocumentationFormat = DocumentationFormat.Markdown });
        var compilation = Compilation.Create("NamespaceDocs", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var root = Path.Combine(Path.GetTempPath(), "namespace-docs-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        try
        {
            var assemblyPath = Path.Combine(root, "NamespaceDocs.dll");
            using (var stream = File.Create(assemblyPath))
                compilation.Emit(stream).Success.ShouldBeTrue();
            if (markdown) ExternalDocumentationEmitter.WriteMarkdownDocumentation(compilation, Path.Combine(root, "NamespaceDocs.docs"));
            else ExternalDocumentationEmitter.WriteXmlDocumentation(compilation, Path.Combine(root, "NamespaceDocs.xml"));
            var reference = MetadataReference.CreateFromFile(assemblyPath);
            var consumer = Compilation.Create("Consumer", syntaxTrees: [SyntaxTree.ParseText("/// Consumer namespace notes\nnamespace Samples\npublic class ConsumerType { }")], references: [.. TestMetadataReferences.Default, reference],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var widget = consumer.GetTypeByMetadataName("Samples.Widget")!;
            widget.ContainingNamespace!.GetDocumentationComment()!.Content.ShouldContain("Namespace overview");
            consumer.GlobalNamespace.GetMembers("Samples").OfType<INamespaceSymbol>().Single()
                .GetDocumentationComment()!.Content.ShouldContain("Namespace overview");
            consumer.GlobalNamespace.GetMembers("Samples").OfType<INamespaceSymbol>().Single()
                .GetDocumentationComment()!.Content.ShouldContain("Consumer namespace notes");
            var content = Path.Combine(root, "content");
            Directory.CreateDirectory(content);
            File.WriteAllText(Path.Combine(content, "namespace.md"), "---\nuid: N:Samples\n---\nAdditional namespace guidance.");
            var output = Path.Combine(root, "site");
            DocumentationGenerator.ProcessAssembly(consumer, (IAssemblySymbol)consumer.GetAssemblyOrModuleSymbol(reference)!, output,
                new DocumentationSiteOptions([], ApiContent: content));
            var html = File.ReadAllText(Path.Combine(output, "Samples/index.html"));
            html.ShouldContain("Namespace overview");
            html.ShouldContain("Additional namespace guidance");
        }
        finally { Directory.Delete(root, true); }
    }

    [Fact]
    public void SplitNamespaceCommentsMergeWithoutDocumentingImplicitParents()
    {
        var trees = new[]
        {
            SyntaxTree.ParseText("/// First part\nnamespace Samples.Docs\npublic class One { }"),
            SyntaxTree.ParseText("/// Second part\nnamespace Samples.Docs\npublic class Two { }")
        };
        var compilation = Compilation.Create("SplitDocs", trees, TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var symbol = compilation.GetTypeByMetadataName("Samples.Docs.One")!.ContainingNamespace!;
        symbol.GetDocumentationComment()!.Content.ShouldContain("First part");
        symbol.GetDocumentationComment()!.Content.ShouldContain("Second part");
        symbol.ContainingNamespace!.GetDocumentationComment().ShouldBeNull();
    }
}
