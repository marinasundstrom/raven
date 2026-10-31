using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class NamespaceDocumentationTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, "Namespace")]
    [InlineData(true, "Module")]
    public void ModuleTerminologyRetainsAssemblyOwner(bool modules, string label)
    {
        var tree = SyntaxTree.ParseText("namespace Examples { public class Item {} }");
        var compilation = Compilation.Create("Package", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var output = Path.Combine(Path.GetTempPath(), "module-docs-" + Guid.NewGuid().ToString("N"));
        try
        {
            DocumentationGenerator.ProcessAssembly(compilation, compilation.Assembly, output,
                new DocumentationSiteOptions([], ModuleTerminology: modules));
            var html = File.ReadAllText(Path.Combine(output, "Examples/Item/index.html"));
            Assert.Contains($"<strong>{label}</strong>", html);
            Assert.Contains("<strong>Assembly</strong>", html);
            Assert.Contains(label + " overview", html);
            Assert.Contains("Package", html);
        }
        finally { if (Directory.Exists(output)) Directory.Delete(output, true); }
    }

    [Theory]
    [Trait("Target", "DotNet")]
    [Trait("Layer", "documentation")]
    [InlineData("Samples")]
    [InlineData("")]
    public void DotNetAssemblyMemberSidecarsUseCarrierIds(string namespaceName)
        => VerifyAssemblyMemberSidecars(TargetPlatform.DotNet, "NamespaceMembers.", namespaceName);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [Trait("Layer", "documentation")]
    [InlineData("Samples")]
    [InlineData("")]
    public void NeoClrAssemblyMemberSidecarsUseNamespaceIds(string namespaceName)
        => VerifyAssemblyMemberSidecars(TargetPlatform.NeoCLR, "", namespaceName);

    private static void VerifyAssemblyMemberSidecars(TargetPlatform target, string carrier, string namespaceName)
    {
        var declaration = namespaceName.Length == 0 ? "" : "namespace " + namespaceName + "\n";
        var owner = (namespaceName.Length == 0 ? "" : namespaceName + ".") + carrier;
        var tree = SyntaxTree.ParseText(declaration + "/// Function summary\npublic func Score(value: int) -> int { value }\n/// Generic summary\npublic func Identity<T>(value: T) -> T => value\n/// Constant summary\npublic const Scale: int = 2",
            new ParseOptions { DocumentationMode = true, DocumentationFormat = DocumentationFormat.Markdown });
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var references = TestMetadataReferences.Default.ToArray();
        if (target == TargetPlatform.NeoCLR)
        {
            var corePath = references.OfType<PortableExecutableReference>()
                .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
            using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(corePath);
            core.Name.Name = "NeoCLR.CoreProbe";
            core.Name.PublicKey = [];
            using var image = new MemoryStream();
            core.Write(image);
            references = [.. references, MetadataReference.CreateFromImage(image.ToArray())];
            options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(null);
        }
        var compilation = Compilation.Create("MemberDocs", [tree], references, options);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var root = Path.Combine(Path.GetTempPath(), "member-docs-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        try
        {
            ExternalDocumentationEmitter.WriteXmlDocumentation(compilation, Path.Combine(root, "docs.xml"));
            ExternalDocumentationEmitter.WriteMarkdownDocumentation(compilation, Path.Combine(root, "docs.docs"));
            var xml = System.Xml.Linq.XDocument.Load(Path.Combine(root, "docs.xml"));
            var ids = xml.Descendants("member").Select(member => member.Attribute("name")!.Value).ToArray();
            ids.ShouldContain("M:" + owner + "Score(System.Int32)");
            ids.ShouldContain("F:" + owner + "Scale");
            ids.ShouldContain("M:" + owner + "Identity``1(``0)");
            var markdown = string.Join("\n", Directory.EnumerateFiles(Path.Combine(root, "docs.docs"), "*.md", SearchOption.AllDirectories).Select(File.ReadAllText));
            markdown.ShouldContain("xref: M:" + owner + "Score(System.Int32)");
            markdown.ShouldContain("xref: F:" + owner + "Scale");
            markdown.ShouldContain("Function summary");
            markdown.ShouldContain("Constant summary");
            markdown.ShouldContain("xref: M:" + owner + "Identity``1(``0)");
        }
        finally { Directory.Delete(root, true); }
    }

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
