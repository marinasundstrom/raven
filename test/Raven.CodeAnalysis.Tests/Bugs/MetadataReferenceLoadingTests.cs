using System;
using System.IO;
using System.Security.Cryptography;
using System.Linq;
using System.Text;
using System.Text.Json;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class MetadataReferenceLoadingTests
{
    [Fact]
    public void GetTypeByMetadataName_LoadsReferences_WhenNoSyntaxTrees()
    {
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var referencePaths = TargetFrameworkResolver.GetReferenceAssemblies(version);
        var references = referencePaths.Select(MetadataReference.CreateFromFile).ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(references);

        var consoleType = compilation.GetTypeByMetadataName("System.Console");
        Assert.NotNull(consoleType);

        var stringType = compilation.GetTypeByMetadataName("System.String");
        Assert.NotNull(stringType);
    }

    [Fact]
    public void WildcardImport_ResolvesNestedMetadataNamespace()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
namespace Outer.Inner {
    public static class Observer {
        public static func Ping() -> int {
            return 42
        }
    }
}
""",
            assemblyName: "nested-metadata-fixture");

        var tree = SyntaxTree.ParseText(
            """
import Outer.Inner.*

val value = Observer.Ping()
""");

        var compilation = Compilation.Create(
            "consumer",
            [tree],
            [.. TestMetadataReferences.Default, metadataReference],
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void GetDiagnostics_DoesNotCrash_WhenMetadataReferenceHasMissingOptionalDependencies()
    {
        var tree = SyntaxTree.ParseText(
            """
import System.Console.*

val message = $"Hello"
WriteLine(message)
""");

        var compilation = Compilation.Create(
            "consumer",
            [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(Compilation).Assembly.Location)],
            new CompilationOptions(OutputKind.ConsoleApplication));

        var exception = Record.Exception(() => compilation.GetDiagnostics());

        Assert.Null(exception);
    }

    [Fact]
    public void MetadataMethods_WithUnreadableSignatures_DoNotBecomeParameterless()
    {
        var dependencyTree = SyntaxTree.ParseText("""
            public class MissingType {}
            """);
        var dependencyCompilation = Compilation.Create(
            "dependency",
            [dependencyTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var dependencyStream = new MemoryStream();
        var dependencyEmit = dependencyCompilation.Emit(dependencyStream);
        Assert.True(dependencyEmit.Success, string.Join(Environment.NewLine, dependencyEmit.Diagnostics));

        var dependencyDirectory = Path.Combine(Path.GetTempPath(), $"raven-metadata-dependency-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dependencyDirectory);
        var dependencyPath = Path.Combine(dependencyDirectory, "dependency.dll");
        File.WriteAllBytes(dependencyPath, dependencyStream.ToArray());

        var hostTree = SyntaxTree.ParseText("""
            public class Host {
                public static func M(value: MissingType) -> unit {}
                public static func N() -> unit {}
            }
            """);
        var hostCompilation = Compilation.Create(
            "host",
            [hostTree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(dependencyPath)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var hostStream = new MemoryStream();
        var hostEmit = hostCompilation.Emit(hostStream);
        Assert.True(hostEmit.Success, string.Join(Environment.NewLine, hostEmit.Diagnostics));

        var hostDirectory = Path.Combine(Path.GetTempPath(), $"raven-metadata-host-{Guid.NewGuid():N}");
        Directory.CreateDirectory(hostDirectory);
        var hostPath = Path.Combine(hostDirectory, "host.dll");
        File.WriteAllBytes(hostPath, hostStream.ToArray());

        try
        {
            var compilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(hostPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var hostType = compilation.GetTypeByMetadataName("Host");
            Assert.NotNull(hostType);

            var unreadable = Assert.Single(hostType!.GetMembers("M").OfType<IMethodSymbol>());
            Assert.Single(unreadable.Parameters);
            Assert.Equal(TypeKind.Error, unreadable.Parameters[0].Type.TypeKind);
            var fallback = Assert.Single(hostType.GetMembers("N").OfType<IMethodSymbol>());
            Assert.Empty(fallback.Parameters);
        }
        finally
        {
            if (Directory.Exists(dependencyDirectory))
                Directory.Delete(dependencyDirectory, recursive: true);

            if (Directory.Exists(hostDirectory))
                Directory.Delete(hostDirectory, recursive: true);
        }
    }

    [Fact]
    public void MetadataReferences_LoadSidecarXmlDocumentation_ForRavenCodeAnalysis()
    {
        var ravenCodeAnalysisPath = typeof(Compilation).Assembly.Location;
        Assert.True(File.Exists(ravenCodeAnalysisPath), $"Expected Raven.CodeAnalysis assembly at '{ravenCodeAnalysisPath}'.");
        Assert.True(
            File.Exists(Path.ChangeExtension(ravenCodeAnalysisPath, ".xml")),
            $"Expected XML documentation next to '{ravenCodeAnalysisPath}'.");

        var compilation = Compilation.Create(
            "consumer",
            syntaxTrees: [],
            references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(ravenCodeAnalysisPath)],
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var syntaxFactory = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.SyntaxFactory");
        Assert.NotNull(syntaxFactory);

        var storedPropertyDeclaration = syntaxFactory!
            .GetMembers("StoredPropertyDeclaration")
            .OfType<IMethodSymbol>()
            .Single();

        var documentation = storedPropertyDeclaration.GetDocumentationComment();
        Assert.NotNull(documentation);
        Assert.Equal(Raven.CodeAnalysis.DocumentationFormat.Xml, documentation!.Format);
        Assert.Contains("stored property", documentation!.Content, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("PropertyDeclaration", documentation.Content, StringComparison.Ordinal);
    }

    [Fact]
    public void MetadataReferences_PreferMarkdownSidecarDocumentation_WhenAvailable()
    {
        var sourceAssemblyPath = typeof(Compilation).Assembly.Location;
        var sourceXmlPath = Path.ChangeExtension(sourceAssemblyPath, ".xml");
        Assert.True(File.Exists(sourceAssemblyPath));
        Assert.True(File.Exists(sourceXmlPath));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-markdown-docs-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        var assemblyPath = Path.Combine(directory, Path.GetFileName(sourceAssemblyPath));
        var xmlPath = Path.Combine(directory, Path.GetFileName(sourceXmlPath));
        File.Copy(sourceAssemblyPath, assemblyPath);
        File.Copy(sourceXmlPath, xmlPath);

        var docsRoot = Path.Combine(directory, "Raven.CodeAnalysis.docs");
        var symbolsRoot = Path.Combine(docsRoot, "invariant", "symbols", "M");
        Directory.CreateDirectory(symbolsRoot);

        var manifest = new
        {
            formatVersion = 1,
            assemblyName = "Raven.CodeAnalysis",
            documentationFormat = "markdown",
            idFormat = "doc-comment-id",
            defaultLocale = "invariant",
            locales = new[] { "invariant" },
            symbolsPath = "symbols"
        };

        File.WriteAllText(
            Path.Combine(docsRoot, "manifest.json"),
            JsonSerializer.Serialize(manifest));

        const string memberId =
            "M:Raven.CodeAnalysis.Syntax.SyntaxFactory.StoredPropertyDeclaration(Raven.CodeAnalysis.Syntax.SyntaxList{Raven.CodeAnalysis.Syntax.AttributeListSyntax},Raven.CodeAnalysis.Syntax.SyntaxTokenList,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.TypeAnnotationClauseSyntax,Raven.CodeAnalysis.Syntax.EqualsValueClauseSyntax)";
        var encodedName = DocumentationCommentIdBuilder.GetMarkdownPathHash(memberId) + ".md";
        File.WriteAllText(
            Path.Combine(symbolsRoot, encodedName),
            """
            ---
            xref: M:Raven.CodeAnalysis.Syntax.SyntaxFactory.StoredPropertyDeclaration(Raven.CodeAnalysis.Syntax.SyntaxList{Raven.CodeAnalysis.Syntax.AttributeListSyntax},Raven.CodeAnalysis.Syntax.SyntaxTokenList,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.TypeAnnotationClauseSyntax,Raven.CodeAnalysis.Syntax.EqualsValueClauseSyntax)
            ---

            # StoredPropertyDeclaration

            Markdown sidecar documentation wins over XML.
            """);

        try
        {
            var compilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var syntaxFactory = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.SyntaxFactory");
            Assert.NotNull(syntaxFactory);

            var storedPropertyDeclaration = syntaxFactory!
                .GetMembers("StoredPropertyDeclaration")
                .OfType<IMethodSymbol>()
                .Single();

            var documentation = storedPropertyDeclaration.GetDocumentationComment();
            Assert.NotNull(documentation);
            Assert.Equal(Raven.CodeAnalysis.DocumentationFormat.Markdown, documentation!.Format);
            Assert.Contains("Markdown sidecar documentation wins over XML.", documentation.Content, StringComparison.Ordinal);
            Assert.DoesNotContain("xref:", documentation.Content, StringComparison.Ordinal);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void MetadataReferences_SkipMarkdownSidecar_WhenFrontMatterXrefDoesNotMatch()
    {
        var sourceAssemblyPath = typeof(Compilation).Assembly.Location;
        var sourceXmlPath = Path.ChangeExtension(sourceAssemblyPath, ".xml");
        Assert.True(File.Exists(sourceAssemblyPath));
        Assert.True(File.Exists(sourceXmlPath));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-markdown-docs-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        var assemblyPath = Path.Combine(directory, Path.GetFileName(sourceAssemblyPath));
        var xmlPath = Path.Combine(directory, Path.GetFileName(sourceXmlPath));
        File.Copy(sourceAssemblyPath, assemblyPath);
        File.Copy(sourceXmlPath, xmlPath);

        var docsRoot = Path.Combine(directory, "Raven.CodeAnalysis.docs");
        var symbolsRoot = Path.Combine(docsRoot, "invariant", "symbols", "M");
        Directory.CreateDirectory(symbolsRoot);

        var manifest = new
        {
            formatVersion = 1,
            assemblyName = "Raven.CodeAnalysis",
            documentationFormat = "markdown",
            idFormat = "doc-comment-id",
            defaultLocale = "invariant",
            locales = new[] { "invariant" },
            symbolsPath = "symbols"
        };

        File.WriteAllText(
            Path.Combine(docsRoot, "manifest.json"),
            JsonSerializer.Serialize(manifest));

        const string memberId =
            "M:Raven.CodeAnalysis.Syntax.SyntaxFactory.StoredPropertyDeclaration(Raven.CodeAnalysis.Syntax.SyntaxList{Raven.CodeAnalysis.Syntax.AttributeListSyntax},Raven.CodeAnalysis.Syntax.SyntaxTokenList,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.TypeAnnotationClauseSyntax,Raven.CodeAnalysis.Syntax.EqualsValueClauseSyntax)";
        var encodedName = DocumentationCommentIdBuilder.GetMarkdownPathHash(memberId) + ".md";
        File.WriteAllText(
            Path.Combine(symbolsRoot, encodedName),
            """
            ---
            xref: M:Raven.CodeAnalysis.Syntax.SyntaxFactory.PropertyDeclaration
            ---

            Wrong symbol documentation.
            """);

        try
        {
            var compilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var syntaxFactory = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.SyntaxFactory");
            Assert.NotNull(syntaxFactory);

            var storedPropertyDeclaration = syntaxFactory!
                .GetMembers("StoredPropertyDeclaration")
                .OfType<IMethodSymbol>()
                .Single();

            var documentation = storedPropertyDeclaration.GetDocumentationComment();
            Assert.NotNull(documentation);
            Assert.Equal(Raven.CodeAnalysis.DocumentationFormat.Xml, documentation!.Format);
            Assert.DoesNotContain("Wrong symbol documentation.", documentation.Content, StringComparison.Ordinal);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void ExternalDocumentationEmitter_EmitsMarkdownSidecar_LoadableFromMetadata()
    {
        var tree = SyntaxTree.ParseText(
            """
/// Creates a widget value.
public class Widget {
    /// Returns the current title.
    public func GetTitle() -> string {
        return "Hello"
    }
}
""",
            new ParseOptions
            {
                DocumentationMode = true,
                DocumentationFormat = DocumentationFormat.Markdown
            });

        var compilation = Compilation.Create(
            "WidgetLibrary",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-emitted-markdown-docs-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        var assemblyPath = Path.Combine(directory, "WidgetLibrary.dll");
        File.WriteAllBytes(assemblyPath, peStream.ToArray());
        ExternalDocumentationEmitter.WriteMarkdownDocumentation(compilation, Path.Combine(directory, "WidgetLibrary.docs"));

        try
        {
            var consumerCompilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var widgetType = consumerCompilation.GetTypeByMetadataName("Widget");
            Assert.NotNull(widgetType);
            Assert.Contains("widget value", widgetType!.GetDocumentationComment()!.Content, StringComparison.OrdinalIgnoreCase);

            var getTitle = Assert.Single(widgetType.GetMembers("GetTitle").OfType<IMethodSymbol>());
            var documentation = getTitle.GetDocumentationComment();
            Assert.NotNull(documentation);
            Assert.Equal(Raven.CodeAnalysis.DocumentationFormat.Markdown, documentation!.Format);
            Assert.Contains("current title", documentation.Content, StringComparison.OrdinalIgnoreCase);

            var docsRoot = Path.Combine(directory, "WidgetLibrary.docs");
            var memberId = "M:Widget.GetTitle";
            var memberPath = Path.Combine(
                docsRoot,
                "invariant",
                "symbols",
                "M",
                DocumentationCommentIdBuilder.GetMarkdownPathHash(memberId) + ".md");
            var emittedMarkdown = File.ReadAllText(memberPath);
            Assert.Contains("---", emittedMarkdown, StringComparison.Ordinal);
            Assert.Contains($"xref: {memberId}", emittedMarkdown, StringComparison.Ordinal);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }
}
