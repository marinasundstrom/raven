using System;
using System.IO;
using System.Security.Cryptography;
using System.Linq;
using System.Text;
using System.Text.Json;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MetadataReferenceResolutionTests
{
    [Fact]
    public void AddReferences_AppendsToExistingReferences()
    {
        var ravenCodeAnalysisPath = typeof(Compilation).Assembly.Location;
        var compilation = Compilation.Create(
                "consumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(MetadataReference.CreateFromFile(ravenCodeAnalysisPath));

        Assert.NotNull(compilation.GetTypeByMetadataName("System.String"));
        Assert.NotNull(compilation.GetTypeByMetadataName(
            "Raven.CodeAnalysis.Syntax.SyntaxFactory"));
    }

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

let value = Observer.Ping()
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

let message = $"Hello"
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
    public void GetDiagnostics_IgnoresNativePortableExecutableReferences()
    {
        var directory = Path.Combine(Path.GetTempPath(), $"raven-native-reference-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);
        var nativeReferencePath = Path.Combine(directory, "native-host.dll");
        File.WriteAllBytes(nativeReferencePath, "not a managed assembly"u8.ToArray());

        try
        {
            var tree = SyntaxTree.ParseText("public func Value() -> int => 42");
            var nativeReference = MetadataReference.CreateFromFile(nativeReferencePath);
            var compilation = Compilation.Create(
                "consumer",
                [tree],
                [.. TestMetadataReferences.Default, nativeReference],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var diagnostics = compilation.GetDiagnostics();

            Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
            Assert.Null(compilation.GetAssemblyOrModuleSymbol(nativeReference));
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
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
            var unreadableParameter = unreadable.Parameters[0];
            Assert.Equal(TypeKind.Error, unreadableParameter.Type.TypeKind);
            Assert.Same(unreadableParameter.Type, unreadableParameter.Type);
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
    public void MetadataReferences_ReloadDocumentation_WhenMarkdownSidecarIsAdded()
    {
        var sourceAssemblyPath = typeof(Compilation).Assembly.Location;
        var sourceXmlPath = Path.ChangeExtension(sourceAssemblyPath, ".xml");
        Assert.True(File.Exists(sourceAssemblyPath));
        Assert.True(File.Exists(sourceXmlPath));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-markdown-docs-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        var assemblyPath = Path.Combine(directory, Path.GetFileName(sourceAssemblyPath));
        File.Copy(sourceAssemblyPath, assemblyPath);
        File.Copy(sourceXmlPath, Path.Combine(directory, Path.GetFileName(sourceXmlPath)));

        const string memberId =
            "M:Raven.CodeAnalysis.Syntax.SyntaxFactory.StoredPropertyDeclaration(Raven.CodeAnalysis.Syntax.SyntaxList{Raven.CodeAnalysis.Syntax.AttributeListSyntax},Raven.CodeAnalysis.Syntax.SyntaxTokenList,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.SyntaxToken,Raven.CodeAnalysis.Syntax.TypeAnnotationClauseSyntax,Raven.CodeAnalysis.Syntax.EqualsValueClauseSyntax)";

        DocumentationComment GetDocumentation()
        {
            var compilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var syntaxFactory = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.SyntaxFactory");
            var method = Assert.Single(syntaxFactory!
                .GetMembers("StoredPropertyDeclaration")
                .OfType<IMethodSymbol>());
            return Assert.IsType<DocumentationComment>(method.GetDocumentationComment());
        }

        try
        {
            Assert.Equal(DocumentationFormat.Xml, GetDocumentation().Format);

            var docsRoot = Path.Combine(directory, "Raven.CodeAnalysis.docs");
            var symbolsRoot = Path.Combine(docsRoot, "invariant", "symbols", "M");
            Directory.CreateDirectory(symbolsRoot);
            File.WriteAllText(
                Path.Combine(docsRoot, "manifest.json"),
                JsonSerializer.Serialize(new
                {
                    formatVersion = 1,
                    assemblyName = "Raven.CodeAnalysis",
                    documentationFormat = "markdown",
                    idFormat = "doc-comment-id",
                    defaultLocale = "invariant",
                    locales = new[] { "invariant" },
                    symbolsPath = "symbols"
                }));
            File.WriteAllText(
                Path.Combine(symbolsRoot, DocumentationCommentIdBuilder.GetMarkdownPathHash(memberId) + ".md"),
                $$"""
                ---
                xref: {{memberId}}
                ---

                Documentation loaded from the newly added Markdown sidecar.
                """);

            var documentation = GetDocumentation();
            Assert.Equal(DocumentationFormat.Markdown, documentation.Format);
            Assert.Contains("newly added Markdown sidecar", documentation.Content, StringComparison.Ordinal);
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

    [Theory]
    [InlineData(DocumentationFormat.Xml, false)]
    [InlineData(DocumentationFormat.Markdown, false)]
    [InlineData(DocumentationFormat.Xml, true)]
    [InlineData(DocumentationFormat.Markdown, true)]
    public void ExternalDocumentationSidecars_LoadForNonGenericAndGenericExtensionDefinitions(
        DocumentationFormat format,
        bool genericExtension)
    {
        var source = genericExtension
            ? """
                public class Box<T> {}

                public extension BoxExtensions<T> for Box<T> {
                    /// Returns the supplied text from a generic extension.
                    public func Echo(text: string) -> string => text
                }
                """
            : """
                /// Represents a simple widget.
                public class Widget {
                    /// Returns the current title from a non-generic definition.
                    public func GetTitle() -> string => "Hello"
                }
                """;
        var tree = SyntaxTree.ParseText(
            source,
            new ParseOptions
            {
                DocumentationMode = true,
                DocumentationFormat = DocumentationFormat.Markdown
            });
        var assemblyName = genericExtension
            ? "GenericExtensionDocumentationLibrary"
            : "NonGenericDocumentationLibrary";
        var compilation = Compilation.Create(
            assemblyName,
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-sidecar-matrix-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);
        var assemblyPath = Path.Combine(directory, assemblyName + ".dll");
        File.WriteAllBytes(assemblyPath, peStream.ToArray());

        if (format == DocumentationFormat.Markdown)
        {
            ExternalDocumentationEmitter.WriteMarkdownDocumentation(
                compilation,
                Path.Combine(directory, assemblyName + ".docs"));
        }
        else
        {
            ExternalDocumentationEmitter.WriteXmlDocumentation(
                compilation,
                Path.Combine(directory, assemblyName + ".xml"));
        }

        try
        {
            var consumerCompilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            var type = consumerCompilation.GetTypeByMetadataName(
                genericExtension ? "BoxExtensions" : "Widget");
            Assert.NotNull(type);

            var method = Assert.Single(type!
                .GetMembers(genericExtension ? "Echo" : "GetTitle")
                .OfType<IMethodSymbol>());
            var documentation = method.GetDocumentationComment();

            Assert.NotNull(documentation);
            Assert.Equal(format, documentation!.Format);
            Assert.Contains(
                genericExtension ? "generic extension" : "non-generic definition",
                documentation.Content,
                StringComparison.OrdinalIgnoreCase);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void ConstructedMetadataSymbols_PreserveMarkdownDocumentation()
    {
        var tree = SyntaxTree.ParseText(
            """
/// Represents a generic box.
public class Box<T> {
    /// Echoes a value of the requested type.
    public func Echo<U>(value: U) -> U => value
}

/// Represents a generic outcome.
public union Outcome<T, E> {
    case Success(T)
    case Failure(E)
}
""",
            new ParseOptions
            {
                DocumentationMode = true,
                DocumentationFormat = DocumentationFormat.Markdown
            });

        var compilation = Compilation.Create(
            "GenericDocumentationLibrary",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-constructed-markdown-docs-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        var assemblyPath = Path.Combine(directory, "GenericDocumentationLibrary.dll");
        File.WriteAllBytes(assemblyPath, peStream.ToArray());
        ExternalDocumentationEmitter.WriteMarkdownDocumentation(
            compilation,
            Path.Combine(directory, "GenericDocumentationLibrary.docs"));

        try
        {
            var consumerCompilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var boxDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
                consumerCompilation.GetTypeByMetadataName("Box`1"));
            var box = Assert.IsAssignableFrom<INamedTypeSymbol>(
                boxDefinition.Construct(consumerCompilation.GetSpecialType(SpecialType.System_Int32)));

            var boxDocumentation = box.GetDocumentationComment();
            Assert.NotNull(boxDocumentation);
            Assert.Equal(DocumentationFormat.Markdown, boxDocumentation!.Format);
            Assert.Contains("generic box", boxDocumentation.Content, StringComparison.OrdinalIgnoreCase);

            var echoDefinition = Assert.Single(box.GetMembers("Echo").OfType<IMethodSymbol>());
            Assert.Contains(
                "Echoes a value",
                echoDefinition.GetDocumentationComment()!.Content,
                StringComparison.Ordinal);

            var echo = echoDefinition.Construct(
                consumerCompilation.GetSpecialType(SpecialType.System_String));
            Assert.Contains(
                "Echoes a value",
                echo.GetDocumentationComment()!.Content,
                StringComparison.Ordinal);

            var outcomeDefinition = Assert.IsAssignableFrom<IUnionSymbol>(
                consumerCompilation.GetTypeByMetadataName("Outcome`2"));
            var outcome = Assert.IsAssignableFrom<IUnionSymbol>(
                outcomeDefinition.Construct(
                    consumerCompilation.GetSpecialType(SpecialType.System_Int32),
                    consumerCompilation.GetSpecialType(SpecialType.System_String)));
            Assert.Contains(
                "generic outcome",
                outcome.GetDocumentationComment()!.Content,
                StringComparison.OrdinalIgnoreCase);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void ExternalDocumentationEmitter_LoadsUnionCaseDocumentationByLogicalName()
    {
        var tree = SyntaxTree.ParseText(
            """
/// Represents a choice.
public union Choice {
    /// Carries an integer value.
    case Value(value: int)

    /// Represents no value.
    case None
}
""",
            new ParseOptions
            {
                DocumentationMode = true,
                DocumentationFormat = DocumentationFormat.Markdown
            });

        var compilation = Compilation.Create(
            "ChoiceLibrary",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        using var pdbStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream, pdbStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var directory = Path.Combine(Path.GetTempPath(), $"raven-emitted-union-docs-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        var assemblyPath = Path.Combine(directory, "ChoiceLibrary.dll");
        File.WriteAllBytes(assemblyPath, peStream.ToArray());
        ExternalDocumentationEmitter.WriteMarkdownDocumentation(compilation, Path.Combine(directory, "ChoiceLibrary.docs"));

        try
        {
            var consumerCompilation = Compilation.Create(
                "consumer",
                syntaxTrees: [],
                references: [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(assemblyPath)],
                options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            var choice = Assert.IsAssignableFrom<IUnionSymbol>(
                consumerCompilation.GetTypeByMetadataName("Choice"));
            var cases = choice.DeclaredCaseTypes.ToDictionary(@case => @case.Name);

            Assert.Contains("integer value", cases["Value"].GetDocumentationComment()!.Content, StringComparison.OrdinalIgnoreCase);
            Assert.Contains("no value", cases["None"].GetDocumentationComment()!.Content, StringComparison.OrdinalIgnoreCase);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void ExternalDocumentationEmitter_EmitsMarkdownForMacro()
    {
        var tree = SyntaxTree.ParseText(
            """
namespace Example

/// Expands the supplied token tree.
///
/// ## Parameters
/// - `context`: The token tree context to expand.
public macro Expand(context: Raven.CodeAnalysis.Macros.TokenTreeMacroContext) {
    expand Raven.CodeAnalysis.Macros.FreestandingMacroExpansionResult.Empty
}
""",
            new ParseOptions
            {
                DocumentationMode = true,
                DocumentationFormat = DocumentationFormat.Markdown
            });
        var assemblyInfoTree = SyntaxTree.ParseText(
            """
import Raven.CodeAnalysis.Macros.*
[assembly: RavenCompilerPlugin]
""");

        var compilation = Compilation.Create(
                "MacroLibrary",
                [],
                [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(Compilation).Assembly.Location)],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTreesWithLocalMacros(assemblyInfoTree, tree);
        var directory = Path.Combine(Path.GetTempPath(), $"raven-emitted-macro-markdown-docs-{Guid.NewGuid():N}");

        try
        {
            var docsRoot = ExternalDocumentationEmitter.WriteMarkdownDocumentation(
                compilation,
                Path.Combine(directory, "MacroLibrary.docs"));
            var memberId = "M:Example.Expand(Raven.CodeAnalysis.Macros.TokenTreeMacroContext)";
            var memberPath = Assert.Single(Directory.GetFiles(
                Path.Combine(docsRoot, "invariant", "symbols", "M"),
                "*.md"));

            var emittedMarkdown = File.ReadAllText(memberPath);
            Assert.Contains($"xref: {memberId}", emittedMarkdown, StringComparison.Ordinal);
            Assert.Contains("Expands the supplied token tree.", emittedMarkdown, StringComparison.Ordinal);
            Assert.Contains("The token tree context to expand.", emittedMarkdown, StringComparison.Ordinal);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }
}
