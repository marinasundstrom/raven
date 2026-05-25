using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class CompilationSymbolLookupTests : CompilationTestBase
{
    [Fact]
    public void GetTypeByMetadataName_PrefersSourceTypeOverReferencedMetadataType()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
namespace Shared {
    class Widget { }
}
""",
            assemblyName: "Shared.Metadata");

        var sourceTree = SyntaxTree.ParseText(
            """
namespace Shared {
    class Widget { }
}
""");

        var references = TestMetadataReferences.Default.Append(metadataReference).ToArray();
        var compilation = CreateCompilation(sourceTree, references: references);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(sourceTree);
        var declaration = sourceTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var sourceType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        var resolved = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Shared.Widget"));

        Assert.Equal(sourceType, resolved, SymbolEqualityComparer.Default);
        Assert.Equal(compilation.Assembly, resolved.ContainingAssembly);
    }

    [Fact]
    public void MetadataOnlyLookup_BypassesSourcePrecedence()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
namespace Shared {
    class Widget { }
}
""",
            assemblyName: "Shared.Metadata");

        var sourceTree = SyntaxTree.ParseText(
            """
namespace Shared {
    class Widget { }
}
""");

        var references = TestMetadataReferences.Default.Append(metadataReference).ToArray();
        var compilation = CreateCompilation(sourceTree, references: references);
        compilation.EnsureSetup();

        var metadataType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SymbolLookup.GetTypeByMetadataNameMetadataOnly("Shared.Widget"));

        Assert.Equal("Shared.Metadata", metadataType.ContainingAssembly?.Name);
    }

    [Fact]
    public void NamespaceQualifiedMetadataTypeLookup_SeesTypesAcrossReferencedNamespaceParts()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
func Main() {
    val year = System.DateTime.Now.Year
}
""");

        var compilation = CreateCompilation(sourceTree);

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var systemNamespace = Assert.IsAssignableFrom<INamespaceSymbol>(
            compilation.SymbolLookup.LookupNamespaceSourceFirst(null, "System"));
        var dateTime = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SymbolLookup.LookupTypeSourceFirst(systemNamespace, "DateTime"));

        Assert.Equal("DateTime", dateTime.Name);
        Assert.Equal("System", dateTime.ContainingNamespace?.ToDisplayString());
    }

    [Fact]
    public void SourceExtensionLookup_ByName_DoesNotForceSourceDeclarationCompletion()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);

        var sourceTree = SyntaxTree.ParseText(
            """
namespace Shared

class Widget { }

extension WidgetExtensions for Widget {
    func Mark() -> int => 1
}
""");

        var compilation = CreateCompilation(sourceTree, options);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(sourceTree);
        var widgetDeclaration = sourceTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var widgetType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(widgetDeclaration));

        var setupBefore = instrumentation.Setup.CaptureSnapshot();
        var result = model.LookupApplicableExtensionMembers(
            widgetType,
            name: "Mark",
            kinds: ExtensionMemberKinds.InstanceMethods);
        var setupDelta = CompilerSetupInstrumentation.Subtract(
            instrumentation.Setup.CaptureSnapshot(),
            setupBefore);

        var method = Assert.Single(result.InstanceMethods);
        Assert.Equal("Mark", method.Name);
        Assert.Equal(0, setupDelta.EnsureSourceDeclarationsCompleteCalls);
    }

    [Fact]
    public void SourceExtensionLookup_DoesNotCacheIncompleteResultsBeforeDeclarationsAreComplete()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
namespace Shared

class Widget { }

extension WidgetExtensions for Widget {
    func First() -> int => 1

    func Second() -> int {
        val value = self.First()
        return value
    }
}
""");

        var compilation = CreateCompilation(sourceTree);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }
}
