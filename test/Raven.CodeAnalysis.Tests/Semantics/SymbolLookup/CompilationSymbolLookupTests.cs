using System;
using System.Linq;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class CompilationSymbolLookupTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    [Trait("Target", "DotNet")]
    public void SourceEnumConstantsWinOverBootstrapMetadataInDefaultsAndCollections(bool reverseTrees, bool queryFirst)
    {
        const string enumSource = "namespace Shared; public enum Kind { None = 0, Plus = 1 }";
        var bootstrap = TestMetadataFactory.CreateFileReferenceFromSource(enumSource, "Bootstrap.Enum");
        var enumTree = SyntaxTree.ParseText(enumSource);
        var consumerTree = SyntaxTree.ParseText("""
            namespace Shared
            import System.Collections.Generic.*
            public class Node {
                init(kind: Kind = Kind.None) { }
                static val Values: HashSet<Kind> = HashSet<Kind>([Kind.None, Kind.Plus])
                static func Create() -> Node => Node()
            }
            """);
        var trees = reverseTrees ? new[] { enumTree, consumerTree } : new[] { consumerTree, enumTree };
        var compilation = Compilation.Create("EnumPort", trees, TestMetadataReferences.Default.Append(bootstrap).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(consumerTree);
        if (!queryFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        foreach (var member in consumerTree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>())
        {
            var field = Assert.IsAssignableFrom<FieldSymbol>(model.GetSymbolInfo(member).Symbol);
            Assert.Same(compilation.Assembly, field.ContainingAssembly);
            Assert.True(field.IsConst);
        }
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var node = compilation.GetTypeByMetadataName("Shared.Node")!;
        var parameter = Assert.Single(Assert.Single(node.Constructors).Parameters);
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(0, parameter.ExplicitDefaultValue);
    }

    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    [InlineData(true, true, true)]
    [Trait("Target", "Shared")]
    public void CompilerPortAliases_PreserveSourceOwnershipInOverrideSignatures(
        bool reverseTrees, bool fileScoped, bool sameName)
    {
        var alias = sameName ? "GreenNode" : "G";
        var baseBody = "abstract class GreenNode { internal abstract func With(children: GreenNode?[]) -> GreenNode }";
        var derivedBody = $"alias {alias} = Raven.CodeAnalysis.Syntax.GreenNode\n" +
            $"class PortProbe : {alias} {{ internal override func With(children: {alias}?[]) -> {alias} => self }}";
        string Namespace(string name, string body) => fileScoped
            ? $"namespace {name}\n{body}"
            : $"namespace {name} {{\n{body}\n}}";
        var source = SyntaxTree.ParseText(Namespace("Raven.CodeAnalysis.Syntax", baseBody));
        var consumer = SyntaxTree.ParseText(Namespace("Raven.CodeAnalysis.Syntax.InternalSyntax", derivedBody));
        var references = TestMetadataReferences.Default.Append(
            MetadataReference.CreateFromFile(typeof(Raven.CodeAnalysis.Syntax.GreenNode).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("PortSourceOwnership", reverseTrees ? [consumer, source] : [source, consumer],
            references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var declaration = consumer.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(consumer).GetDeclaredSymbol(declaration));
        Assert.Same(compilation.Assembly, method.ReturnType.ContainingAssembly);
        var children = Assert.IsAssignableFrom<ArrayTypeSymbol>(method.Parameters[0].Type);
        Assert.Same(compilation.Assembly, children.ElementType.GetNonNullableType().ContainingAssembly);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(method.IsOverride);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void QualifiedSourceBaseWinsOverBootstrapMetadataAcrossFiles(bool reverseTrees, bool useAlias)
    {
        const string definition = """
            namespace Shared
            public open class Node {
                internal field marker: int = 7
            }
            """;
        var metadata = TestMetadataFactory.CreateFileReferenceFromSource(definition, assemblyName: "Bootstrap.Node");
        var source = SyntaxTree.ParseText(definition, path: "node.rvn");
        var consumer = SyntaxTree.ParseText("namespace Client\n" +
            (useAlias ? "alias Base = Shared.Node\n" : "") +
            "class Derived : " + (useAlias ? "Base" : "Shared.Node") +
            " { func Read() -> int => marker }", path: "derived.rvn");
        var compilation = Compilation.Create("SourceBase", reverseTrees ? [source, consumer] : [consumer, source],
            TestMetadataReferences.Default.Append(metadata).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var derived = Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetSemanticModel(consumer).GetDeclaredSymbol(
            consumer.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single()));
        Assert.Same(compilation.Assembly, derived.BaseType!.ContainingAssembly);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false, "", "Shared.Api")]
    [InlineData(true, "", "Shared.Api")]
    [InlineData(false, "import Shared.*", "Api")]
    [InlineData(true, "import Shared.*", "Api")]
    [InlineData(false, "import Shared.Api", "Api")]
    [InlineData(true, "import Shared.Api", "Api")]
    public void NamespaceMemberLookupUsesCanonicalMetadataType(bool reverseReferences, string imports, string receiver)
    {
        var bootstrap = TestMetadataFactory.CreateFileReferenceFromSource("""
            namespace Shared {
                public static class Api {
                    public static func Legacy() -> int => 1
                }
            }
            """, assemblyName: "Bootstrap");
        var library = TestMetadataFactory.CreateFileReferenceFromSource("""
            namespace Shared {
                public static class Api {
                    public static func Value() -> int => 42
                }
            }
            """, assemblyName: "Shared.Library");
        var references = TestMetadataReferences.Default.Concat(reverseReferences
            ? new[] { library, bootstrap } : new[] { bootstrap, library }).ToArray();
        for (var i = 0; i < 8; i++)
        {
            var tree = SyntaxTree.ParseText($"{imports}\nfunc Main() -> int => {receiver}.Value()", path: $"lookup-{i}.rvn");
            var compilation = CreateCompilation(tree, references: references);
            var expected = compilation.GetTypeByMetadataName("Shared.Api");
            Assert.Equal("Shared.Library", expected?.ContainingAssembly?.Name);
            Assert.Null(compilation.Assembly.GetTypeByMetadataName("Shared.Api"));
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
            var api = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
                .Last(n => n.Identifier.ValueText == "Api");
            Assert.Equal(expected, compilation.GetSemanticModel(tree).GetSymbolInfo(api).Symbol, SymbolEqualityComparer.Default);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SourceNamespaceImportsDoNotRetainMetadataTypesCachedDuringDeclarations(bool reverseDeclarations)
    {
        const string queue = """
            namespace Shared {
                public class Queue {
                    internal static val Current: Queue => Queue()
                }
            }
            """;
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(queue, assemblyName: "Bootstrap.Queue");
        const string consumer = """
            namespace Consumer {
                import Shared.*
                public class Builder<T> {
                    init(queue: Queue) { }
                    static func Create() -> Builder<T> => Builder<T>(Queue.Current)
                }
            }
            """;
        var tree = SyntaxTree.ParseText(reverseDeclarations ? consumer + "\n" + queue : queue + "\n" + consumer);
        var compilation = CreateCompilation(tree, references: TestMetadataReferences.Default.Append(metadataReference).ToArray());
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var receiver = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>()
            .Single(n => n.ToString() == "Queue.Current").Expression;
        var type = Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(receiver).Symbol);
        Assert.Same(compilation.Assembly, type.ContainingAssembly);
        Assert.Same(type, compilation.GetTypeByMetadataName("Shared.Queue"));
    }

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
        var sourceType = Assert.IsAssignableFrom<NamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        var resolved = Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetTypeByMetadataName("Shared.Widget"));

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

        var metadataType = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.SymbolLookup.GetTypeByMetadataNameMetadataOnly("Shared.Widget"));

        Assert.Equal("Shared.Metadata", metadataType.ContainingAssembly?.Name);
    }

    [Fact]
    public void NamespaceQualifiedMetadataTypeLookup_SeesTypesAcrossReferencedNamespaceParts()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
func Main() {
    let year = System.DateTime.Now.Year
}
""");

        var compilation = CreateCompilation(sourceTree);

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var systemNamespace = Assert.IsAssignableFrom<NamespaceSymbol>(
            compilation.SymbolLookup.LookupNamespaceSourceFirst(null, "System"));
        var dateTime = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.SymbolLookup.LookupTypeSourceFirst(systemNamespace, "DateTime"));

        Assert.Equal("DateTime", dateTime.Name);
        Assert.Equal("System", dateTime.ContainingNamespace?.ToDisplayString());
    }

    [Fact]
    public void SimpleNameLookup_FindsNestedMetadataTypeAndCachesTheResult()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
namespace Shared {
    class Container {
        public class Nested { }
    }
}
""",
            assemblyName: "Shared.Metadata");

        var compilation = CreateCompilation(references: [.. TestMetadataReferences.Default, metadataReference]);
        compilation.EnsureSetup();
        compilation.EnsureSourceDeclarationsDeclared();

        var first = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.SymbolLookup.GetTypeBySimpleName("Nested", 0));
        var second = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.SymbolLookup.GetTypeBySimpleName("Nested", 0));

        Assert.Same(first, second);
        Assert.Equal("Container", first.ContainingType?.Name);
        Assert.Equal("Shared.Metadata", first.ContainingAssembly?.Name);
        var importedAssembly = Assert.IsAssignableFrom<ImportedAssemblySymbol>(first.ContainingAssembly);
        Assert.Same(first, importedAssembly.GetTypeBySimpleName("Nested", 0));
        Assert.Null(importedAssembly.GetTypeBySimpleName("Nested", 1));
    }

    [Fact]
    public void SimpleNameLookup_RespectsArityAcrossSourceAndImportedTypes()
    {
        var compilation = CreateCompilation(SyntaxTree.ParseText("class List { }"));
        compilation.EnsureSetup();
        compilation.EnsureSourceDeclarationsDeclared();

        var source = compilation.SymbolLookup.GetTypeBySimpleName("List", 0);
        var imported = compilation.SymbolLookup.GetTypeBySimpleName("List", 1);

        Assert.NotNull(source);
        Assert.Equal(compilation.Assembly, source.ContainingAssembly);
        Assert.NotNull(imported);
        Assert.Equal(compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"),
            imported, SymbolEqualityComparer.Default);
        Assert.Same(imported, compilation.SymbolLookup.GetTypeBySimpleName("List", 1));
        Assert.Null(compilation.SymbolLookup.GetTypeBySimpleName("List", 3));
    }

    [Fact]
    public void QualifiedNamespaceSegmentSymbolInfo_DoesNotBindTheContainingBody()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var sourceTree = SyntaxTree.ParseText(
            """
func ReadNow() {
    let year = System.DateTime.Now.Year
}
""");
        var compilation = CreateCompilation(sourceTree, options);
        var model = compilation.GetSemanticModel(sourceTree);
        var systemIdentifier = sourceTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "System");
        var before = instrumentation.SemanticQuery.CaptureSnapshot();

        var info = model.GetSymbolInfo(systemIdentifier);
        var delta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            before);

        var systemNamespace = Assert.IsAssignableFrom<NamespaceSymbol>(info.Symbol);
        Assert.False(systemNamespace.IsGlobalNamespace);
        Assert.Equal("System", systemNamespace.ToDisplayString());
        Assert.Equal(0, delta.BoundNodeBindFallbacks);
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
        var widgetType = Assert.IsAssignableFrom<NamedTypeSymbol>(model.GetDeclaredSymbol(widgetDeclaration));

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
        let value = self.First()
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
