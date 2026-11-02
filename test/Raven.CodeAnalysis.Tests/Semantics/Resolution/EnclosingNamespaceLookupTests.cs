using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class EnclosingNamespaceLookupTests
{
    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, true, false)]
    [InlineData(true, false, false)]
    [InlineData(true, true, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, true)]
    [InlineData(true, false, true)]
    [InlineData(true, true, true)]
    public void DottedNamespace_FindsEnclosingSourceType(bool fileScoped, bool reverseTrees, bool diagnosticsFirst)
    {
        var parent = SyntaxTree.ParseText("namespace Outer { open class Root { } }");
        var body = "class Child : Root { }\nclass Probe { static func Make() -> Root => Root() }";
        var consumer = SyntaxTree.ParseText(fileScoped ? "namespace Outer.Inner.Deep\n" + body : "namespace Outer.Inner.Deep { " + body + " }");
        var compilation = Create(reverseTrees ? [consumer, parent] : [parent, consumer]);
        if (diagnosticsFirst)
            AssertNoErrors(compilation);
        var child = consumer.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(n => n.Identifier.ValueText == "Child");
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(consumer).GetDeclaredSymbol(child));
        Assert.Equal("Outer.Root", type.BaseType!.ToFullyQualifiedMetadataName());
        Assert.Same(compilation.Assembly, type.BaseType.ContainingAssembly);
        AssertNoErrors(compilation);
    }

    [Theory]
    [InlineData("""
        open class Root { }
        namespace Outer { open class Root { } }
        namespace Outer.Inner { open class Root { } }
        namespace Outer.Inner.Deep { class Child : Root { } }
        """, "Outer.Inner.Root")]
    [InlineData("""
        open class Root { }
        namespace Imported { open class Root { } }
        namespace Outer { open class Root { } }
        namespace Outer.Inner { import Imported.*; class Child : Root { } }
        """, "Imported.Root")]
    [InlineData("""
        open class Root { }
        namespace Imported { open class Root { } }
        namespace Outer { import Imported.*; namespace Inner.Deep { class Child : Root { } } }
        """, "Imported.Root")]
    [InlineData("""
        namespace Imported { open class Root { } }
        namespace Outer.Inner { import Imported.*; open class Root { }
        class Child : Root { } }
        """, "Outer.Inner.Root")]
    public void NamespaceLookup_PreservesLexicalTypeAndImportPrecedence(string source, string expected)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Create([tree]);
        AssertNoErrors(compilation);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(n => n.Identifier.ValueText == "Child");
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.Equal(expected, type.BaseType!.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void EnclosingMetadataNamespace_ResolvesFrameworkTypeAndEmitsIt()
    {
        var tree = SyntaxTree.ParseText("""
            namespace System.Collections.Generic
            class Probe { static func Read() -> Type => typeof(int) }
            """);
        var compilation = Create([tree]);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var method = loaded.Assembly.GetType("System.Collections.Generic.Probe")!.GetMethod("Read")!;
        Assert.Equal(typeof(Type), method.ReturnType);
        Assert.Equal(typeof(int), method.Invoke(null, null));
    }

    [Fact]
    public void RelativeQualifiedName_FindsNamespaceWithinEnclosingScope()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Outer.Sibling { class Value { } }
            namespace Outer.Inner { class Probe { static func Make() -> Sibling.Value => Sibling.Value() } }
            """);
        var compilation = Create([tree]);
        AssertNoErrors(compilation);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(method));
        Assert.Equal("Outer.Sibling.Value", symbol.ReturnType.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void QualifiedNamespaceSegments_DoNotSearchTheOwnersEnclosingScopes()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Outer.Sibling { class Value { } }
            namespace Outer.Inner { class Probe { static func Make() -> Outer.Inner.Sibling.Value => Outer.Inner.Sibling.Value() } }
            """);
        Assert.Contains(Create([tree]).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.GetMessage().Contains("Sibling"));
    }

    [Fact]
    public void EnclosingSourceType_TracksWorkspaceEditsWithoutChangingOlderSnapshots()
    {
        var workspace = new AdhocWorkspace();
        var project = workspace.CurrentSolution.AddProject("enclosing", compilationOptions:
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)).Projects.Single();
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        var parent = project.AddDocument("parent.rvn", SourceText.From("namespace Outer { open class Root { } }"));
        var consumer = parent.Project.AddDocument("consumer.rvn", SourceText.From("namespace Outer.Inner { class Child : Root { } }"));
        Assert.True(workspace.TryApplyChanges(consumer.Project.Solution));
        var before = workspace.GetCompilation(consumer.Project.Id);
        AssertNoErrors(before);

        Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(parent.Id,
            SourceText.From("namespace Outer { open class Renamed { } }"))));
        var missing = workspace.GetCompilation(consumer.Project.Id);
        Assert.Contains(missing.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.GetMessage().Contains("Root"));

        Assert.True(workspace.TryApplyChanges(workspace.CurrentSolution.WithDocumentText(parent.Id,
            SourceText.From("namespace Outer { open class Root { val Marker: int => 42 } }"))));
        var restored = workspace.GetCompilation(consumer.Project.Id);
        AssertNoErrors(restored);
        Assert.NotSame(before.GetTypeByMetadataName("Outer.Root"), restored.GetTypeByMetadataName("Outer.Root"));
        Assert.Single(restored.GetTypeByMetadataName("Outer.Root")!.GetMembers("Marker"));
        Assert.Equal("Outer.Root", restored.GetTypeByMetadataName("Outer.Inner.Child")!.BaseType!.ToFullyQualifiedMetadataName());
        AssertNoErrors(before);
    }

    private static Compilation Create(SyntaxTree[] trees) => Compilation.Create("EnclosingNamespace", trees,
        TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    private static void AssertNoErrors(Compilation compilation) => Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
}
