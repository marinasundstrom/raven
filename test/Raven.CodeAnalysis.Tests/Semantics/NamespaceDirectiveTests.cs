using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NamespaceDirectiveTests
{
    [Fact]
    public void FileScopedNamespaceDirective_AppliesToSynthesizedProgram()
    {
        const string source = """
        namespace Samples

        System.Console.WriteLine("hi");
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var programType = compilation.GetTypeByMetadataName("Samples.Program");
        Assert.NotNull(programType);

        var containingNamespace = programType!.ContainingNamespace;
        Assert.NotNull(containingNamespace);
        Assert.Equal("Samples", containingNamespace!.ToString());
    }

    [Fact]
    public void NamespaceDeclaration_BindsMembersInDeclaredNamespace()
    {
        const string source = """
        namespace Outer
        {
            namespace Inner
            {
                class C {}
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var type = compilation.GetTypeByMetadataName("Outer.Inner.C");
        Assert.NotNull(type);
        Assert.Equal("Outer.Inner", type!.ContainingNamespace?.ToString());
    }

    [Fact]
    public void FileScopedNamespaceDirective_GlobalStatementsSeeNamespaceMembers()
    {
        const string source = """
        namespace Samples

        let person = Person()

        class Person
        {
            init () {}
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);
    }

    [Fact]
    public void FileScopedNamespaceDirective_GlobalStatementsUseAliasesAndNamespaceTypes()
    {
        const string source = """
        namespace Samples

        alias PrintLine = System.Console.WriteLine

        let person = Person()
        PrintLine("hi")

        open class Base {}

        class Person : Base
        {
            init () {}

            public AddRole(role: string) -> Person
            {
                self
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);
    }

    [Fact]
    public void FileScopedNamespaceDirective_OpenModifierAttachesToType()
    {
        const string source = """
        namespace Samples

        open class Person {}
        """;

        var tree = SyntaxTree.ParseText(source);
        var root = (CompilationUnitSyntax)tree.GetRoot();

        var ns = Assert.Single(root.Members.OfType<FileScopedNamespaceDeclarationSyntax>());
        var person = Assert.Single(ns.Members.OfType<ClassDeclarationSyntax>());

        Assert.Contains(person.Modifiers, modifier => modifier.Kind == SyntaxKind.OpenKeyword);
    }

    [Fact]
    public void CompilationOptionsRootNamespace_AppliesToSynthesizedProgram()
    {
        const string source = "System.Console.WriteLine(\"hi\");";

        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.ConsoleApplication, rootNamespace: "Samples.App");
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            options);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        Assert.NotNull(compilation.GetTypeByMetadataName("Samples.App.Program"));
        Assert.Null(compilation.GetTypeByMetadataName("Program"));
    }

    [Fact]
    public void NamespaceDeclaration_WithoutQualifier_RespectsRootNamespace()
    {
        const string source = """
        namespace Services
        {
            class C {}
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.ConsoleApplication, rootNamespace: "Samples.App");
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            options);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var type = compilation.GetTypeByMetadataName("Samples.App.Services.C");
        Assert.NotNull(type);
    }

    [Fact]
    public void NamespaceDeclaration_WithRootPrefix_IsNotDuplicated()
    {
        const string source = """
        namespace Samples.App.Services
        {
            class C {}
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.ConsoleApplication, rootNamespace: "Samples.App");
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            options);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var type = compilation.GetTypeByMetadataName("Samples.App.Services.C");
        Assert.NotNull(type);
    }
}

