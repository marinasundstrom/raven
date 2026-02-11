using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class TopLevelGlobalStatementTests : CompilationTestBase
{
    [Fact]
    public void GlobalStatements_CanReferenceTopLevelTypes()
    {
        const string source = """
val greeter: IGreeter = Greeter();
greeter.Greet();
val shade = Shade.Green;

interface IGreeter {
    public Greet() -> unit;
};

class Greeter : IGreeter {
    public Greet() -> unit => ();
};

enum Shade {
    Red,
    Green,
};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var classDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var interfaceDeclaration = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var enumDeclaration = root.DescendantNodes().OfType<EnumDeclarationSyntax>().Single();

        var classSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var interfaceSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(interfaceDeclaration));
        var enumSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(enumDeclaration));

        var greeterCreation = root
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Greeter");
        var creationType = model.GetTypeInfo(greeterCreation);
        Assert.Same(classSymbol, creationType.Type);

        var annotationType = root
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.ValueText == "IGreeter");
        var annotatedInfo = model.GetTypeInfo(annotationType);
        Assert.Same(interfaceSymbol, annotatedInfo.Type);

        var enumAccess = root
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Shade");
        var enumMember = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(enumAccess).Symbol);
        Assert.Same(enumSymbol, enumMember.ContainingType);
    }

    [Fact]
    public void EmptyTopLevelProgram_EmitsEmptyMain()
    {
        var tree = SyntaxTree.ParseText(string.Empty);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));

        var entryPoint = compilation.GetEntryPoint();
        Assert.NotNull(entryPoint);
        Assert.Equal("Main", entryPoint!.Name);
        Assert.Equal("Program", entryPoint.ContainingType?.Name);
        Assert.Equal(SpecialType.System_Unit, entryPoint.ReturnType.SpecialType);
    }

    [Fact]
    public void TopLevelFunctionMain_IsEntryPoint()
    {
        const string source = """
func Main() -> int => 0
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var entryPoint = compilation.GetEntryPoint();
        var mainSymbol = Assert.IsAssignableFrom<IMethodSymbol>(entryPoint);

        Assert.False(mainSymbol.IsImplicitlyDeclared);
        Assert.Equal("Program", mainSymbol.ContainingType?.Name);
    }

    [Fact]
    public void TopLevelFunctionMain_CanCoexistWithOtherMembers()
    {
        const string source = """
func Helper() -> unit => ();

func Main(args: string[]) -> unit {
    Helper();
}

namespace Utility
{
    class Widget { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var entryPoint = compilation.GetEntryPoint();
        var mainSymbol = Assert.IsAssignableFrom<IMethodSymbol>(entryPoint);

        Assert.False(mainSymbol.IsImplicitlyDeclared);
        Assert.Equal("Program", mainSymbol.ContainingType?.Name);
    }

    [Fact]
    public void TopLevelFunctionMain_RejectsAdditionalGlobalStatements()
    {
        const string source = """
import System.Console.*

val x = 2

func Main() -> unit {
    WriteLine("Hello World");
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TopLevelStatementsDisallowedWithMainFunction);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
    }

    [Fact]
    public void GlobalStatement_AfterTypeDeclaration_ReportsOutOfOrderDiagnostic()
    {
        const string source = """
class Widget { }

val x = 1
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedCodeOutOfOrder);
    }

    [Fact]
    public void FileScopedNamespace_GlobalStatement_BeforeTypeDeclaration_IsAllowed()
    {
        const string source = """
namespace App;

val x = 1

class Widget { }
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedCodeOutOfOrder);
    }

    [Fact]
    public void FileScopedNamespace_GlobalStatement_AfterTypeDeclaration_ReportsOutOfOrderDiagnostic()
    {
        const string source = """
namespace App;

class Widget { }

val x = 1
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, assemblyName: "app");

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedCodeOutOfOrder);
    }
}
