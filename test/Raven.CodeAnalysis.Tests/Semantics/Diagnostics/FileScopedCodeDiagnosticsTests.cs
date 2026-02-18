using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FileScopedCodeDiagnosticsTests
{
    [Fact(Skip = "Requires reference assemblies in this environment")]
    public void Library_WithFileScopedCode_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("0");
        var compilation = Compilation.Create("lib", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedCodeRequiresConsole);
    }

    [Fact(Skip = "Requires reference assemblies in this environment")]
    public void MultipleFiles_WithFileScopedCode_ProducesDiagnostic()
    {
        var tree1 = SyntaxTree.ParseText("0");
        var tree2 = SyntaxTree.ParseText("0");
        var compilation = Compilation.Create("app", [tree1, tree2], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedCodeMultipleFiles);
    }

    [Fact(Skip = "Requires reference assemblies in this environment")]
    public void FileScopedCode_AfterDeclaration_ProducesDiagnostic()
    {
        var code = """
struct S {}
0
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("app", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedCodeOutOfOrder);
    }

    [Fact(Skip = "Requires reference assemblies in this environment")]
    public void FileScopedNamespace_AfterGlobalStatement_ProducesDiagnostic()
    {
        var code = """
0
namespace Foo;
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("app", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.FileScopedNamespaceOutOfOrder);
    }
}

