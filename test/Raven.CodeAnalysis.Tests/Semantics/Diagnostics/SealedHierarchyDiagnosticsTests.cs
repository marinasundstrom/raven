using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SealedHierarchyDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void SealedHierarchy_DifferentFile_ProducesRAV0334()
    {
        var tree1 = SyntaxTree.ParseText("sealed class Expr {}", path: "file1.rvn");
        var tree2 = SyntaxTree.ParseText("class Derived : Expr {}", path: "file2.rvn");
        var compilation = CreateCompilation([tree1, tree2], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == CompilerDiagnostics.SealedHierarchyInheritanceDeniedSameFile.Id);
    }

    [Fact]
    public void SealedHierarchy_Permits_RejectsUnlistedType()
    {
        var source = """
sealed class Expr permits Lit {}
class Lit : Expr {}
class NotListed : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == CompilerDiagnostics.SealedHierarchyInheritanceDeniedNotPermitted.Id);
    }

    [Fact]
    public void Permits_DuplicateType_ProducesRAV0337()
    {
        var source = """
sealed class Expr permits Lit, Lit {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0337");
    }

    [Fact]
    public void Permits_UnknownType_ProducesDiagnostic()
    {
        var source = """
sealed class Expr permits DoesNotExist {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id);
    }

    [Fact]
    public void Permits_WithoutSealed_ProducesRAV0339()
    {
        var source = """
open class Expr permits Lit {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0339");
    }

    [Fact]
    public void Permits_TypeNotDirectSubtype_ProducesRAV0338()
    {
        var source = """
sealed class Expr permits Other {}
class Other {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0338");
    }

    [Fact]
    public void SealedRecordHierarchy_DifferentFile_ProducesRAV0334()
    {
        var tree1 = SyntaxTree.ParseText("sealed record class Expr {}", path: "file1.rvn");
        var tree2 = SyntaxTree.ParseText("record class Derived : Expr {}", path: "file2.rvn");
        var compilation = CreateCompilation([tree1, tree2], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == CompilerDiagnostics.SealedHierarchyInheritanceDeniedSameFile.Id);
    }

    [Fact]
    public void SealedRecordHierarchy_Permits_RejectsUnlistedType()
    {
        var source = """
sealed record class Expr permits Lit {}
record class Lit : Expr {}
record class NotListed : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == CompilerDiagnostics.SealedHierarchyInheritanceDeniedNotPermitted.Id);
    }

    [Fact]
    public void SealedRecordHierarchy_Permits_TypeNotDirectSubtype_ProducesRAV0338()
    {
        var source = """
sealed record class Expr permits Other {}
record class Other {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0338");
    }
}
