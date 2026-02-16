using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SealedHierarchyTests : CompilationTestBase
{
    // ── Syntax ──

    [Fact]
    public void Parse_SealedClass_WithPermitsClause()
    {
        var source = """
sealed class Expr permits Lit, Add {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var classDecl = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.NotNull(classDecl.PermitsClause);
        Assert.Equal(2, classDecl.PermitsClause!.Types.Count);
    }

    [Fact]
    public void Parse_SealedClass_WithoutPermitsClause()
    {
        var source = """
sealed class Expr {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var classDecl = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.Null(classDecl.PermitsClause);
    }

    // ── Same-file closure ──

    [Fact]
    public void SealedHierarchy_SameFile_DerivationSucceeds()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void SealedHierarchy_DifferentFile_ProducesRAV0334()
    {
        var tree1 = SyntaxTree.ParseText("sealed class Expr {}", path: "file1.rvn");
        var tree2 = SyntaxTree.ParseText("class Derived : Expr {}", path: "file2.rvn");
        var compilation = CreateCompilation([tree1, tree2], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0334");
    }

    // ── Permits exclusivity ──

    [Fact]
    public void SealedHierarchy_Permits_AllowsListedType()
    {
        var source = """
sealed class Expr permits Lit {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
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
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0335");
    }

    // ── Permits validation ──

    [Fact]
    public void Permits_DuplicateType_ProducesRAV0337()
    {
        var source = """
sealed class Expr permits Lit, Lit {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0337");
    }

    [Fact]
    public void Permits_UnknownType_ProducesDiagnostic()
    {
        var source = """
sealed class Expr permits DoesNotExist {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        // RAV0103 "not in scope" is reported by the general name resolver
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0103");
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
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0339");
    }

    // ── Modifier combinations ──

    [Fact]
    public void SealedAbstractClass_IsValid()
    {
        var source = """
sealed abstract class Expr {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var classDecl = (ClassDeclarationSyntax)root.Members[0];
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDecl));
        Assert.True(symbol.IsAbstract);
        Assert.False(symbol.IsClosed);
    }

    // ── Symbol properties ──

    [Fact]
    public void SealedHierarchy_IsClosed_IsFalse()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var classDecl = (ClassDeclarationSyntax)root.Members[0];
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDecl));

        Assert.False(symbol.IsClosed);
        Assert.True(symbol.IsSealedHierarchy);
    }

    [Fact]
    public void SealedHierarchy_PermittedDirectSubtypes_AreDiscovered()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var classDecl = (ClassDeclarationSyntax)root.Members[0];
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDecl));

        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Lit");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Add");
    }

    [Fact]
    public void SealedHierarchy_PermittedDirectSubtypes_AreDiscovered2()
    {
        var source = """
import System.*
import System.Console.*

val expr: Expr = Lit()

match expr {
    Lit lit => WriteLine(lit)
    Add add => WriteLine(add)
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        using var stream = new MemoryStream();
        //var result = compilation.Emit(stream);
        //Assert.True(result.Success);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var classDecl = root.Members.OfType<ClassDeclarationSyntax>().First();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDecl));

        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Lit");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Add");

        var diagnostics = compilation.GetDiagnostics();
    }

    // ── Permits type not a direct subtype ──

    [Fact]
    public void Permits_TypeNotDirectSubtype_ProducesRAV0338()
    {
        var source = """
sealed class Expr permits Other {}
class Other {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0338");
    }

    // ── IL Emission ──

    [Fact]
    public void SealedHierarchy_EmittedType_IsNotILSealed()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        stream.Position = 0;
        var assembly = System.Runtime.Loader.AssemblyLoadContext.Default.LoadFromStream(stream);
        var exprType = assembly.GetType("Expr");
        Assert.NotNull(exprType);
        Assert.False(exprType!.IsSealed);
    }

    [Fact]
    public void SealedHierarchy_EmitsClosedHierarchyAttribute()
    {
        var source = """
sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        stream.Position = 0;
        var assembly = System.Runtime.Loader.AssemblyLoadContext.Default.LoadFromStream(stream);
        var exprType = assembly.GetType("Expr");
        Assert.NotNull(exprType);

        var attributes = exprType!.GetCustomAttributesData();
        var closedAttr = attributes.FirstOrDefault(a =>
            a.AttributeType.FullName == "System.Runtime.CompilerServices.ClosedHierarchyAttribute");
        Assert.NotNull(closedAttr);
    }
}
