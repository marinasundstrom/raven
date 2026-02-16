using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;

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
    public void SealedHierarchy_IsClosed_IsFalse_And_IsAbstract_IsTrue()
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
        Assert.True(symbol.IsAbstract);
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
        var alc = new AssemblyLoadContext("SealedNotILSealed", isCollectible: true);
        try
        {
            var assembly = alc.LoadFromStream(stream);
            var exprType = assembly.GetType("Expr");
            Assert.NotNull(exprType);
            Assert.False(exprType!.IsSealed);
            Assert.True(exprType.IsAbstract);
        }
        finally
        {
            alc.Unload();
        }
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
        var alc = new AssemblyLoadContext("SealedClosedHierarchyAttr", isCollectible: true);
        try
        {
            var assembly = alc.LoadFromStream(stream);
            var exprType = assembly.GetType("Expr");
            Assert.NotNull(exprType);

            var attributes = exprType!.GetCustomAttributesData();
            var closedAttr = attributes.FirstOrDefault(a =>
                a.AttributeType.FullName == "System.Runtime.CompilerServices.ClosedHierarchyAttribute");
            Assert.NotNull(closedAttr);
        }
        finally
        {
            alc.Unload();
        }
    }

    // ── Sealed record classes ──

    [Fact]
    public void Parse_SealedRecordClass_WithPermitsClause()
    {
        var source = """
sealed record class Expr permits Lit, Add {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var recordDecl = Assert.IsType<RecordDeclarationSyntax>(root.Members[0]);
        Assert.NotNull(recordDecl.PermitsClause);
        Assert.Equal(2, recordDecl.PermitsClause!.Types.Count);
    }

    [Fact]
    public void Parse_SealedRecordClass_WithoutPermitsClause()
    {
        var source = """
sealed record class Expr {}
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var recordDecl = Assert.IsType<RecordDeclarationSyntax>(root.Members[0]);
        Assert.Null(recordDecl.PermitsClause);
    }

    [Fact]
    public void SealedRecordHierarchy_SameFile_DerivationSucceeds()
    {
        var source = """
sealed record class Expr {}
record class Lit : Expr {}
record class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void SealedRecordHierarchy_DifferentFile_ProducesRAV0334()
    {
        var tree1 = SyntaxTree.ParseText("sealed record class Expr {}", path: "file1.rvn");
        var tree2 = SyntaxTree.ParseText("record class Derived : Expr {}", path: "file2.rvn");
        var compilation = CreateCompilation([tree1, tree2], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0334");
    }

    [Fact]
    public void SealedRecordHierarchy_Permits_AllowsListedType()
    {
        var source = """
sealed record class Expr permits Lit {}
record class Lit : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
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
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0335");
    }

    [Fact]
    public void SealedRecordHierarchy_SymbolProperties()
    {
        var source = """
sealed record class Expr {}
record class Lit : Expr {}
record class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var recordDecl = (RecordDeclarationSyntax)root.Members[0];
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(recordDecl));

        Assert.False(symbol.IsClosed);
        Assert.True(symbol.IsSealedHierarchy);
        Assert.True(symbol.IsAbstract);
        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Lit");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Add");
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
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV0338");
    }

    // ── Body-less (marker) type declarations ──

    [Fact]
    public void SealedHierarchy_BodylessDeclarations_Succeed()
    {
        var source = """
sealed class Expr
class Lit : Expr
class Add : Expr
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
        Assert.True(symbol.IsSealedHierarchy);
        Assert.True(symbol.IsAbstract);
        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
    }

    [Fact]
    public void BodylessClass_ParsesSuccessfully()
    {
        var source = """
class Marker
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var classDecl = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.Equal("Marker", classDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessStruct_ParsesSuccessfully()
    {
        var source = """
struct Unit
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var structDecl = Assert.IsType<StructDeclarationSyntax>(root.Members[0]);
        Assert.Equal("Unit", structDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessInterface_ParsesSuccessfully()
    {
        var source = """
interface IMarker
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var ifaceDecl = Assert.IsType<InterfaceDeclarationSyntax>(root.Members[0]);
        Assert.Equal("IMarker", ifaceDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessRecordClass_ParsesSuccessfully()
    {
        var source = """
record class Event
""";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var recordDecl = Assert.IsType<RecordDeclarationSyntax>(root.Members[0]);
        Assert.Equal("Event", recordDecl.Identifier.Text);
    }

    [Fact]
    public void BodylessSealedClass_WithPermits_Succeeds()
    {
        var source = """
sealed class Expr permits Lit, Add
class Lit : Expr
class Add : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void BodylessSealedRecordClass_WithPermits_Succeeds()
    {
        var source = """
sealed record class Node permits Leaf, Branch
record class Leaf : Node
record class Branch : Node
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    // ── Match exhaustiveness ──

    [Fact]
    public void SealedHierarchy_Match_ExhaustiveWhenAllSubtypesCovered()
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
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_NotExhaustiveWhenSubtypeMissing()
    {
        var source = """
import System.*
import System.Console.*

val expr: Expr = Lit()

val result = expr match {
    Lit lit => 1
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_ExhaustiveWithCatchAll()
    {
        var source = """
import System.*
import System.Console.*

val expr: Expr = Lit()

match expr {
    Lit lit => WriteLine(lit)
    _ => WriteLine("other")
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_SemanticModelReportsExhaustive()
    {
        var source = """
import System.*

val expr: Expr = Lit()

val result = expr match {
    Lit lit => 1
    Add add => 2
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var matchExpr = root.DescendantNodes().OfType<MatchExpressionSyntax>().First();
        var info = model.GetMatchExhaustiveness(matchExpr);
        Assert.True(info.IsExhaustive, $"Expected exhaustive match but missing: [{string.Join(", ", info.MissingCases)}]");
        Assert.True(info.MissingCases.IsDefaultOrEmpty);
    }

    [Fact]
    public void SealedHierarchy_Match_SemanticModelReportsMissingCases()
    {
        var source = """
import System.*

val expr: Expr = Lit()

val result = expr match {
    Lit _ => 1
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var matchExpr = root.DescendantNodes().OfType<MatchExpressionSyntax>().First();
        var info = model.GetMatchExhaustiveness(matchExpr);
        Assert.False(info.IsExhaustive);
        Assert.Contains("Add", info.MissingCases);
    }

    [Fact]
    public void SealedHierarchy_Match_WithPermits_ExhaustiveWhenAllPermittedCovered()
    {
        var source = """
import System.*
import System.Console.*

val expr: Expr = Lit()

match expr {
    Lit lit => WriteLine(lit)
    Add add => WriteLine(add)
}

sealed class Expr permits Lit, Add {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedRecordHierarchy_Match_ExhaustiveWhenAllSubtypesCovered()
    {
        var source = """
import System.*
import System.Console.*

val expr: Expr = Lit()

match expr {
    Lit lit => WriteLine(lit)
    Add add => WriteLine(add)
}

sealed record class Expr {}
record class Lit : Expr {}
record class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    // ── Record (deconstruct) pattern exhaustiveness ──

    [Fact]
    public void SealedHierarchy_Match_DeconstructPattern_ExhaustiveWhenAllCovered()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_DeconstructPattern_NotExhaustiveWhenMissing()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Lit(val value) => value
}

sealed record Expr
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_MixedPatterns_ExhaustiveWhenAllCovered()
    {
        var source = """
import System.*
import System.Console.*

val expr: Expr = Lit(42)

match expr {
    Lit(val value) => WriteLine(value)
    Add add => WriteLine(add)
}

sealed record Expr
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_DeconstructPattern_SemanticModelReportsExhaustive()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var matchExpr = root.DescendantNodes().OfType<MatchExpressionSyntax>().First();
        var info = model.GetMatchExhaustiveness(matchExpr);
        Assert.True(info.IsExhaustive, $"Expected exhaustive match but missing: [{string.Join(", ", info.MissingCases)}]");
    }

    [Fact]
    public void SealedHierarchy_Match_DeconstructPattern_SemanticModelReportsMissing()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Add(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var matchExpr = root.DescendantNodes().OfType<MatchExpressionSyntax>().First();
        var info = model.GetMatchExhaustiveness(matchExpr);
        Assert.False(info.IsExhaustive);
        Assert.Contains("Lit", info.MissingCases);
    }

    [Fact]
    public void SealedRecordHierarchy_Match_DeconstructPattern_WithPermits_Exhaustive()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
}

sealed record Expr permits Lit, Add
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }
}
