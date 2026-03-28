using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SealedHierarchyTests : CompilationTestBase
{
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
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
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
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    // ── Permits validation ──

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
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

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
        Assert.Empty(compilation.GetDiagnostics());

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
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var classDecl = root.Members.OfType<ClassDeclarationSyntax>().First();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDecl));

        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Lit");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Add");

    }

    // ── Sealed record classes ──

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
        Assert.Empty(compilation.GetDiagnostics());
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
        Assert.Empty(compilation.GetDiagnostics());
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
        Assert.Empty(compilation.GetDiagnostics());

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
    public void SealedInterface_SameFileImplementors_AreDiscovered()
    {
        var source = """
sealed interface HttpResponse {}
class Success : HttpResponse {}
class NotFound : HttpResponse {}
sealed interface Nested : HttpResponse {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var interfaceDecl = Assert.IsType<InterfaceDeclarationSyntax>(root.Members[0]);
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(interfaceDecl));

        Assert.True(symbol.IsSealedHierarchy);
        Assert.Equal(3, symbol.PermittedDirectSubtypes.Length);
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Success");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "NotFound");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Nested");
    }

    [Fact]
    public void SealedInterface_Permits_AllowsListedTypes()
    {
        var source = """
sealed interface HttpResponse permits Success, Nested {}
class Success : HttpResponse {}
interface Nested : HttpResponse {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void SealedInterface_NestedTypes_AreDiscovered()
    {
        var source = """
sealed interface HttpResponse {
    record class Success(status: int) : HttpResponse {}
    record class NotFound(message: string) : HttpResponse {}
}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var interfaceDecl = Assert.IsType<InterfaceDeclarationSyntax>(root.Members[0]);
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(interfaceDecl));

        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "Success");
        Assert.Contains(symbol.PermittedDirectSubtypes, t => t.Name == "NotFound");
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
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var classDecl = (ClassDeclarationSyntax)root.Members[0];
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDecl));
        Assert.True(symbol.IsSealedHierarchy);
        Assert.True(symbol.IsAbstract);
        Assert.Equal(2, symbol.PermittedDirectSubtypes.Length);
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
        Assert.Empty(compilation.GetDiagnostics());
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
        Assert.Empty(compilation.GetDiagnostics());
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
    public void SealedHierarchy_Match_ExhaustiveWithAbstractIntermediateSubtype()
    {
        var source = """
import System.*

val expr: Expr = Lit()

val result = expr match {
    Lit lit => 1
}

sealed class Expr {}
sealed abstract class Node : Expr {}
class Lit : Node {}
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
    public void SealedHierarchy_MatchExpression_FullyCoveredCatchAll_IsCurrentlyNotRedundant()
    {
        var source = """
import System.*

val expr: Expr = Lit()

val result = expr match {
    Lit lit => 1
    Add add => 2
    _ => 0
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2103");
    }

    [Fact]
    public void SealedHierarchy_MatchStatement_FullyCoveredCatchAll_IsRedundant()
    {
        var source = """
import System.*

func Evaluate(expr: Expr) -> int {
    match expr {
        Lit lit => 1
        Add add => 2
        _ => 0
    }
}

sealed class Expr {}
class Lit : Expr {}
class Add : Expr {}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2103");
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
    public void SealedHierarchy_Match_DeconstructPatternWithValueConstraint_NotExhaustive()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Lit(0) => 0
    Add(val left, val right) => 1
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
    public void SealedHierarchy_Match_DeconstructPatternWithValueConstraint_CatchAllNotRedundant()
    {
        var source = """
import System.*

val expr: Expr = Lit(42)

val result = expr match {
    Lit(0) => 0
    Add(val left, val right) => 1
    _ => -1
}

sealed record Expr
record Lit(Value: int) : Expr
record Add(Left: Expr, Right: Expr) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2103");
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2110");
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

    [Fact]
    public void SealedHierarchy_Match_OpenIntermediate_LeafCasesAreNotExhaustive()
    {
        var source = """
import System.*

val expr: Expr = Add(Lit(1), Lit(2))

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
    Sub(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2100" && d.GetMessage().Contains("BinaryExpr", StringComparison.Ordinal));
    }

    [Fact]
    public void SealedHierarchy_Match_OpenIntermediate_ParentCaseIsExhaustive()
    {
        var source = """
import System.*

val expr: Expr = Add(Lit(1), Lit(2))

val result = expr match {
    Lit(val value) => value
    BinaryExpr(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_SealedIntermediate_LeafCasesAreExhaustive()
    {
        var source = """
import System.*

val expr: Expr = Add(Lit(1), Lit(2))

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
    Sub(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
sealed abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void SealedHierarchy_Match_OpenIntermediate_SemanticModelReportsParentCaseMissing()
    {
        var source = """
import System.*

val expr: Expr = Add(Lit(1), Lit(2))

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
    Sub(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var matchExpr = root.DescendantNodes().OfType<MatchExpressionSyntax>().First();
        var info = model.GetMatchExhaustiveness(matchExpr);
        Assert.False(info.IsExhaustive);
        Assert.Contains("BinaryExpr", info.MissingCases);
    }

    [Fact]
    public void SealedInterface_GenericNestedCases_AreIncludedInMatchExhaustiveness()
    {
        var source = """
import System.*

func Evaluate<T>(expr: Expr<T>) {
    match expr {
    }
}

sealed interface Expr<T> {
    record NumericalExpr(Value: float) : Expr<float>
    record StringExpr(Value: string) : Expr<string>
    record AddExpr(Left: Expr<float>, Right: Expr<float>) : Expr<float>
}
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2100" && d.GetMessage().Contains("NumericalExpr", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2100" && d.GetMessage().Contains("StringExpr", StringComparison.Ordinal));
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV2100" && d.GetMessage().Contains("AddExpr", StringComparison.Ordinal));
    }

    // ── Record inheritance with primary constructor forwarding ──

    [Fact]
    public void RecordInheritance_PrimaryConstructorBaseCall_BindsWithoutErrors()
    {
        var source = """
sealed record Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Lit(Value: int) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordInheritance_PrimaryConstructorBaseCall_ParametersResolveable()
    {
        var source = """
import System.*

val expr: Expr = Add(Lit(1), Lit(2))

val result = expr match {
    Lit(val v) => v
    Add(val left, val right) => 0
    Sub(val left, val right) => 0
    BinaryExpr(val left, val right) => 0
}

sealed record Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Lit(Value: int) : Expr
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordInheritance_PrimaryConstructorBaseCall_BaseConstructorInitializerIsSet()
    {
        var source = """
sealed record Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""";
        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);

        var addType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Add"));
        var addCtor = Assert.IsAssignableFrom<IMethodSymbol>(
            addType.Constructors.Single(c => !c.IsStatic && c.Parameters.Length == 2));

        Assert.Equal("BinaryExpr", addCtor.ContainingType.BaseType?.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
