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
    public void SealedHierarchy_MatchStatement_FullyCoveredCatchAll_IsCurrentlyNotRedundant()
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
        Assert.DoesNotContain(diagnostics, d => d.Descriptor.Id == "RAV2103");
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
}
