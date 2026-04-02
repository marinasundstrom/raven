using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticModelDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void GetDiagnostics_CollectsMethodBodyDiagnostics()
    {
        var source = """
class Test {
    func M() {
        1();
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        var model = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = model.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidInvocation);
    }

    [Fact]
    public void GetDiagnostics_IncompleteStatement_DoesNotCrashBinder()
    {
        const string source = """
func Main() {
    if true {
        )
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        var model = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = compilation.GetDiagnostics();
        _ = diagnostics.Length;

        var incompleteStatement = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IncompleteStatementSyntax>()
            .Single();

        Assert.IsType<BoundExpressionStatement>(model.GetBoundNode(incompleteStatement));
    }

    [Fact]
    public void GetDiagnostics_MalformedInvocationInMatchArm_DoesNotCrashAndReportsMissingParen()
    {
        const string source = """
import System.*
import System.Console.*

func Main() -> () {
    val x = 1
    match x {
        2 => WriteLine(("Yes")
        _ => WriteLine(("No")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        _ = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = compilation.GetDiagnostics();
        _ = diagnostics.Length;

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor.Id == CompilerDiagnostics.CharacterExpected.Id
                          && diagnostic.GetMessage().Contains("')' expected", System.StringComparison.Ordinal));
    }

    [Fact]
    public void GetDiagnostics_ParsedInvalidLocalInterfaceDeclaration_ReportsSingleDiagnostic()
    {
        const string source = """
class Test {
    func M() {
        interface Helper {}
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Descriptor.Id == "RAV7002")
            .ToArray();

        var diagnostic = Assert.Single(diagnostics);
        Assert.Contains(
            "Only class, struct, record, and enum declarations are valid local type declarations",
            diagnostic.GetMessage(),
            System.StringComparison.Ordinal);
    }

    [Fact]
    public void GetDiagnostics_InvalidLocalTypeDeclarationStatement_DoesNotCrashBinder()
    {
        const string source = """
class Test {
    func M() {
        class Helper {}
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var root = syntaxTree.GetRoot();
        var localTypeStatement = root.DescendantNodes().OfType<TypeDeclarationStatementSyntax>().Single();

        var malformedStatement = SyntaxFactory.TypeDeclarationStatement(
            SyntaxFactory.InterfaceDeclaration(
                SyntaxFactory.List<AttributeListSyntax>(),
                SyntaxFactory.TokenList(),
                SyntaxFactory.InterfaceKeyword,
                SyntaxFactory.Identifier("Helper"),
                SyntaxFactory.List<TypeParameterConstraintClauseSyntax>(),
                SyntaxFactory.List<MemberDeclarationSyntax>()));

        var updatedRoot = (CompilationUnitSyntax)root.ReplaceNode(localTypeStatement, malformedStatement);
        var updatedTree = SyntaxTree.Create(updatedRoot);
        var compilation = CreateCompilation(updatedTree);
        var model = compilation.GetSemanticModel(updatedTree);

        var diagnostics = compilation.GetDiagnostics();
        _ = diagnostics.Length;

        var updatedStatement = updatedTree.GetRoot().DescendantNodes().OfType<TypeDeclarationStatementSyntax>().Single();
        var updatedInterface = updatedTree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor.Id == "RAV7002"
                          && diagnostic.GetMessage().Contains("Only class, struct, record, and enum declarations are valid local type declarations", System.StringComparison.Ordinal));
        Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(updatedInterface));
        Assert.IsType<BoundExpressionStatement>(model.GetBoundNode(updatedStatement));
    }
}
