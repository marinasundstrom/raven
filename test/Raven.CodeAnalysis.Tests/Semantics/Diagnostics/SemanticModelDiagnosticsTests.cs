using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticModelDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void DeletedTokens_AcrossFlowAndPatternSyntax_DoNotCrashSemanticQueries()
    {
        const string source = """
func Evaluate(value: string?) -> int {
    let length = if value is not null { value.Length } else { 0 }
    return match length {
        0 => 1
        _ => length
    }
}
""";

        var originalTree = SyntaxTree.ParseText(source);
        var tokens = originalTree.GetRoot().DescendantTokens()
            .Where(static token => !token.IsMissing && token.Span.Length > 0)
            .GroupBy(static token => token.Kind)
            .SelectMany(static group => group.Where((_, index) => index == 0 || index == group.Count() - 1))
            .ToArray();

        foreach (var token in tokens)
        {
            var mutatedSource = source.Remove(token.Span.Start, token.Span.Length);
            var exception = Record.Exception(() =>
            {
                var tree = SyntaxTree.ParseText(mutatedSource);
                var compilation = CreateCompilation(tree);
                var model = compilation.GetSemanticModel(tree);

                _ = compilation.GetDiagnostics();
                foreach (var expression in tree.GetRoot().DescendantNodes().OfType<ExpressionSyntax>())
                {
                    _ = model.GetTypeInfo(expression);
                    _ = model.GetSymbolInfo(expression);
                }
            });

            Assert.True(
                exception is null,
                $"Deleting token '{token.Text}' ({token.Kind}) caused {exception}");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ObjectInitializer_HasContainingConstructionTypeForSemanticQueries(bool diagnosticsFirst)
    {
        const string source = """
class Widget {
    init() {}
    var Name: string = ""
}

func Create() -> Widget {
    return Widget() {
        Name = "Raven"
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var initializer = tree.GetRoot()
            .DescendantNodes()
            .OfType<ObjectInitializerExpressionSyntax>()
            .Single();
        var typeInfo = model.GetTypeInfo(initializer);
        var symbolInfo = model.GetSymbolInfo(initializer);

        Assert.Equal("Widget", typeInfo.Type?.Name);
        Assert.Null(symbolInfo.Symbol);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ObjectInitializer_AssignmentNameHasPropertySymbolInfo(bool diagnosticsFirst)
    {
        const string source = """
class Widget {
    init() {}
    var Name: string = ""
}

func Create() -> Widget {
    return Widget() {
        Name = "Raven"
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var assignmentName = tree.GetRoot()
            .DescendantNodes()
            .OfType<ObjectInitializerAssignmentEntrySyntax>()
            .Single()
            .Name;

        var property = Assert.IsAssignableFrom<IPropertySymbol>(model.GetSymbolInfo(assignmentName).Symbol);

        Assert.Equal("Name", property.Name);
        Assert.Equal(SpecialType.System_String, property.Type.SpecialType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void WithInitializer_AssignmentNameHasPropertySymbolInfo(bool diagnosticsFirst)
    {
        const string source = """
record Widget(val Name: string)

func Rename(widget: Widget) -> Widget {
    return widget with {
        Name = "Raven"
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var assignmentName = tree.GetRoot()
            .DescendantNodes()
            .OfType<WithAssignmentSyntax>()
            .Single()
            .Name;

        var property = Assert.IsAssignableFrom<IPropertySymbol>(model.GetSymbolInfo(assignmentName).Symbol);

        Assert.Equal("Name", property.Name);
        Assert.Equal(SpecialType.System_String, property.Type.SpecialType);
        Assert.Empty(compilation.GetDiagnostics());
    }

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
    let x = 1
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
    public void GetDiagnostics_PipeOperatorLambdaTarget_DoesNotCrashOnUnreadableMetadataExtensionReceiver()
    {
        const string source = """
import System.Console.*

let increment = (x: int, amount: int) -> int => x + amount
let result = 5 |> increment(2)

WriteLine("Result: $result")
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
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

    [Fact]
    public void GetDiagnostics_UnsupportedConstructedLiteralKind_ProducesErrorExpression()
    {
        var tree = SyntaxTree.ParseText("let value = 1");
        var root = tree.GetRoot();
        var literal = root.DescendantNodes().OfType<LiteralExpressionSyntax>().Single();
        var malformedLiteral = SyntaxFactory.LiteralExpression(SyntaxKind.None, SyntaxFactory.Literal(1));
        var updatedRoot = (CompilationUnitSyntax)root.ReplaceNode(literal, malformedLiteral);
        var updatedTree = SyntaxTree.Create(updatedRoot);
        var compilation = CreateCompilation(updatedTree);
        var model = compilation.GetSemanticModel(updatedTree);
        var updatedLiteral = updatedTree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single();

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.InvalidExpressionTerm);
        Assert.IsType<BoundErrorExpression>(model.GetBoundNode(updatedLiteral));
    }
}
