using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class ClosedHierarchyMetadataTests
{
    [Fact]
    public void MetadataHierarchy_ImportsPermittedDirectSubtypes()
    {
        var reference = TestMetadataFactory.CreateFromSource(
            """
            public sealed abstract class Expr permits Lit, Add {}
            public class Lit : Expr {}
            public class Add : Expr {}
            """,
            $"ClosedHierarchy.Metadata.{Guid.NewGuid():N}");
        var compilation = CreateMetadataConsumer(reference);

        var expression = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Expr"));

        Assert.True(expression.IsSealedHierarchy);
        Assert.Equal(
            ["Lit", "Add"],
            expression.PermittedDirectSubtypes.Select(type => type.Name));
    }

    [Fact]
    public void MetadataHierarchy_ParticipatesInMatchExhaustiveness()
    {
        var reference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
            public sealed abstract class Expr permits Lit, Add {}
            public class Lit : Expr {}
            public class Add : Expr {}
            """,
            $"ClosedHierarchy.Exhaustiveness.{Guid.NewGuid():N}");
        var tree = SyntaxTree.ParseText(
            """
            func Evaluate(expr: Expr) -> int {
                return match expr {
                    Lit => 1
                }
            }
            """);
        var compilation = Compilation.Create(
                "ClosedHierarchy.Consumer",
                [tree],
                [.. TestMetadataReferences.Default, reference],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
                          diagnostic.Descriptor != CompilerDiagnostics.MatchExpressionNotExhaustive);
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive &&
                          diagnostic.GetMessage().Contains("Add", StringComparison.Ordinal));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var exhaustiveness = model.GetMatchExhaustiveness(match);

        Assert.False(exhaustiveness.IsExhaustive);
        Assert.Contains("Add", exhaustiveness.MissingCases);
    }

    [Fact]
    public void SyntaxApi_ImportsGeneratedClosedHierarchiesAcrossSyntaxFamilies()
    {
        var reference = MetadataReference.CreateFromFile(typeof(ExpressionSyntax).Assembly.Location);
        var compilation = CreateMetadataConsumer(reference);

        var syntaxNode = GetType("SyntaxNode");
        var expression = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.ExpressionSyntax"));
        var statement = GetType("StatementSyntax");

        Assert.True(syntaxNode.IsSealedHierarchy);
        Assert.Contains(
            syntaxNode.PermittedDirectSubtypes,
            type => type.Name == nameof(StructuredTriviaSyntax));
        Assert.True(expression.IsSealedHierarchy);
        Assert.Contains(
            expression.PermittedDirectSubtypes,
            type => type.Name == nameof(LiteralExpressionSyntax));
        Assert.Contains(
            expression.PermittedDirectSubtypes,
            type => type.Name == "Missing" &&
                    type.ContainingType?.Name == nameof(ExpressionSyntax));

        Assert.True(statement.IsSealedHierarchy);
        Assert.Contains(
            statement.PermittedDirectSubtypes,
            type => type.Name == nameof(ReturnStatementSyntax));

        Assert.All(
            new[]
            {
                "PatternSyntax",
                "TypeSyntax",
                "NameSyntax",
                "MemberDeclarationSyntax",
                "BaseMethodDeclarationSyntax",
                "VariableDesignationSyntax",
                "CollectionElementSyntax",
                "ObjectInitializerEntrySyntax",
                "StructuredTriviaSyntax"
            },
            typeName => Assert.True(
                GetType(typeName).IsSealedHierarchy,
                $"Expected {typeName} to publish closed-hierarchy metadata."));

        INamedTypeSymbol GetType(string name) =>
            Assert.IsAssignableFrom<INamedTypeSymbol>(
                compilation.GetTypeByMetadataName($"Raven.CodeAnalysis.Syntax.{name}"));
    }

    [Fact]
    public void SyntaxApi_MatchReportsUnhandledExpressionKinds()
    {
        var reference = MetadataReference.CreateFromFile(typeof(ExpressionSyntax).Assembly.Location);
        var tree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Syntax.*

            func Describe(expression: ExpressionSyntax) -> string {
                return match expression {
                    LiteralExpressionSyntax => "literal"
                }
            }
            """);
        var compilation = Compilation.Create(
                "SyntaxApi.Exhaustiveness",
                [tree],
                [.. TestMetadataReferences.Default, reference],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var exhaustiveness = model.GetMatchExhaustiveness(match);

        Assert.False(exhaustiveness.IsExhaustive);
        Assert.Contains(
            exhaustiveness.MissingCases,
            missingCase => missingCase.Contains(
                nameof(AssignmentExpressionSyntax),
                StringComparison.Ordinal));
        Assert.Contains(
            exhaustiveness.MissingCases,
            missingCase => string.Equals(
                missingCase,
                "Missing",
                StringComparison.Ordinal));
    }

    private static Compilation CreateMetadataConsumer(MetadataReference reference)
        => Compilation.Create(
            "ClosedHierarchy.MetadataConsumer",
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(reference);
}
