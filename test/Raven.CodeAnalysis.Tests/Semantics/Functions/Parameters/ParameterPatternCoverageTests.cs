using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ParameterPatternCoverageTests : CompilationTestBase
{
    [Theory]
    [InlineData("int[]", "[head, ..tail]", "head")]
    [InlineData("int[]", "[x, y]", "x + y")]
    [InlineData("int[]", "[]", "0")]
    [InlineData("(int, int[])", "(id, [head, ..tail])", "id + head")]
    public void RefutableLambdaParameter_ReportsError(string inputType, string pattern, string body)
    {
        var (compilation, tree) = CreateCompilation($"let f: ({inputType}) -> int = ({pattern}) => {body}");

        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        var error = Assert.Single(errors);
        Assert.Equal(CompilerDiagnostics.RefutableParameterPattern.Id, error.Id);
        Assert.Equal(tree.GetRoot().DescendantNodes().OfType<SequencePatternSyntax>().Single().Span, error.Location.SourceSpan);
    }

    [Theory]
    [InlineData("int[]", "[..items]", "items.Length")]
    [InlineData("int[]", "[...items]", "items.Length")]
    [InlineData("int[2]", "[x, y]", "x + y")]
    [InlineData("int[3]", "[head, ..tail]", "head + tail.Length")]
    [InlineData("int[0]", "[]", "0")]
    [InlineData("(int, int[2])", "(id, [x, y])", "id + x + y")]
    [InlineData("(int, int[])", "(id, [..items])", "id + items.Length")]
    [InlineData("string", "[..text]", "text.Length")]
    public void IrrefutableLambdaParameter_AcceptsInputShape(string inputType, string pattern, string body)
    {
        var (compilation, _) = CreateCompilation($"let f: ({inputType}) -> int = ({pattern}) => {body}");

        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void RecordDeconstruction_ReportsNestedRefutablePattern()
    {
        var (compilation, tree) = CreateCompilation("""
record class Row(Id: int, Items: int[])
let f: (Row) -> int = ((id, [head, ..tail])) => id + head
""");

        var error = Assert.Single(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.RefutableParameterPattern.Id, error.Id);
        Assert.Equal(tree.GetRoot().DescendantNodes().OfType<SequencePatternSyntax>().Single().Span, error.Location.SourceSpan);
    }

    [Fact]
    public void ColdSymbolQuery_DoesNotLoseOrDuplicateCoverageDiagnostic()
    {
        var (compilation, tree) = CreateCompilation("let f: (int[]) -> int = ([head, ..tail]) => head");
        var model = compilation.GetSemanticModel(tree);
        var use = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Last(n => n.Identifier.ValueText == "head");

        Assert.NotNull(model.GetSymbolInfo(use).Symbol);
        Assert.Single(compilation.GetDiagnostics().Where(d => d.Id == CompilerDiagnostics.RefutableParameterPattern.Id));
        Assert.Single(compilation.GetDiagnostics().Where(d => d.Id == CompilerDiagnostics.RefutableParameterPattern.Id));
    }

    [Fact]
    public void InvalidInputType_DoesNotAddCoverageDiagnostic()
    {
        var (compilation, _) = CreateCompilation("let f: (Missing) -> int = ([head, ..tail]) => 0");

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(diagnostics, d => d.Id == CompilerDiagnostics.RefutableParameterPattern.Id);
    }
}
