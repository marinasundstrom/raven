using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NamedParameterPatternTests : CompilationTestBase
{
    [Theory]
    [InlineData("func read(Row(let x): Row) -> int => x")]
    [InlineData("func read(Row(x): Row) -> int => x")]
    [InlineData("class C { func read(Row(let x): Row) -> int => x }")]
    [InlineData("let f: (Row) -> int = (Row(let x): Row) => x")]
    [InlineData("let f: (Row) -> int = Row(let x) => x")]
    [InlineData("func read((Row(let x), _): (Row, int)) -> int => x")]
    public void NominalParameter_DeconstructsInput(string declaration)
    {
        var (compilation, tree) = CreateCompilation("record class Row(Value: int)\n" + declaration);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var use = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().Last(n => n.Identifier.ValueText == "x");
        Assert.IsAssignableFrom<ILocalSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(use).Symbol);
    }

    [Fact]
    public void GenericNominalParameter_BindsConcreteComponentType()
    {
        var (compilation, tree) = CreateCompilation("""
record class Row<T>(Value: T)
func read(Row<int>(let x): Row<int>) -> int => x
""");
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var use = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().Last(n => n.Identifier.ValueText == "x");
        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(use).Symbol);
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
    }

    [Fact]
    public void Select_NominalLambda_InfersInputAndOutput()
    {
        var (compilation, _) = CreateCompilation("""
import System.Linq.*
record class Row(Value: int)
let rows = [Row(1), Row(2)]
let values = rows.Select(Row(let x) => x).ToArray()
""");
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Theory]
    [InlineData("int", "let x")]
    [InlineData("int[]", "[..x]")]
    public void NestedUnsupportedDeconstruction_ReportsContextError(string type, string nestedPattern)
    {
        var (compilation, _) = CreateCompilation($$"""
record class Item(Value: {{type}})
record class Row(Item: Item)
func read(Row({Value: {{nestedPattern}}}): Row) -> unit => ()
""");
        var error = Assert.Single(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.ParameterPatternContextNotSupported.Id, error.Id);
    }

    [Fact]
    public void UnionCaseParameter_ReportsRefutability()
    {
        var (compilation, _) = CreateCompilation("""
union Choice { case Some(value: int); case None }
func read(Choice.Some(let x): Choice) -> int => x
""");
        var error = Assert.Single(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.RefutableParameterPattern.Id, error.Id);
    }

    [Fact]
    public void NestedNominalParameter_ReportsRefutablePayload()
    {
        var (compilation, tree) = CreateCompilation("""
record class Row(Items: int[])
func read(Row([head, ..rest]): Row) -> int => head
""");
        var error = Assert.Single(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.RefutableParameterPattern.Id, error.Id);
        Assert.Equal(tree.GetRoot().DescendantNodes().OfType<SequencePatternSyntax>().Single().Span, error.Location.SourceSpan);
    }

    [Theory]
    [InlineData("Row?", "Row(let x)")]
    [InlineData("object", "Row(let x)")]
    public void NominalParameter_WithBroaderInput_IsRefutable(string inputType, string pattern)
    {
        var (compilation, _) = CreateCompilation($"record class Row(Value: int)\nfunc read({pattern}: {inputType}) -> int => x");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.RefutableParameterPattern.Id);
    }

    [Theory]
    [InlineData("func sum((x, y): (int, int)) -> int { x + y }")]
    [InlineData("func sum((x, y): (int, int)) -> int => x + y")]
    [InlineData("class C { func sum((x, y): (int, int)) -> int { x + y } }")]
    [InlineData("class C { func sum((x, y): (int, int)) -> int => x + y }")]
    [InlineData("class C { func run() -> int { func sum((x, y): (int, int)) -> int => x + y; sum((1, 2)) } }")]
    [InlineData("class C { func sum([x, y]: int[2]) -> int => x + y }")]
    public void NamedPatternParameter_BindsBody(string source)
    {
        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var model = compilation.GetSemanticModel(tree);
        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single(p => p.Pattern is not null);
        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));
        Assert.True(parameter.HasImplicitName);
        Assert.Single(((IMethodSymbol)parameter.ContainingSymbol!).Parameters);
        var use = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().Last(n => n.Identifier.ValueText == "x");
        Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(use).Symbol);
    }

    [Theory]
    [InlineData("(x, y): (int, int), (x, z): (int, int)")]
    [InlineData("x: int, (x, y): (int, int)")]
    [InlineData("(x, x): (int, int)")]
    public void DuplicateParameterBinding_ReportsError(string parameters)
    {
        var (compilation, _) = CreateCompilation($"class C {{ func sum({parameters}) -> int => x }}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.VariableAlreadyDefined.Id);
    }

    [Theory]
    [InlineData("class C { func sum((x, y): (int, int)) -> int => x + y }")]
    [InlineData("record class Row(Value: int)\nclass C { func read(Row(let x): Row) -> int => x }")]
    public void ColdPatternDeclarationQuery_ReturnsLocal(string source)
    {
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var designation = tree.GetRoot().DescendantNodes().OfType<SingleVariableDesignationSyntax>().First();
        Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void RefutableNamedParameter_ReportsCoverageError()
    {
        var (compilation, _) = CreateCompilation("class C { func head([x, ..rest]: int[]) -> int => x }");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.RefutableParameterPattern.Id);
    }

    [Theory]
    [InlineData("interface C { func sum((x, y): (int, int)) -> int }")]
    [InlineData("class C { func sum(ref (x, y): (int, int)) -> int => 0 }")]
    public void UnsupportedContext_ReportsDiagnostic(string source)
    {
        var (compilation, _) = CreateCompilation(source);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == CompilerDiagnostics.ParameterPatternContextNotSupported.Id);
    }
}
