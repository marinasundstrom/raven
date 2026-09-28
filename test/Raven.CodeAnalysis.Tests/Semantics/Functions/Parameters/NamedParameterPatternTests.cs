using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NamedParameterPatternTests : CompilationTestBase
{
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

    [Fact]
    public void ColdPatternDeclarationQuery_ReturnsLocal()
    {
        var (compilation, tree) = CreateCompilation("class C { func sum((x, y): (int, int)) -> int => x + y }");
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
