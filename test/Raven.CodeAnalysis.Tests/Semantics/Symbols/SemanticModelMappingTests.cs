using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticModelMappingTests : CompilationTestBase
{
    [Fact]
    public void GetSyntax_ReturnsSyntaxForBoundExpression()
    {
        var (compilation, tree) = CreateCompilation("func Main() { val x = 1 + 2; }");
        var model = compilation.GetSemanticModel(tree);

        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var boundDeclarator = model.GetBoundNode(declarator);

        var syntax = model.GetSyntax(boundDeclarator);

        Assert.NotNull(syntax);
        Assert.Same(declarator.SyntaxTree, syntax!.SyntaxTree);
        Assert.Equal(declarator.Span, syntax.Span);
    }

    [Fact]
    public void GetSyntax_ReturnsSyntaxForBoundStatement()
    {
        var (compilation, tree) = CreateCompilation("func Main() { if true { val x = 0; } }");
        var model = compilation.GetSemanticModel(tree);

        var ifStatement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var boundIf = model.GetBoundNode(ifStatement);

        var syntax = model.GetSyntax(boundIf);

        Assert.NotNull(syntax);
        Assert.Same(ifStatement.SyntaxTree, syntax!.SyntaxTree);
        Assert.Equal(ifStatement.Span, syntax.Span);
    }

    [Fact]
    public void GetSyntax_ReturnsSyntaxForLoweredMatchSyntheticStatement()
    {
        var code = """
func Main(value: int) -> int {
    match value {
        0 => return 1
        _ => return 2
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);

        var methodBlockSyntax = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Single();
        var matchStatement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var lowered = model.GetBoundNode(matchStatement, BoundTreeView.Lowered);
        var loweredBlock = Assert.IsType<BoundBlockStatement>(lowered);
        var loweredIf = loweredBlock.Statements.OfType<BoundIfStatement>().First();

        var syntax = model.GetSyntax(loweredIf);

        Assert.NotNull(syntax);
        Assert.Same(matchStatement.SyntaxTree, syntax!.SyntaxTree);
        Assert.NotEqual(methodBlockSyntax.Span, syntax.Span);
    }

    [Fact]
    public void GetDeclaredSymbol_DeclarationPatternDesignation_UsesDeclaredType()
    {
        const string code = """
class C {
    func Run(value: object) -> int {
        if value is string text {
            return text.Length
        }

        return 0
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var designation = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(designation => designation.Identifier.ValueText == "text");

        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation));

        Assert.Equal("text", symbol.Name);
        Assert.Equal("string", symbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void GetDeclaredSymbol_MatchSequenceRestDesignation_UsesSliceType()
    {
        const string code = """
class C {
    func Run(values: int[]) -> int {
        return values match {
            [val head, ..val rest] => rest.Length + head
            _ => 0
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var designation = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(designation => designation.Identifier.ValueText == "rest");

        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation));

        Assert.Equal("rest", symbol.Name);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(symbol.Type);
        Assert.Equal(SpecialType.System_Int32, arrayType.ElementType.SpecialType);
    }
}
