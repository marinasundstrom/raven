using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class ArrayTypeSyntaxTest
{
    [Fact]
    public void ArrayType_WithQualifiedElementType_Parses()
    {
        const string code = "val names: System.String[] = []";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;

        var array = Assert.IsType<ArrayTypeSyntax>(typeSyntax);
        Assert.IsType<QualifiedNameSyntax>(array.ElementType);
        var rankSpecifier = Assert.Single(array.RankSpecifiers);
        Assert.Empty(rankSpecifier.CommaTokens);
    }

    [Fact]
    public void ArrayType_WithMultiDimensionalRank_Parses()
    {
        const string code = "val matrix: int[,]";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;

        var array = Assert.IsType<ArrayTypeSyntax>(typeSyntax);
        var rankSpecifier = Assert.Single(array.RankSpecifiers);
        Assert.True(rankSpecifier.SizeToken.IsMissing);
        Assert.Single(rankSpecifier.CommaTokens);
    }

    [Fact]
    public void ArrayType_WithFixedSize_Parses()
    {
        const string code = "val values: int[4]";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;

        var array = Assert.IsType<ArrayTypeSyntax>(typeSyntax);
        var rankSpecifier = Assert.Single(array.RankSpecifiers);
        Assert.Equal("4", rankSpecifier.SizeToken.ValueText);
        Assert.Empty(rankSpecifier.CommaTokens);
    }

    [Fact]
    public void ArrayType_WithJaggedAndMultiDimensionalRanks_Parses()
    {
        const string code = "val values: int[][,]";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;

        var array = Assert.IsType<ArrayTypeSyntax>(typeSyntax);
        Assert.Collection(
            array.RankSpecifiers,
            first =>
            {
                Assert.True(first.SizeToken.IsMissing);
                Assert.Empty(first.CommaTokens);
            },
            second => Assert.Single(second.CommaTokens));
    }

    [Fact]
    public void ArrayType_WithJaggedFixedAndOpenRanks_Parses()
    {
        const string code = "val values: int[4][]";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;

        var array = Assert.IsType<ArrayTypeSyntax>(typeSyntax);
        Assert.Collection(
            array.RankSpecifiers,
            first =>
            {
                Assert.Equal("4", first.SizeToken.ValueText);
                Assert.Empty(first.CommaTokens);
            },
            second =>
            {
                Assert.True(second.SizeToken.IsMissing);
                Assert.Empty(second.CommaTokens);
            });
    }
}
