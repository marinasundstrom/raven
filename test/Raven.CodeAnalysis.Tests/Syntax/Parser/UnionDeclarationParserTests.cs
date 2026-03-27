using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class UnionDeclarationParserTests
{
    [Fact]
    public void UnionDeclaration_WithCases_ParsesCaseList()
    {
        var source = """
            union Token {
                Identifier(text: string)
                Unknown
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal("Token", declaration.Identifier.Text);
        Assert.Collection(
            declaration.CaseTypes,
            first =>
            {
                Assert.Equal("Identifier", first.Identifier.Text);
                Assert.NotNull(first.ParameterList);
            },
            second =>
            {
                Assert.Equal("Unknown", second.Identifier.Text);
                Assert.Null(second.ParameterList);
            });

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void UnionDeclaration_WithSemicolonSeparatedCases_ParsesCaseList()
    {
        var source = "union Token { Identifier(text: string); Unknown }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Collection(
            declaration.CaseTypes,
            first => Assert.Equal(SyntaxKind.SemicolonToken, first.TerminatorToken.Kind),
            second => Assert.Equal(SyntaxKind.None, second.TerminatorToken.Kind));

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void UnionDeclaration_OnSameLineWithoutSeparator_ReportsDiagnostic()
    {
        var source = "union LookupResult { Found(id: int) Missing }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));
        Assert.Equal(2, declaration.CaseTypes.Count);

        Assert.Contains(
            tree.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.ExpectedNewLineBetweenDeclarations);
    }

    [Fact]
    public void UnionDeclaration_WithTypeParameters_ParsesGenerics()
    {
        var source = "union Result<T> { Ok(value: T) }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        var okCase = Assert.Single(declaration.CaseTypes);
        Assert.Equal("Ok", okCase.Identifier.Text);
        Assert.NotNull(okCase.ParameterList);
    }

    [Fact]
    public void UnionDeclaration_WithStorageKindAndNominalMembers_ParsesMemberTypes()
    {
        var source = "union struct Either<T1, T2>(T1, T2)";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(SyntaxKind.StructKeyword, declaration.ClassOrStructKeyword.Kind);
        Assert.NotNull(declaration.TypeParameterList);
        Assert.NotNull(declaration.MemberTypes);
        Assert.Empty(declaration.CaseTypes);
        Assert.Equal(2, declaration.MemberTypes!.Types.Count);
    }

    [Fact]
    public void UnionDeclaration_WithGenericNominalMembers_ParsesMemberTypes()
    {
        var source = "union MyResult2<T>(List<T>, int)";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        Assert.NotNull(declaration.MemberTypes);
        Assert.Empty(declaration.CaseTypes);
        Assert.Equal("List<T>", declaration.MemberTypes!.Types[0].ToString());
        Assert.Equal("int", declaration.MemberTypes.Types[1].ToString());
    }

    [Fact]
    public void UnionDeclaration_WithoutStorageKind_DefaultsToNominalMemberFormWhenParenthesized()
    {
        var source = "union Either(A, B)";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(SyntaxKind.None, declaration.ClassOrStructKeyword.Kind);
        Assert.NotNull(declaration.MemberTypes);
        Assert.Empty(declaration.CaseTypes);
        Assert.Equal("A", declaration.MemberTypes!.Types[0].ToString());
        Assert.Equal("B", declaration.MemberTypes!.Types[1].ToString());
    }
}
