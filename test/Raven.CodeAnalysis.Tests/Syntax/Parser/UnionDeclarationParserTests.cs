using System.Linq;

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
                case Identifier(text: string)
                case Unknown
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));
        var caseDeclarations = declaration.Members.OfType<CaseDeclarationSyntax>().ToArray();

        Assert.Equal("Token", declaration.Identifier.Text);
        Assert.Collection(
            caseDeclarations,
            first =>
            {
                Assert.Equal(SyntaxKind.CaseKeyword, first.CaseKeyword.Kind);
                Assert.Equal("Identifier", first.Identifier.Text);
                Assert.NotNull(first.ParameterList);
            },
            second =>
            {
                Assert.Equal(SyntaxKind.CaseKeyword, second.CaseKeyword.Kind);
                Assert.Equal("Unknown", second.Identifier.Text);
                Assert.Null(second.ParameterList);
            });

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void UnionDeclaration_WithSemicolonSeparatedCases_ParsesCaseList()
    {
        var source = "union Token { case Identifier(text: string); case Unknown }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));
        var caseDeclarations = declaration.Members.OfType<CaseDeclarationSyntax>().ToArray();

        Assert.Collection(
            caseDeclarations,
            first => Assert.Equal(SyntaxKind.SemicolonToken, first.TerminatorToken.Kind),
            second => Assert.Equal(SyntaxKind.None, second.TerminatorToken.Kind));

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void UnionDeclaration_OnSameLineWithoutSeparator_ReportsDiagnostic()
    {
        var source = "union LookupResult { case Found(id: int) case Missing }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));
        Assert.Equal(2, declaration.Members.OfType<CaseDeclarationSyntax>().Count());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void UnionDeclaration_WithTypeParameters_ParsesGenerics()
    {
        var source = "union Result<T> { case Ok(value: T) }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        var okCase = Assert.Single(declaration.Members.OfType<CaseDeclarationSyntax>());
        Assert.Equal("Ok", okCase.Identifier.Text);
        Assert.NotNull(okCase.ParameterList);
    }

    [Fact]
    public void UnionDeclaration_WithStorageKindAndNominalMembers_ParsesMemberTypes()
    {
        var source = "union struct Either<T1, T2>(T1 | T2)";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(SyntaxKind.StructKeyword, declaration.ClassOrStructKeyword.Kind);
        Assert.NotNull(declaration.TypeParameterList);
        Assert.NotNull(declaration.MemberTypes);
        Assert.Empty(declaration.Members.OfType<CaseDeclarationSyntax>());
        Assert.Equal(2, declaration.MemberTypes!.Types.Count);
    }

    [Fact]
    public void UnionDeclaration_WithCasesAndMethodBody_ParsesAllMembers()
    {
        var source = """
            union class Option<T> {
                case Some(value: T)
                case None

                func Map<TResult>(mapper: T -> TResult) -> Option<TResult> {
                    None
                }
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));
        var members = declaration.Members.ToArray();

        Assert.Collection(
            members,
            first => Assert.IsType<CaseDeclarationSyntax>(first),
            second => Assert.IsType<CaseDeclarationSyntax>(second),
            third => Assert.IsType<MethodDeclarationSyntax>(third));

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void UnionDeclaration_WithGenericNominalMembers_ParsesMemberTypes()
    {
        var source = "union MyResult2<T>(List<T> | int)";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        Assert.NotNull(declaration.MemberTypes);
        Assert.Empty(declaration.Members.OfType<CaseDeclarationSyntax>());
        Assert.Equal("List<T>", declaration.MemberTypes!.Types[0].ToString());
        Assert.Equal("int", declaration.MemberTypes.Types[1].ToString());
    }

    [Fact]
    public void UnionDeclaration_WithoutStorageKind_DefaultsToNominalMemberFormWhenParenthesized()
    {
        var source = "union Either(A | B)";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<UnionDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(SyntaxKind.None, declaration.ClassOrStructKeyword.Kind);
        Assert.NotNull(declaration.MemberTypes);
        Assert.Empty(declaration.Members.OfType<CaseDeclarationSyntax>());
        Assert.Equal("A", declaration.MemberTypes!.Types[0].ToString());
        Assert.Equal("B", declaration.MemberTypes!.Types[1].ToString());
    }
}
