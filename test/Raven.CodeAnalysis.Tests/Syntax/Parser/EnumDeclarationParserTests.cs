using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class EnumDeclarationParserTests
{
    [Fact]
    public void EnumDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = "[Flags] enum Colors { Red }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(root.Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Flags", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EnumDeclaration_WithBaseList_ParsesUnderlyingType()
    {
        var source = "enum Status : byte { Ok = 1 }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.BaseList);
        var underlying = Assert.IsType<SimpleBaseTypeSyntax>(Assert.Single(declaration.BaseList!.Types));
        var predefined = Assert.IsType<PredefinedTypeSyntax>(underlying.Type);
        Assert.Equal(SyntaxKind.ByteKeyword, predefined.Keyword.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EnumDeclaration_NewLineSeparatedMembers_UseImplicitSeparators()
    {
        var source = """
            enum Status {
                Ok
                Error
                Unknown
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(3, declaration.Members.Count);
        Assert.Equal(SyntaxKind.None, declaration.Members.GetSeparator(0).Kind);
        Assert.Equal(SyntaxKind.None, declaration.Members.GetSeparator(1).Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EnumDeclaration_CommaLessMembersOnSameLine_ProducesDiagnostic()
    {
        var source = "enum Status { Ok Error Unknown }";
        var tree = SyntaxTree.ParseText(source);

        Assert.Contains(
            tree.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.IdentifierExpected ||
                          diagnostic.Descriptor == CompilerDiagnostics.CharacterExpected);
    }

    [Fact]
    public void EnumDeclaration_SemicolonSeparatedMembers_UseExplicitSeparators()
    {
        var source = "enum Status { Ok; Error; Unknown }";
        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.Equal(3, declaration.Members.Count);
        Assert.Equal(SyntaxKind.SemicolonToken, declaration.Members.GetSeparator(0).Kind);
        Assert.Equal(SyntaxKind.SemicolonToken, declaration.Members.GetSeparator(1).Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EnumDeclaration_ExplicitSeparatorsMayMixWithImplicitNewlineSeparators()
    {
        var source = """
            enum Status {
                Ok;
                Error
                Unknown
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.Equal(SyntaxKind.SemicolonToken, declaration.Members.GetSeparator(0).Kind);
        Assert.Equal(SyntaxKind.None, declaration.Members.GetSeparator(1).Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EnumDeclaration_MixedExplicitSeparatorKinds_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("enum Status { Ok, Error; Unknown }");
        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        Assert.Equal(SyntaxKind.CommaToken, declaration.Members.GetSeparator(0).Kind);
        Assert.Equal(SyntaxKind.SemicolonToken, declaration.Members.GetSeparator(1).Kind);
        Assert.Contains(tree.GetDiagnostics(), diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CharacterExpected);
    }
}
