using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxFactoryDeclarationTests
{
    [Fact]
    public void PropertyDeclaration_WithAccessorList_LeavesExpressionBodyNull()
    {
        var property = PropertyDeclaration(
            List<AttributeListSyntax>(),
            TokenList(),
            VarKeyword,
            Identifier("Title"),
            TypeAnnotationClause(ColonToken, PredefinedType(StringKeyword)),
            AccessorList(List<AccessorDeclarationSyntax>()));

        property.AccessorList.ShouldNotBeNull();
        property.ExpressionBody.ShouldBeNull();
        property.Initializer.ShouldBeNull();
    }

    [Fact]
    public void PropertyDeclaration_WithExpressionBody_LeavesAccessorListNull()
    {
        var property = PropertyDeclaration(
            List<AttributeListSyntax>(),
            TokenList(),
            VarKeyword,
            Identifier("Title"),
            TypeAnnotationClause(ColonToken, PredefinedType(StringKeyword)),
            ArrowExpressionClause(IdentifierName("_title")));

        property.AccessorList.ShouldBeNull();
        property.ExpressionBody.ShouldNotBeNull();
        property.Initializer.ShouldBeNull();
    }

    [Fact]
    public void MethodDeclaration_WithBody_LeavesExpressionBodyNull()
    {
        var method = MethodDeclaration(
            List<AttributeListSyntax>(),
            TokenList(),
            FuncKeyword,
            Identifier("Build"),
            ParameterList(OpenParenToken, SeparatedList<ParameterSyntax>(), CloseParenToken),
            List<TypeParameterConstraintClauseSyntax>(),
            BlockStatement(List<StatementSyntax>()));

        method.Body.ShouldNotBeNull();
        method.ExpressionBody.ShouldBeNull();
    }

    [Fact]
    public void MethodDeclaration_WithExpressionBody_LeavesBodyNull()
    {
        var method = MethodDeclaration(
            List<AttributeListSyntax>(),
            TokenList(),
            FuncKeyword,
            Identifier("Build"),
            ParameterList(OpenParenToken, SeparatedList<ParameterSyntax>(), CloseParenToken),
            List<TypeParameterConstraintClauseSyntax>(),
            ArrowExpressionClause(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1))));

        method.Body.ShouldBeNull();
        method.ExpressionBody.ShouldNotBeNull();
    }

    [Fact]
    public void AccessorDeclaration_WithBody_LeavesExpressionBodyNull()
    {
        var accessor = AccessorDeclaration(
            SyntaxKind.SetAccessorDeclaration,
            List<AttributeListSyntax>(),
            TokenList(),
            SetKeyword,
            BlockStatement(List<StatementSyntax>()));

        accessor.Body.ShouldNotBeNull();
        accessor.ExpressionBody.ShouldBeNull();
    }

    [Fact]
    public void AccessorDeclaration_WithExpressionBody_LeavesBodyNull()
    {
        var accessor = AccessorDeclaration(
            SyntaxKind.GetAccessorDeclaration,
            List<AttributeListSyntax>(),
            TokenList(),
            GetKeyword,
            ArrowExpressionClause(IdentifierName("_title")));

        accessor.Body.ShouldBeNull();
        accessor.ExpressionBody.ShouldNotBeNull();
    }

    [Fact]
    public void Parameter_WithBindingKeywordAndType_UsesTypedConvenienceOverload()
    {
        var parameter = Parameter(
            List<AttributeListSyntax>(),
            ValKeyword,
            Identifier("title"),
            TypeAnnotationClause(ColonToken, PredefinedType(StringKeyword)));

        parameter.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        parameter.Identifier.ValueText.ShouldBe("title");
        parameter.TypeAnnotation.ShouldNotBeNull();
        parameter.Pattern.ShouldBeNull();
    }
}
