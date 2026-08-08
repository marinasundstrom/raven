using System;

using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class SyntaxFactoryEntryPointTests
{
    [Fact]
    public void ParseExpression_ParsesBinaryExpression()
    {
        var expression = SyntaxFactory.ParseExpression("1 + 2");

        Assert.IsType<InfixOperatorExpressionSyntax>(expression);
    }

    [Fact]
    public void ParseExpression_ParsesBitwiseXorExpression()
    {
        var expression = SyntaxFactory.ParseExpression("1 ^ 2");

        var binary = Assert.IsType<InfixOperatorExpressionSyntax>(expression);
        Assert.Equal(SyntaxKind.BitwiseXorExpression, binary.Kind);
    }

    [Fact]
    public void ParseExpression_ParsesBitwiseNotExpression()
    {
        var expression = SyntaxFactory.ParseExpression("~1");

        var unary = Assert.IsType<PrefixOperatorExpressionSyntax>(expression);
        Assert.Equal(SyntaxKind.BitwiseNotExpression, unary.Kind);
    }

    [Fact]
    public void ParseExpression_ParsesBlockExpression()
    {
        var expression = SyntaxFactory.ParseExpression("{ return 1; }");

        Assert.IsType<BlockSyntax>(expression);
    }

    [Fact]
    public void ParseStatement_ParsesReturnStatement()
    {
        var statement = SyntaxFactory.ParseStatement("return 1;");

        Assert.IsType<ReturnStatementSyntax>(statement);
    }

    [Fact]
    public void ParseStatement_WithPosition_ParsesFromOffset()
    {
        const string source = "let x = 0;\nreturn x;";
        var offset = source.IndexOf("return", StringComparison.Ordinal);

        var statement = SyntaxFactory.ParseStatement(SourceText.From(source), position: offset);

        Assert.IsType<ReturnStatementSyntax>(statement);
    }

    [Fact]
    public void ParseType_ParsesNullableType()
    {
        var type = SyntaxFactory.ParseType("string?");

        Assert.IsType<NullableTypeSyntax>(type);
    }

    [Fact]
    public void ParsePattern_ParsesVariablePattern()
    {
        var pattern = SyntaxFactory.ParsePattern("let value");

        Assert.IsType<VariablePatternSyntax>(pattern);
    }

    [Fact]
    public void ParseCompilationUnit_ParsesMembers()
    {
        var compilationUnit = SyntaxFactory.ParseCompilationUnit("class Widget { }");

        Assert.IsType<ClassDeclarationSyntax>(Assert.Single(compilationUnit.Members));
    }

    [Fact]
    public void ParseMemberDeclaration_RequiresExactlyOneDeclaration()
    {
        var member = SyntaxFactory.ParseMemberDeclaration("class Widget { }");
        var multiple = SyntaxFactory.ParseMemberDeclaration("class First { } class Second { }");
        var globalStatement = SyntaxFactory.ParseMemberDeclaration("let value = 1");
        var importAndMember = SyntaxFactory.ParseMemberDeclaration("import System.*\nclass Widget { }");

        Assert.IsType<ClassDeclarationSyntax>(member);
        Assert.Null(multiple);
        Assert.Null(globalStatement);
        Assert.Null(importAndMember);
    }
}
