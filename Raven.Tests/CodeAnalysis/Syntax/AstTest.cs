﻿using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class AstTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test1()
    {
        var ifStatement = IfStatement(
                condition: BinaryExpression(
                    IdentifierName("x"),
                    GreaterThanToken,
                    IdentifierName("y")),
                statement: Block(SingletonList<StatementSyntax>(
                    ReturnStatement(
                        LiteralExpression(2))
                )));

        var ifStatementWithElseClause = ifStatement
                .WithElseClause(
                    ElseClause(
                        ReturnStatement(
                            LiteralExpression(2))));

        var methodDeclaration = MethodDeclaration(
                ParseTypeName("test"),
                IdentifierName("FooBar"),
                TypeParameterList(
                    SeparatedList<ParameterSyntax>([
                        Parameter(IdentifierName("a")),
                        CommaToken,
                        Parameter(IdentifierName("b"))
                    ])
                ))
            .WithBody(
                Block(
                    List<StatementSyntax>(ifStatementWithElseClause)));

        var r = methodDeclaration.ReturnType;

        var pars = methodDeclaration.ParameterList.Parameters.ToList();

        testOutputHelper.WriteLine($"Equal: {ifStatement} == {ifStatementWithElseClause} = {ifStatement == ifStatementWithElseClause}");

        var descendants = ifStatementWithElseClause.Descendants().ToArray();

        var returnStatements = ifStatementWithElseClause
            .Descendants()
            .OfType<ReturnStatementSyntax>().ToArray();

        var parentIfStatement = returnStatements
            .First()
            .Ancestors()
            .OfType<IfStatementSyntax>()
            .ToList();

        var blockStatement = ifStatementWithElseClause.Statement;

        var blockStatementChildren = blockStatement.ChildNodesAndTokens();

        var elseClause = ifStatementWithElseClause.ElseClause;

        var c = ifStatementWithElseClause.ChildNodesAndTokens();
        foreach (var e in c)
        {
            testOutputHelper.WriteLine($"{e.Node?.Kind ?? e.Token.Kind}");
        }

        /*
        var c2 = foo2.ChildNodes();
        foreach (var e in c2)
        {
            testOutputHelper.WriteLine($"{e}");
        }

        var ancestors = foo2.Ancestors();
        foreach (var e in ancestors)
        {
            testOutputHelper.WriteLine($"{e}");
        }

        var descendants = foo2.Descendants();
        foreach (var e in descendants)
        {
            testOutputHelper.WriteLine($"{e}");
        }
        */

        var f = ifStatementWithElseClause.TrailingTrivia;
    }

    [Fact]
    public void Test2()
    {
        var x = SeparatedList<ParameterSyntax>([
            Parameter(IdentifierName("x"))
        ]);

        var foo = x.ToList();
    }

    [Fact]
    public void Test3()
    {
        var varDecl = LocalDeclarationStatement(
            VariableDeclaration(SeparatedList<VariableDeclaratorSyntax>([
                VariableDeclarator(
                    IdentifierName("Foo"),
                    TypeAnnotation(ParseTypeName("int")))
            ])));

        var varDecl2 = LocalDeclarationStatement(
            VariableDeclaration(SeparatedList<VariableDeclaratorSyntax>([
                VariableDeclarator(
                    IdentifierName("Foo"),
                    TypeAnnotation(ParseTypeName("int")),
                    EqualsValueClause(LiteralExpression(20)))
            ])));
    }

    [Fact]
    public void Test4()
    {
        var ifStatement = IfStatement(
            condition: BinaryExpression(
                IdentifierName("x"),
                GreaterThanToken,
                IdentifierName("y")),
            statement: Block(List<StatementSyntax>(
                ReturnStatement(
                    LiteralExpression(2))
            )));

        var ifStatementWithElseClause = ifStatement
                .WithElseClause(
                    ElseClause(
                        ReturnStatement(
                            LiteralExpression(2))));

        var methodDeclaration = MethodDeclaration(
                ParseTypeName("test"),
                IdentifierName("FooBar"),
                TypeParameterList(
                    SeparatedList<ParameterSyntax>([
                        Parameter(IdentifierName("a")),
                        CommaToken,
                        Parameter(IdentifierName("b"))
                    ])
                ))
            .WithBody(
                Block(
                    List<StatementSyntax>(
                        ifStatementWithElseClause)));

        var members = List<MemberDeclarationSyntax>(
                    NamespaceDeclaration(
                        IdentifierName("MyApp"),
                        List<MemberDeclarationSyntax>(
                            LocalDeclarationStatement(
                                    VariableDeclaration(SeparatedList<VariableDeclaratorSyntax>([
                                        VariableDeclarator(
                                            IdentifierName("Foo"),
                                            TypeAnnotation(ParseTypeName("int")),
                                            EqualsValueClause(LiteralExpression(20)))
                            ]))),
                            methodDeclaration)));

        var compilationUnit = CompilationUnit()
            .WithImports(
                List(
                    ImportDirective(IdentifierName("Foo"))))
            .WithMembers(members);

        var m = compilationUnit.Members;

        compilationUnit.NormalizeWhitespace();

        var syntaxtTree = SyntaxTree.Create(compilationUnit);

        var compilation = Compilation.Create("MyCompilation")
            .AddSyntaxTrees(syntaxtTree);
        //.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));

        var semanticModel = compilation.GetSemanticModel(syntaxtTree);

        var varDelc = compilationUnit
            .Descendants()
            .OfType<VariableDeclarationSyntax>()
            .First();

        var symbol = semanticModel.GetSymbolInfo(varDelc);

        if (symbol.Symbol is ILocalSymbol localSymbol)
        {
            Console.WriteLine(localSymbol.Name);
        }
    }
}