using Xunit;
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

        TypeParameterListSyntax parsLis = methodDeclaration.ParameterList;

        var sep = parsLis.Parameters.GetSeparator(0);

        var pars = parsLis.Parameters.ToList();

        testOutputHelper.WriteLine($"Equal: {ifStatement} == {ifStatementWithElseClause} = {ifStatement == ifStatementWithElseClause}");

        var descendants = ifStatementWithElseClause.DescendantNodes().ToArray();

        var returnStatements = ifStatementWithElseClause
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>().ToArray();

        var parentIfStatement = returnStatements
            .First()
            .AncestorNodes()
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

        var str = ifStatement.ToString();
        var str2 = ifStatement.ToFullString();

        var foo1 = ifStatement.Span;
        var foo2 = ifStatement.FullSpan;

        var ifStatementWithElseClause = ifStatement
                .WithElseClause(
                    ElseClause(
                        ReturnStatement(
                            LiteralExpression(2))));

        var methodDeclaration = MethodDeclaration(
                ParseTypeName("int"),
                IdentifierName("FooBar"),
                TypeParameterList(
                    SeparatedList<ParameterSyntax>([
                        Parameter(IdentifierName("a"), TypeAnnotation(ParseTypeName("int"))),
                        CommaToken,
                        Parameter(IdentifierName("b"), TypeAnnotation(ParseTypeName("int")))
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

        var test = compilationUnit.ToFullString();

        var m = compilationUnit.Members;

        compilationUnit = compilationUnit.NormalizeWhitespace();

        var syntaxTree = SyntaxTree.Create(compilationUnit);

        var compilation = Compilation.Create("MyCompilation")
            .AddSyntaxTrees(syntaxTree);
        //.AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var varDelc = compilation.SyntaxTrees.First()
            .GetSyntaxRoot()
            .DescendantNodes()
            .OfType<VariableDeclarationSyntax>()
            .First();

        var symbol = semanticModel.GetSymbolInfo(varDelc);

        if (symbol.Symbol is ILocalSymbol localSymbol)
        {
            Console.WriteLine(localSymbol.Name);
        }
    }

    [Fact]
    public void Test5()
    {
        var compilationUnit = CompilationUnit()
            .WithImports(
                List(
                    ImportDirective(
                        ImportKeyword, 
                        IdentifierName("Foo")
                                .WithLeadingTrivia(
                                    TriviaList(Whitespace(" "))),
                        SemicolonToken)));
        
        var syntaxTree = SyntaxTree.Create(compilationUnit);

        var compilation = Compilation.Create("MyCompilation")
            .AddSyntaxTrees(syntaxTree);

        var root = compilation.SyntaxTrees.First().GetSyntaxRoot();
        
        testOutputHelper.WriteLine(root.ToFullString());
    }
}