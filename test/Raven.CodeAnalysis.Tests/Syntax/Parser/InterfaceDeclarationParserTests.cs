using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class InterfaceDeclarationParserTests
{
    [Fact]
    public void InterfaceDeclaration_AllowsEmptyBody()
    {
        var source = "interface IA {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal("IA", declaration.Identifier.Text);
        Assert.Empty(declaration.Members);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InterfaceDeclaration_AllowsAdjacentDeclarations()
    {
        var source = "interface IA {} interface IB {} class C : IA, IB {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var members = root.Members;

        Assert.Equal(3, members.Count);
        Assert.IsType<InterfaceDeclarationSyntax>(members[0]);
        Assert.IsType<InterfaceDeclarationSyntax>(members[1]);

        var classDeclaration = Assert.IsType<ClassDeclarationSyntax>(members[2]);
        Assert.NotNull(classDeclaration.BaseList);
        var baseList = classDeclaration.BaseList!;
        Assert.Equal(2, baseList.Types.Count);

        Assert.Equal(
            2,
            tree.GetDiagnostics().Count(d => d.Descriptor == CompilerDiagnostics.ExpectedNewLineBetweenDeclarations));
    }

    [Fact]
    public void InterfaceDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = "[Service] interface IService {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(root.Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Service", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InterfaceDeclaration_WithPermitsClause_ParsesSuccessfully()
    {
        var source = "sealed interface IHttpResponse permits Success, NotFound {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal("IHttpResponse", declaration.Identifier.Text);
        Assert.NotNull(declaration.PermitsClause);
        Assert.Equal(2, declaration.PermitsClause!.Types.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InterfaceDeclaration_WithNestedConstrainedGenericRecords_ParsesSuccessfully()
    {
        var source = """
            import System.Numerics.*

            sealed interface Expr<T>
                where T: INumber<T> {
                record Literal<T>(Value: T) : Expr<T>
                    where T: INumber<T>

                record Add<T>(Left: Expr<T>, Right: Expr<T>) : Expr<T>
                    where T: INumber<T>
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(root.Members.Last());
        Assert.NotNull(declaration.TypeParameterList);
        Assert.Single(declaration.TypeParameterList!.Parameters);
        Assert.Single(declaration.ConstraintClauses);
        Assert.Equal(2, declaration.Members.Count);

        var literal = Assert.IsType<RecordDeclarationSyntax>(declaration.Members[0]);
        Assert.NotNull(literal.TypeParameterList);
        Assert.NotNull(literal.ParameterList);
        Assert.Single(literal.ConstraintClauses);

        var add = Assert.IsType<RecordDeclarationSyntax>(declaration.Members[1]);
        Assert.NotNull(add.TypeParameterList);
        Assert.NotNull(add.ParameterList);
        Assert.Single(add.ConstraintClauses);

        Assert.Empty(tree.GetDiagnostics());
    }
}
