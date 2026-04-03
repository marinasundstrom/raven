using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class ConstrainedSealedHierarchyRegressionTests : Raven.CodeAnalysis.Semantics.Tests.CompilationTestBase
{
    [Fact]
    public void NestedGenericSealedCases_UseOwnTypeParameterInBaseInterface()
    {
        const string code = """
        import System.Numerics.*

        sealed interface Expr<T>
            where T: INumber<T> {
            record Literal<T>(Value: T) : Expr<T>
                where T: INumber<T>

            record Add<T>(Left: Expr<T>, Right: Expr<T>) : Expr<T>
                where T: INumber<T>
        }
        """;

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "sealed_hierarchy_constraints_debug",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var declarations = root.DescendantNodes().OfType<TypeDeclarationSyntax>().ToArray();
        var exprDeclaration = declarations.Single(t => t.Identifier.ValueText == "Expr");
        var addDeclaration = declarations.Single(t => t.Identifier.ValueText == "Add");

        var exprSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(exprDeclaration));
        var addSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(addDeclaration));

        Assert.Single(exprSymbol.TypeParameters);
        Assert.Single(addSymbol.TypeParameters);

        var addTypeParameter = addSymbol.TypeParameters[0];
        Assert.NotEmpty(addTypeParameter.ConstraintTypes);

        var addConstraint = Assert.IsAssignableFrom<INamedTypeSymbol>(addTypeParameter.ConstraintTypes.Single());
        Assert.Equal("INumber", addConstraint.Name);
        Assert.Single(addConstraint.TypeArguments);
        Assert.Same(addTypeParameter, addConstraint.TypeArguments[0]);

        var exprInterface = Assert.Single(addSymbol.Interfaces.Where(i => i.Name == "Expr"));
        Assert.Equal("Expr", exprInterface.Name);
        Assert.Single(exprInterface.TypeArguments);
        Assert.Same(addTypeParameter, exprInterface.TypeArguments[0]);
    }
}
