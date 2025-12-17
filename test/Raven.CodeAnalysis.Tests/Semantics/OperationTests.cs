using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class OperationTests : CompilationTestBase
{
    [Fact]
    public void GetOperation_VariableDeclaration_ReturnsDeclarators()
    {
        const string source = """
var first = 1, second = 2
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<LocalDeclarationStatementSyntax>()
            .Single();

        var operation = model.GetOperation(declarationSyntax)
            .ShouldBeOfType<IVariableDeclarationOperation>();

        operation.Kind.ShouldBe(OperationKind.LocalDeclaration);
        operation.Declarators.Select(declarator => declarator.Symbol.Name)
            .ShouldBe(new[] { "first", "second" });

        var firstDeclaratorSyntax = declarationSyntax.Declaration.Declarators.First();
        model.GetOperation(firstDeclaratorSyntax)
            .ShouldBeSameAs(operation.Declarators[0]);

        var initializer = operation.Declarators[0].Initializer;
        initializer.ShouldNotBeNull();
        initializer!.IsImplicit.ShouldBeFalse();
        initializer.Value.ShouldNotBeNull();
    }
}
