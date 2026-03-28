using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class PropertySemanticTests : CompilationTestBase
{
    [Fact]
    public void AutoPropertyGetter_HasSynthesizedBody()
    {
        const string source = """
            class Counter {
                public var Count: int { get; set; }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var counter = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var property = counter.GetMembers("Count").OfType<IPropertySymbol>().Single();
        var getter = property.GetMethod;
        Assert.NotNull(getter);

        Assert.True(compilation.TryGetSynthesizedMethodBody(getter!, BoundTreeView.Original, out var body));
        Assert.NotNull(body);
        var returnStatement = Assert.IsType<BoundReturnStatement>(Assert.Single(body!.Statements));
        var fieldAccess = Assert.IsType<BoundFieldAccess>(returnStatement.Expression);
        Assert.Equal("<Count>k__BackingField", fieldAccess.Field.Name);
    }

    [Fact]
    public void AutoPropertySetter_HasSynthesizedBody()
    {
        const string source = """
            class Counter {
                public static var Count: int { get; set; }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var counter = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var property = counter.GetMembers("Count").OfType<IPropertySymbol>().Single();
        var setter = property.SetMethod;
        Assert.NotNull(setter);

        Assert.True(compilation.TryGetSynthesizedMethodBody(setter!, BoundTreeView.Original, out var body));
        Assert.NotNull(body);

        Assert.Collection(
            body!.Statements,
            statement =>
            {
                var assignment = Assert.IsType<BoundAssignmentStatement>(statement);
                var fieldAssignment = Assert.IsType<BoundFieldAssignmentExpression>(assignment.Expression);
                Assert.Equal("<Count>k__BackingField", fieldAssignment.Field.Name);
                Assert.Null(fieldAssignment.Receiver);
            },
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }
}
