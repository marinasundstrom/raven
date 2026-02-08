using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class OperationTests : CompilationTestBase
{
    [Fact]
    public void GetOperation_VariableDeclaration_ReturnsDeclarators()
    {
        const string source = """
var first = 1
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var declarationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<LocalDeclarationStatementSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IVariableDeclarationOperation>(model.GetOperation(declarationSyntax));

        operation.Kind.ShouldBe(OperationKind.LocalDeclaration);
        operation.Declarators.Select(declarator => declarator.Symbol.Name)
            .ShouldBe(new[] { "first" });

        var firstDeclaratorSyntax = declarationSyntax.Declaration.Declarators.First();
        model.GetOperation(firstDeclaratorSyntax)
            .ShouldBeSameAs(operation.Declarators[0]);

        var initializer = operation.Declarators[0].Initializer;
        initializer.ShouldNotBeNull();
        initializer!.IsImplicit.ShouldBeFalse();
        initializer.Value.ShouldNotBeNull();
    }

    [Fact]
    public void GetOperation_IfExpression_ReturnsConditionalShape()
    {
        const string source = """
var flag = true
var value = if flag 1 else 2
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var ifExpressionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IConditionalOperation>(model.GetOperation(ifExpressionSyntax));

        operation.Kind.ShouldBe(OperationKind.Conditional);
        operation.Condition.ShouldNotBeNull();
        operation.WhenTrue.ShouldNotBeNull();
        operation.WhenFalse.ShouldNotBeNull();
        operation.WhenTrue!.Kind.ShouldBe(OperationKind.Literal);
        operation.WhenFalse!.Kind.ShouldBe(OperationKind.Literal);
        operation.ChildOperations.Length.ShouldBe(3);
        operation.Condition!.Parent.ShouldBeSameAs(operation);
        model.GetOperation(ifExpressionSyntax).ShouldBeSameAs(operation);
    }

    [Fact]
    public void GetOperation_IfStatement_ReturnsConditionalBranches()
    {
        const string source = """
class C {
    Test(flag: bool) {
        if flag return else return
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var ifStatementSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfStatementSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IConditionalOperation>(model.GetOperation(ifStatementSyntax));

        operation.Condition.ShouldNotBeNull();
        operation.WhenTrue.ShouldNotBeNull();
        operation.WhenFalse.ShouldNotBeNull();
        operation.WhenTrue!.Kind.ShouldBe(OperationKind.Return);
        operation.WhenFalse!.Kind.ShouldBe(OperationKind.Return);
    }

    [Fact]
    public void GetOperation_ParameterReference_ExposesNamedAccessor()
    {
        const string source = """
class C {
    Test(param: int) -> int {
        return param
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var returnExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .Single()
            .Expression
            .ShouldBeOfType<IdentifierNameSyntax>();

        var operation = Assert.IsAssignableFrom<IParameterReferenceOperation>(model.GetOperation(returnExpression));

        operation.Parameter.ShouldBeSameAs(operation.Symbol);
        operation.Parameter.Name.ShouldBe("param");
    }

    [Fact]
    public void GetOperation_MatchExpression_ExposesSwitchShapeAndPatternChildren()
    {
        const string source = """
var value = 1
var result = value match {
    not 1 or 2 when true => 10
    _ => 20
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var matchExpressionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MatchExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<ISwitchOperation>(model.GetOperation(matchExpressionSyntax));

        operation.Value.ShouldNotBeNull();
        operation.Patterns.Length.ShouldBe(2);
        operation.Guards.Length.ShouldBe(1);
        operation.ArmValues.Length.ShouldBe(2);

        var orPattern = Assert.IsAssignableFrom<IOrPatternOperation>(operation.Patterns[0]);
        Assert.IsAssignableFrom<INotPatternOperation>(orPattern.Left);
        Assert.IsAssignableFrom<IConstantPatternOperation>(orPattern.Right);

        var notPattern = Assert.IsAssignableFrom<INotPatternOperation>(orPattern.Left);
        var innerConstant = Assert.IsAssignableFrom<IConstantPatternOperation>(notPattern.Pattern);
        innerConstant.Value.ShouldNotBeNull();

        operation.Guards[0].Kind.ShouldBe(OperationKind.Literal);
        operation.ArmValues[0].Kind.ShouldBe(OperationKind.Literal);
        operation.ArmValues[1].Kind.ShouldBe(OperationKind.Literal);
    }

    [Fact]
    public void GetOperation_PositionalPattern_ExposesSubpatterns()
    {
        const string source = """
var tuple = (1, 2)
var result = tuple match {
    (val a, val b) => a
    _ => 0
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var positionalPatternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<PositionalPatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IPositionalPatternOperation>(model.GetOperation(positionalPatternSyntax));

        operation.Subpatterns.Length.ShouldBe(2);
        operation.Subpatterns[0].Kind.ShouldBe(OperationKind.DeclarationPattern);
        operation.Subpatterns[1].Kind.ShouldBe(OperationKind.DeclarationPattern);
    }

    [Fact]
    public void GetOperation_ArrayElementAccess_ExposesInstanceAndArguments()
    {
        const string source = """
var numbers: int[] = [1, 2, 3]
var value = numbers[0]
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var elementAccessSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ElementAccessExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IElementAccessOperation>(model.GetOperation(elementAccessSyntax));

        operation.Kind.ShouldBe(OperationKind.ArrayElement);
        operation.Instance.ShouldNotBeNull();
        operation.Arguments.Length.ShouldBe(1);
        operation.Arguments[0].Kind.ShouldBe(OperationKind.Argument);
        operation.Indexer.ShouldBeNull();
    }

    [Fact]
    public void GetOperation_TupleExpression_ExposesElements()
    {
        const string source = """
var tuple = (1, 2)
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var tupleSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<TupleExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<ITupleOperation>(model.GetOperation(tupleSyntax));

        operation.Elements.Length.ShouldBe(2);
        operation.Elements[0].Kind.ShouldBe(OperationKind.Argument);
        operation.Elements[1].Kind.ShouldBe(OperationKind.Argument);
    }

    [Fact]
    public void GetOperation_LambdaExpression_ExposesBodyAndParameters()
    {
        const string source = """
func Apply(value: int, projector: int -> int) -> int {
    return projector(value)
}

var result = Apply(1, v => v + 1)
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<ILambdaOperation>(model.GetOperation(lambdaSyntax));

        operation.Parameters.Length.ShouldBe(1);
        operation.Parameters[0].Name.ShouldBe("v");
        operation.Body.ShouldNotBeNull();
        operation.Body!.Kind.ShouldBe(OperationKind.Binary);
        operation.ReturnType.Name.ShouldBe("Int32");
    }

    [Fact]
    public void GetOperation_TryStatement_ExposesBodyCatchesAndFinally()
    {
        const string source = """
class C {
    Test() {
        try {
            return
        } catch (System.Exception ex) {
            return
        } finally {
            ()
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var tryStatementSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<TryStatementSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<ITryOperation>(model.GetOperation(tryStatementSyntax));

        operation.Body.ShouldNotBeNull();
        operation.Body!.Kind.ShouldBe(OperationKind.Block);
        operation.Catches.Length.ShouldBe(1);
        operation.Catches[0].ExceptionType.Name.ShouldBe("Exception");
        operation.Catches[0].Local.ShouldNotBeNull();
        operation.Catches[0].Body.ShouldNotBeNull();
        operation.Finally.ShouldNotBeNull();
        operation.Finally!.Kind.ShouldBe(OperationKind.Block);
    }

    private static Raven.CodeAnalysis.MetadataReference[] GetReferencesWithRavenCore()
    {
        var corePath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(corePath))
            return TestMetadataReferences.Default;

        return [.. TestMetadataReferences.Default, Raven.CodeAnalysis.MetadataReference.CreateFromFile(corePath)];
    }
}
