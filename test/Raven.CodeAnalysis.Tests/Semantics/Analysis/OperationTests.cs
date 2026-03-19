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
    public void GetOperation_VariableDeclaration_WithMultipleDeclarators_ReturnsAllDeclarators()
    {
        const string source = """
val first = 1, second = 2
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
            .ShouldBe(new[] { "first", "second" });
        operation.Declarators.Length.ShouldBe(2);
    }

    [Fact]
    public void GetOperation_UseDeclaration_ReturnsDeclarators()
    {
        const string source = """
import System.IO.*

class C {
    func Run() -> unit {
        use stream = MemoryStream()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var declarationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<UseDeclarationStatementSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IVariableDeclarationOperation>(model.GetOperation(declarationSyntax));

        operation.Kind.ShouldBe(OperationKind.LocalDeclaration);
        operation.Declarators.Select(declarator => declarator.Symbol.Name)
            .ShouldBe(new[] { "stream" });
        operation.ChildOperations.Length.ShouldBe(1);
    }

    [Fact]
    public void GetOperation_SpreadElement_ExposesExpression()
    {
        const string source = """
class C {
    func Run() -> unit {
        val items = [1, 2]
        val combined = [0, ...items, 3]
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var collectionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Last();

        var collectionOperation = Assert.IsAssignableFrom<ICollectionOperation>(model.GetOperation(collectionSyntax));
        var operation = collectionOperation.ChildOperations.OfType<ISpreadElementOperation>().Single();

        operation.Expression.ShouldNotBeNull();
        operation.Expression!.Kind.ShouldBe(OperationKind.LocalReference);
        operation.ChildOperations.Length.ShouldBe(1);
    }

    [Fact]
    public void GetOperation_IfPatternStatement_ConditionExposesIsPatternShape()
    {
        const string source = """
class C {
    func Run(person: (string, int)) -> string {
        if val (name, >= 18) = person {
            return name
        }

        return ""
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var ifPatternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfPatternStatementSyntax>()
            .Single();

        var conditional = Assert.IsAssignableFrom<IConditionalOperation>(model.GetOperation(ifPatternSyntax));
        var isPattern = Assert.IsAssignableFrom<IIsPatternOperation>(conditional.Condition);

        isPattern.Value.ShouldNotBeNull();
        isPattern.Pattern.ShouldNotBeNull();
        isPattern.ChildOperations.Length.ShouldBe(2);
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
    func Test(flag: bool) {
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
    func Test(param: int) -> int {
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
    public void GetOperation_MatchExpression_ArmValue_SkipsInternalRequiredResultWrapper()
    {
        const string source = """
var value = 0 match {
    0 => 1
    _ => 2
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var matchExpressionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MatchExpressionSyntax>()
            .Single();

        var matchOperation = Assert.IsAssignableFrom<ISwitchOperation>(model.GetOperation(matchExpressionSyntax));
        var operation = matchOperation.ArmValues[0];
        operation.Kind.ShouldBe(OperationKind.Literal);
        operation.ChildOperations.Length.ShouldBe(0);
    }

    [Fact]
    public void GetOperation_MatchStatement_ExposesSwitchShapeAndArms()
    {
        const string source = """
import System.Console.*

val value = 1

match value {
    1 => WriteLine("one")
    _ => WriteLine("other")
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var matchStatementSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MatchStatementSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<ISwitchOperation>(model.GetOperation(matchStatementSyntax));
        operation.Kind.ShouldBe(OperationKind.Switch);
        operation.Value.ShouldNotBeNull();
        operation.Patterns.Length.ShouldBe(2);
        operation.Guards.Length.ShouldBe(0);
        operation.ArmValues.Length.ShouldBe(2);
        operation.ArmValues[0].Kind.ShouldBe(OperationKind.Invocation);
        operation.ArmValues[1].Kind.ShouldBe(OperationKind.Invocation);
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
    public void GetOperation_NominalDeconstructionPattern_ExposesRecursivePatternShape()
    {
        const string source = """
val value: object = new Person("Ada", 42)
val result = value match {
    Person(val name, val age) => name
    _ => ""
}

record class Person(Name: string, Age: int)
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var patternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<NominalDeconstructionPatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IRecursivePatternOperation>(model.GetOperation(patternSyntax));
        operation.Kind.ShouldBe(OperationKind.RecursivePattern);
        operation.DeconstructMethod.Name.ShouldBe("Deconstruct");
        operation.Arguments.Length.ShouldBe(2);
        operation.Arguments[0].Kind.ShouldBe(OperationKind.DeclarationPattern);
        operation.Arguments[1].Kind.ShouldBe(OperationKind.DeclarationPattern);
    }

    [Fact]
    public void GetOperation_RangePattern_ExposesBoundsAndExclusivity()
    {
        const string source = """
val value: int = 9
val result = value match {
    2..<10 => 1
    _ => 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var patternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<RangePatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IRangePatternOperation>(model.GetOperation(patternSyntax));
        operation.Kind.ShouldBe(OperationKind.RangePattern);
        operation.IsUpperExclusive.ShouldBeTrue();
        operation.LowerBound.ShouldNotBeNull();
        operation.UpperBound.ShouldNotBeNull();
        operation.LowerBound!.Kind.ShouldBe(OperationKind.Literal);
        operation.UpperBound!.Kind.ShouldBe(OperationKind.Literal);
    }

    [Fact]
    public void GetOperation_ComparisonPattern_ExposesOperatorAndValue()
    {
        const string source = """
val value: int = 9
val result = value match {
    > 2 => 1
    _ => 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var patternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ComparisonPatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IComparisonPatternOperation>(model.GetOperation(patternSyntax));
        operation.Kind.ShouldBe(OperationKind.ComparisonPattern);
        operation.OperatorKind.ShouldBe(SyntaxKind.GreaterThanToken);
        operation.Value.ShouldNotBeNull();
        operation.Value!.Kind.ShouldBe(OperationKind.Literal);
    }

    [Fact]
    public void GetOperation_PropertyPattern_ExposesMembersAndSubpatterns()
    {
        const string source = """
val value: object = new Person("Ada", 42)
val result = value match {
    Person { Name: "Ada", Age: _ } => 1
    _ => 0
}

record class Person(Name: string, Age: int)
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var patternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyPatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IPropertyPatternOperation>(model.GetOperation(patternSyntax));
        operation.Kind.ShouldBe(OperationKind.PropertyPattern);
        operation.Members.Length.ShouldBe(2);
        operation.Members[0].Name.ShouldBe("Name");
        operation.Members[1].Name.ShouldBe("Age");
        operation.Subpatterns.Length.ShouldBe(2);
        operation.Subpatterns[0].Kind.ShouldBe(OperationKind.ConstantPattern);
        operation.Subpatterns[1].Kind.ShouldBe(OperationKind.DiscardPattern);
    }

    [Fact]
    public void GetOperation_DictionaryPattern_ExposesKeysAndSubpatterns()
    {
        const string source = """
import System.Collections.Generic.*

val value: Dictionary<string, int> = !["Ada": 42, "Grace": 43]
val result = value match {
    ["Ada": val age, "Grace": _] => 1
    _ => 0
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var patternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<DictionaryPatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IDictionaryPatternOperation>(model.GetOperation(patternSyntax));
        operation.Kind.ShouldBe(OperationKind.DictionaryPattern);
        operation.ReceiverType.Name.ShouldBe("IDictionary");
        operation.KeyType.SpecialType.ShouldBe(SpecialType.System_String);
        operation.ValueType.SpecialType.ShouldBe(SpecialType.System_Int32);
        operation.Designator.ShouldBeNull();
        operation.Keys.Length.ShouldBe(2);
        operation.Keys[0].Kind.ShouldBe(OperationKind.Literal);
        operation.Keys[1].Kind.ShouldBe(OperationKind.Literal);
        operation.Subpatterns.Length.ShouldBe(2);
        operation.Subpatterns[0].Kind.ShouldBe(OperationKind.DeclarationPattern);
        operation.Subpatterns[1].Kind.ShouldBe(OperationKind.DiscardPattern);
    }

    [Fact]
    public void GetOperation_ComparisonPattern_ExposesEqualityOperandOperation()
    {
        const string source = """
var expected = 2
var tuple = (1, 2)
var result = tuple match {
    (val a, == expected) => a
    _ => 0
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var comparisonPatternSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ComparisonPatternSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IComparisonPatternOperation>(model.GetOperation(comparisonPatternSyntax));
        operation.OperatorKind.ShouldBe(SyntaxKind.EqualsEqualsToken);
        operation.Value.ShouldNotBeNull();
        operation.Value!.Kind.ShouldBe(OperationKind.LocalReference);
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
            .OfType<SimpleFunctionExpressionSyntax>()
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
    func Test() {
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

    [Fact]
    public void GetOperation_InterpolatedString_ExposesContentsAndInterpolation()
    {
        const string source = """
func Message(name: string) -> string {
    return "Hello ${name}"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var interpolatedString = tree.GetRoot()
            .DescendantNodes()
            .OfType<InterpolatedStringExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IInterpolatedStringOperation>(model.GetOperation(interpolatedString));
        operation.Kind.ShouldBe(OperationKind.InterpolatedString);
        operation.Contents.Length.ShouldBeGreaterThan(0);

        var interpolation = operation.Contents.OfType<IInterpolationOperation>().Single();
        interpolation.Expression.ShouldNotBeNull();
        interpolation.Expression!.Kind.ShouldBe(OperationKind.ParameterReference);
    }

    [Fact]
    public void GetOperation_NameOfExpression_ReturnsNameOfOperation()
    {
        const string source = """
val memberName = nameof(System.Console.WriteLine)
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var nameofSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<NameOfExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<INameOfOperation>(model.GetOperation(nameofSyntax));
        operation.Kind.ShouldBe(OperationKind.NameOf);
        operation.Name.ShouldBe("WriteLine");
        operation.Operand.ShouldNotBeNull();
    }

    [Fact]
    public void GetOperation_NullCoalesceExpression_ReturnsCoalesceOperation()
    {
        const string source = """
func M(name: string?) -> string {
    return name ?? "fallback"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var nullCoalesceSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<NullCoalesceExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<ICoalesceOperation>(model.GetOperation(nullCoalesceSyntax));
        operation.Kind.ShouldBe(OperationKind.Coalesce);
        operation.Left.ShouldNotBeNull();
        operation.Left!.Kind.ShouldBe(OperationKind.ParameterReference);
        operation.Right.ShouldNotBeNull();
        operation.Right!.Kind.ShouldBe(OperationKind.Literal);
        operation.ChildOperations.Length.ShouldBe(2);
    }

    [Fact]
    public void GetOperation_WithExpression_ReturnsWithOperation()
    {
        const string source = """
record class Person(Name: string, Age: int)

val bob = Person("Bob", 30)
val updated = bob with {
    Age = 31
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var withExpressionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<WithExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IWithOperation>(model.GetOperation(withExpressionSyntax));
        operation.Kind.ShouldBe(OperationKind.With);
        operation.Receiver.ShouldNotBeNull();
        operation.Members.Length.ShouldBe(1);
        operation.Members[0].Name.ShouldBe("Age");
        operation.Values.Length.ShouldBe(1);
        operation.Values[0].Kind.ShouldBe(OperationKind.Literal);
        operation.ChildOperations.Length.ShouldBe(2);
    }

    [Fact]
    public void GetOperation_UnionCaseExpression_CurrentlySurfacesAsConversionToObjectCreation()
    {
        const string source = """
union Error {
    MissingName(message: string)
}

val error: Error = MissingName("x")
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var conversion = Assert.IsAssignableFrom<IConversionOperation>(model.GetOperation(invocationSyntax));
        conversion.Operand.ShouldNotBeNull();
        conversion.Operand!.Kind.ShouldBe(OperationKind.ObjectCreation);
    }

    [Fact]
    public void GetOperation_ReturnExpression_ReturnsReturnExpressionOperation()
    {
        const string source = """
func M(name: string?) -> string {
    val value = name ?? return "fallback"
    return value
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var returnExpressionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IReturnOperation>(model.GetOperation(returnExpressionSyntax));
        operation.Kind.ShouldBe(OperationKind.ReturnExpression);
        operation.ReturnedValue.ShouldNotBeNull();
        operation.ReturnedValue!.Kind.ShouldBe(OperationKind.Literal);
        operation.ChildOperations.Length.ShouldBe(1);
    }

    [Fact]
    public void GetOperation_ThrowExpression_ReturnsThrowExpressionOperation()
    {
        const string source = """
import System.*

func M(name: string?) -> string {
    return name ?? throw InvalidOperationException("missing")
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var throwExpressionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ThrowExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IThrowOperation>(model.GetOperation(throwExpressionSyntax));
        operation.Kind.ShouldBe(OperationKind.ThrowExpression);
        operation.Exception.ShouldNotBeNull();
        operation.Exception!.Kind.ShouldBe(OperationKind.ObjectCreation);
        operation.ChildOperations.Length.ShouldBe(1);
    }

    [Fact]
    public void GetOperation_PropagationExpression_ReturnsPropagationOperation()
    {
        const string source = """
union Option<T> {
    Some(T)
    None
}

func test() -> Option<int> {
    val r = test2()?
    return .Some(r)
}

func test2() -> Option<int> {
    return .Some(1)
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var propagateSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropagateExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IPropagationOperation>(model.GetOperation(propagateSyntax));
        operation.Kind.ShouldBe(OperationKind.Propagate);
        operation.Operand.ShouldNotBeNull();
        operation.Operand!.Kind.ShouldBe(OperationKind.Invocation);
        operation.ChildOperations.Length.ShouldBe(1);
    }

    [Fact]
    public void GetOperation_DereferenceExpression_ReturnsDereferenceOperation()
    {
        const string source = """
class Test {
    unsafe static func Run() -> int {
        var value = 41
        val pointer: *int = &value
        return *pointer
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var dereferenceSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<PrefixOperatorExpressionSyntax>()
            .Single(node => node.Kind == SyntaxKind.DereferenceExpression);

        var operation = Assert.IsAssignableFrom<IDereferenceOperation>(model.GetOperation(dereferenceSyntax));
        operation.Kind.ShouldBe(OperationKind.Dereference);
        operation.Operand.ShouldNotBeNull();
        operation.Operand!.Kind.ShouldBe(OperationKind.LocalReference);
        operation.ChildOperations.Length.ShouldBe(1);
    }

    [Fact]
    public void GetOperation_PointerMemberAccess_ReturnsMemberReferenceOperation()
    {
        const string source = """
struct Holder {
    public field Value: int = 42
}

class Test {
    unsafe static func Run() -> int {
        var holder = Holder()
        val pointer: *Holder = &holder
        return pointer->Value
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var pointerAccessSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(access => access.OperatorToken.Kind == SyntaxKind.ArrowToken);

        var operation = Assert.IsAssignableFrom<IMemberReferenceOperation>(model.GetOperation(pointerAccessSyntax));
        operation.Kind.ShouldBe(OperationKind.FieldReference);
        operation.Symbol.Name.ShouldBe("Value");
    }

    [Fact]
    public void GetOperation_CollectionComprehensionElement_ReturnsComprehensionOperation()
    {
        const string source = """
val numbers = [1, 2, 3, 4]
val result = [for n in numbers if n % 2 == 0 => n * n]
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var collectionSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(collection => collection.Elements.Any(element => element is CollectionComprehensionElementSyntax));

        var collectionOperation = Assert.IsAssignableFrom<ICollectionOperation>(model.GetOperation(collectionSyntax));
        var operation = Assert.IsAssignableFrom<ICollectionComprehensionOperation>(
            Assert.Single(collectionOperation.ChildOperations));
        operation.Kind.ShouldBe(OperationKind.CollectionComprehension);
        operation.Source.ShouldNotBeNull();
        operation.Source!.Kind.ShouldBe(OperationKind.LocalReference);
        operation.Condition.ShouldNotBeNull();
        operation.Condition!.Kind.ShouldBe(OperationKind.Binary);
        operation.Selector.ShouldNotBeNull();
        operation.Selector!.Kind.ShouldBe(OperationKind.Binary);
        operation.IterationLocal.Name.ShouldContain("n");
        operation.ChildOperations.Length.ShouldBe(3);
    }

    [Fact]
    public void GetOperation_DictionaryExpression_ExposesEntrySpreadAndComprehension()
    {
        const string source = """
import System.Collections.Generic.*

val other: Dictionary<string, int> = !["b": 2]
val xs = [1, 2, 3]
val map = [
    "a": 1,
    ...other,
    for x in xs if x > 1 => x.ToString(): x * 10
]
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var dictionarySyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(collection => collection.Elements.Any(element => element is DictionarySpreadElementSyntax or DictionaryComprehensionElementSyntax or SpreadElementSyntax));

        var operation = Assert.IsAssignableFrom<IDictionaryOperation>(model.GetOperation(dictionarySyntax));
        operation.Kind.ShouldBe(OperationKind.Dictionary);
        operation.Elements.Length.ShouldBe(3);

        var entry = Assert.IsAssignableFrom<IDictionaryElementOperation>(operation.Elements[0]);
        entry.Kind.ShouldBe(OperationKind.DictionaryElement);
        entry.Key.ShouldNotBeNull();
        entry.Value.ShouldNotBeNull();
        entry.Key!.Kind.ShouldBe(OperationKind.Literal);
        entry.Value!.Kind.ShouldBe(OperationKind.Literal);

        var spread = Assert.IsAssignableFrom<IDictionarySpreadElementOperation>(operation.Elements[1]);
        spread.Kind.ShouldBe(OperationKind.DictionarySpreadElement);
        spread.Expression.ShouldNotBeNull();
        spread.Expression!.Kind.ShouldBe(OperationKind.LocalReference);

        var comprehension = Assert.IsAssignableFrom<IDictionaryComprehensionOperation>(operation.Elements[2]);
        comprehension.Kind.ShouldBe(OperationKind.DictionaryComprehension);
        comprehension.Source.ShouldNotBeNull();
        comprehension.Source!.Kind.ShouldBe(OperationKind.LocalReference);
        comprehension.Condition.ShouldNotBeNull();
        comprehension.Condition!.Kind.ShouldBe(OperationKind.Binary);
        comprehension.KeySelector.ShouldNotBeNull();
        comprehension.KeySelector!.Kind.ShouldBe(OperationKind.Invocation);
        comprehension.ValueSelector.ShouldNotBeNull();
        comprehension.ValueSelector!.Kind.ShouldBe(OperationKind.Binary);
        comprehension.KeyType.SpecialType.ShouldBe(SpecialType.System_String);
        comprehension.ValueType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void GetOperation_ObjectCreationInitializer_ExposesEntries()
    {
        const string source = """
class Foo {
    var Name: string = ""
}

val foo = Foo {
    Name = "updated"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var creationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var creation = Assert.IsAssignableFrom<IObjectCreationOperation>(model.GetOperation(creationSyntax));
        var operation = Assert.IsAssignableFrom<IObjectInitializerOperation>(creation.Initializer);
        operation.Kind.ShouldBe(OperationKind.ObjectInitializer);
        operation.Entries.Length.ShouldBe(1);

        var entry = Assert.IsAssignableFrom<IObjectInitializerAssignmentOperation>(operation.Entries[0]);
        entry.Kind.ShouldBe(OperationKind.ObjectInitializerAssignment);
        entry.Member.Name.ShouldBe("Name");
        entry.Value.ShouldNotBeNull();
        entry.Value!.Kind.ShouldBe(OperationKind.Literal);
    }

    [Fact]
    public void GetOperation_ObjectCreation_ExposesInitializerOperation()
    {
        const string source = """
class Foo {
    var Name: string = ""
}

val foo = Foo {
    Name = "updated"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var creationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var operation = Assert.IsAssignableFrom<IObjectCreationOperation>(model.GetOperation(creationSyntax));
        operation.Kind.ShouldBe(OperationKind.ObjectCreation);
        operation.Initializer.ShouldNotBeNull();
        Assert.IsAssignableFrom<IObjectInitializerOperation>(operation.Initializer);
    }

    [Fact]
    public void GetOperation_MethodGroupWithOverloads_DoesNotThrow()
    {
        const string source = """
class C {
    func M(value: int) -> unit {}
    func M(value: string) -> unit {}

    func Test() -> unit {
        val methodRef = M
    }
}
""";

        var (compilation, tree) = CreateCompilation(source, references: GetReferencesWithRavenCore());
        var model = compilation.GetSemanticModel(tree);
        var methodIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(node => node.Identifier.ValueText == "M");

        var operation = Record.Exception(() => model.GetOperation(methodIdentifier));
        operation.ShouldBeNull();

        var methodReference = Assert.IsAssignableFrom<IMethodReferenceOperation>(model.GetOperation(methodIdentifier));
        methodReference.Method.Name.ShouldBe("M");
    }

    private static Raven.CodeAnalysis.MetadataReference[] GetReferencesWithRavenCore()
    {
        var corePath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(corePath))
            return TestMetadataReferences.Default;

        return [.. TestMetadataReferences.Default, Raven.CodeAnalysis.MetadataReference.CreateFromFile(corePath)];
    }
}
