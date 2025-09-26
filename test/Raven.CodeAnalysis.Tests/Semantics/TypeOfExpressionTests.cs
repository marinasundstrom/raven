using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeOfExpressionTests : CompilationTestBase
{
    [Fact]
    public void TypeOfExpression_ReturnsSystemType()
    {
        var (compilation, tree) = CreateCompilation("let t = typeof(int)");
        var model = compilation.GetSemanticModel(tree);
        var typeOfExpression = tree.GetRoot().DescendantNodes().OfType<TypeOfExpressionSyntax>().Single();

        var typeInfo = model.GetTypeInfo(typeOfExpression);
        Assert.Equal(SpecialType.System_Type, typeInfo.Type!.SpecialType);

        var operandType = model.GetTypeInfo(typeOfExpression.Type).Type;
        Assert.Equal(SpecialType.System_Int32, operandType!.SpecialType);
    }
}
