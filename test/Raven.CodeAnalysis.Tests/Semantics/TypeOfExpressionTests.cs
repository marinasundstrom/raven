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

    [Fact]
    public void TypeOfExpression_BindsBoundNodeWithOperandType()
    {
        var (compilation, tree) = CreateCompilation("let t = typeof(int)");
        var model = compilation.GetSemanticModel(tree);
        var typeOfExpression = tree.GetRoot().DescendantNodes().OfType<TypeOfExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundTypeOfExpression>(model.GetBoundNode(typeOfExpression));

        Assert.Equal(SpecialType.System_Type, bound.Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, bound.OperandType.SpecialType);
    }

    [Fact]
    public void TypeOfExpression_WithNamespaceOperand_ReportsNamespaceUsedLikeAType()
    {
        var (compilation, tree) = CreateCompilation("let t = typeof(System.Collections)");

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        var typeSyntax = tree.GetRoot().DescendantNodes().OfType<TypeOfExpressionSyntax>().Single().Type;
        Assert.Equal("RAV0119", diagnostic.Descriptor.Id);
        Assert.Equal($"'{typeSyntax}' is a namespace but is used like a type", diagnostic.GetMessage());
    }
}
