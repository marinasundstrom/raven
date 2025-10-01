using System;
using System.IO;
using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
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
    public void TypeOfExpression_MethodReturnTypeAndOperandAgree()
    {
        const string source = """
class TypeInspector {
    static GetIntType() -> System.Type {
        typeof(int)
    }

    static GetListType() -> System.Type {
        typeof(System.Collections.Generic.List<int>)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var typeOfExpressions = tree.GetRoot().DescendantNodes().OfType<TypeOfExpressionSyntax>().ToArray();

        var type = (INamedTypeSymbol)compilation.GlobalNamespace.LookupType("TypeInspector")!;

        Assert.Collection(typeOfExpressions,
            expr => AssertTypeOf(model, compilation, type, expr, "GetIntType"),
            expr => AssertTypeOf(model, compilation, type, expr, "GetListType"));

        Assert.Empty(compilation.GetDiagnostics());

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
    }

    private static void AssertTypeOf(
        SemanticModel model,
        Compilation compilation,
        INamedTypeSymbol type,
        TypeOfExpressionSyntax expression,
        string methodName)
    {
        var typeInfo = model.GetTypeInfo(expression);
        var method = type.GetMembers(methodName).OfType<IMethodSymbol>().Single();

        Assert.True(SymbolEqualityComparer.Default.Equals(method.ReturnType, typeInfo.Type));
        var conversion = compilation.ClassifyConversion(typeInfo.Type!, method.ReturnType);
        Assert.True(conversion.Exists && conversion.IsImplicit);
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

    [Fact]
    public void TypeOfExpression_WithFullyQualifiedSystemType_BindsSuccessfully()
    {
        var (compilation, tree) = CreateCompilation("let t = typeof(System.Object)");
        var model = compilation.GetSemanticModel(tree);
        var typeOfExpression = tree.GetRoot().DescendantNodes().OfType<TypeOfExpressionSyntax>().Single();

        var typeInfo = model.GetTypeInfo(typeOfExpression);
        Assert.Equal(SpecialType.System_Type, typeInfo.Type!.SpecialType);

        var operandType = model.GetTypeInfo(typeOfExpression.Type).Type;
        Assert.Equal(SpecialType.System_Object, operandType!.SpecialType);

        Assert.Empty(compilation.GetDiagnostics());
    }
}
