using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

using PublicOperatorKind = Raven.CodeAnalysis.Operations.BinaryOperatorKind;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BinaryOperationContractTests : CompilationTestBase
{
    [Theory]
    [InlineData("+", PublicOperatorKind.Add)]
    [InlineData("-", PublicOperatorKind.Subtract)]
    [InlineData("*", PublicOperatorKind.Multiply)]
    [InlineData("/", PublicOperatorKind.Divide)]
    [InlineData("%", PublicOperatorKind.Remainder)]
    public void PrimitiveOperatorExposesBoundMeaning(string token, PublicOperatorKind expected)
    {
        var (compilation, tree) = CreateCompilation($"func Combine(left: int, right: int) -> int {{ return left {token} right }}");
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IBinaryOperation>(model.GetOperation(syntax));
        Assert.Equal(expected, operation.OperatorKind);
        Assert.False(operation.IsLifted);
        Assert.False(operation.IsChecked);
        Assert.Null(operation.OperatorMethod);
        Assert.Same(operation, model.GetOperation(syntax));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NullableOperatorSeparatesLiftingFromOperatorKind()
    {
        var (compilation, tree) = CreateCompilation("func Combine(left: int?, right: int?) -> int? { return left + right }");
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IBinaryOperation>(model.GetOperation(syntax));
        Assert.Equal(PublicOperatorKind.Add, operation.OperatorKind);
        Assert.True(operation.IsLifted);
        Assert.False(operation.IsChecked);
        Assert.Null(operation.OperatorMethod);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void ImportedOperatorExposesSelectedMethod()
    {
        var (compilation, tree) = CreateCompilation("func Combine(left: System.DateTime, right: System.TimeSpan) -> System.DateTime { return left + right }");
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        // Bound user operators can already be represented as invocation operations.
        // Consumers must handle that semantic shape rather than guessing from '+'.
        var operation = Assert.IsAssignableFrom<IInvocationOperation>(model.GetOperation(syntax));
        Assert.Equal("op_Addition", operation.TargetMethod.Name);
        Assert.Equal("DateTime", operation.TargetMethod.ContainingType!.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void CoalescingDoesNotPretendToBeAnOrdinaryBinaryOperator()
    {
        var (compilation, tree) = CreateCompilation("func Choose(left: int?, right: int) -> int { return left ?? right }");
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<NullCoalesceExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<ICoalesceOperation>(model.GetOperation(syntax));
        Assert.Equal(PublicOperatorKind.None, operation.OperatorKind);
        Assert.False(operation.IsChecked);
        Assert.False(operation.IsLifted);
        Assert.Null(operation.OperatorMethod);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
    [Fact]
    public void StaticInvocationHasNoInstance()
    {
        var (compilation, tree) = CreateCompilation("func Absolute(value: int) -> int { return System.Math.Abs(value) }");
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IInvocationOperation>(model.GetOperation(syntax));
        Assert.True(operation.TargetMethod.IsStatic);
        Assert.Null(operation.Instance);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void InstanceInvocationExposesReceiverRatherThanMethodGroup()
    {
        var (compilation, tree) = CreateCompilation("func Format(value: int) -> string { return value.ToString() }");
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IInvocationOperation>(model.GetOperation(syntax));
        var receiver = Assert.IsAssignableFrom<IParameterReferenceOperation>(operation.Instance);
        Assert.Equal("value", receiver.Parameter.Name);
        Assert.Same(receiver, operation.Instance);
        Assert.Contains(receiver, operation.ChildOperations);
        Assert.Same(operation, receiver.Parent);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

}
