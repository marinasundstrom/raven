using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class LambdaInferenceTests : CompilationTestBase
{
    [Fact]
    public void Lambda_WithoutParameterTypes_UsesTargetDelegateSignature()
    {
        const string code = """
import System.*
class Calculator {
    Transform(value: int, projector: Func<int, int>) -> int {
        return projector(value)
    }

    Apply() -> int {
        return Transform(5, delta => delta + 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        Assert.False(boundLambda.CandidateDelegates.IsDefaultOrEmpty);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, parameter.Type),
            parameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.NotNull(boundLambda.Body.Type);
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, boundLambda.Body.Type),
            $"{boundLambda.Body.GetType().FullName} - {boundLambda.Body.Type?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}");
        Assert.True(
            SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType),
            boundLambda.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.True(
            SymbolEqualityComparer.Default.Equals(
                compilation.GetTypeByMetadataName("System.Func`2")?.Construct(intType, intType),
                boundLambda.DelegateType),
            boundLambda.DelegateType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var transformSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .First(f => f.Identifier.Text == "Transform");
        var transformSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(transformSyntax));
        var projectorParameter = Assert.Single(transformSymbol.Parameters, p => p.Name == "projector");
        Assert.True(SymbolEqualityComparer.Default.Equals(projectorParameter.Type, boundLambda.DelegateType));
    }

    [Fact]
    public void Lambda_WithConflictingDelegateCandidates_SuppressesParameterInferenceDiagnostic()
    {
        const string code = """
import System.*
class Container {
    Overloaded(pred: Func<int, bool>) -> unit { }
    Overloaded(pred: Func<string, bool>) -> unit { }

    Invoke() -> unit {
        Overloaded(value => true)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => ReferenceEquals(
                diagnostic.Descriptor,
                CompilerDiagnostics.LambdaParameterTypeCannotBeInferred));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var unbound = Assert.IsType<BoundUnboundLambda>(boundLambda.Unbound);
        var suppression = Assert.Single(unbound.SuppressedDiagnostics);
        Assert.Equal("value", suppression.ParameterName);
    }

    [Fact]
    public void Lambda_WithCandidateMissingParameters_StillInfersTypeFromValidDelegate()
    {
        const string code = """
import System.*
class Container {
    Overloaded(projector: Func<int, int>) -> int {
        return projector(1)
    }

    Overloaded(callback: Action) -> int {
        callback()
        return 0
    }

    Invoke() -> int {
        return Overloaded(value => value + 1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<SimpleLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type));
    }

    [Fact]
    public void Lambda_WithoutReturnType_InferredFromBody()
    {
        const string code = """
import System.*
class Container {
    Provide() -> unit {
        let lambda = (value: int) => value + 1
        lambda(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var lambdaSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedLambdaExpressionSyntax>()
            .Single();

        var boundLambda = Assert.IsType<BoundLambdaExpression>(model.GetBoundNode(lambdaSyntax));
        var lambdaSymbol = Assert.IsAssignableFrom<IMethodSymbol>(boundLambda.Symbol);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaSymbol.ReturnType));
    }

    [Fact]
    public void Lambda_WithOverloadSpecificDelegate_RebindsToSelectedMethod()
    {
        const string code = """
import System.*
class Container {
    Overloaded(projector: Func<int, int>, value: int) -> int {
        return projector(value)
    }

    Overloaded(projector: Func<string, string>, value: string) -> string {
        return projector(value)
    }

    Invoke() -> int {
        return Overloaded(value => value + 1, 5)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax { Identifier.ValueText: "Overloaded" });

        var invocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        var method = Assert.IsAssignableFrom<IMethodSymbol>(invocation.Method);

        Assert.Equal("Overloaded", method.Name);

        var funcDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Func`2"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expectedDelegate = Assert.IsAssignableFrom<INamedTypeSymbol>(funcDefinition.Construct(intType, intType));

        var projectorParameter = Assert.Single(method.Parameters, p => p.Name == "projector");
        Assert.Equal(
            expectedDelegate.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat),
            projectorParameter.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var lambdaArgument = Assert.IsAssignableFrom<BoundLambdaExpression>(invocation.Arguments.First());
        Assert.NotNull(lambdaArgument.Unbound);
        Assert.False(lambdaArgument.CandidateDelegates.IsDefaultOrEmpty);
        var lambdaParameter = Assert.Single(lambdaArgument.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, lambdaParameter.Type));
    }

    [Fact]
    public void Lambda_AssignedToLocal_InvocationBindsToDelegateInvoke()
    {
        const string code = """
import System.*
class Calculator {
    Compute() -> int {
        let add = (left: int, right: int) -> int => left + right
        return add(2, 3)
    }
}
""";

        var (compilation, tree) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocationSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single();

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocationSyntax));
        Assert.Equal("Invoke", boundInvocation.Method.Name);

        Assert.NotNull(boundInvocation.Receiver);
        var receiver = boundInvocation.Receiver!;
        var delegateType = Assert.IsAssignableFrom<INamedTypeSymbol>(receiver.Type);
        Assert.Equal(TypeKind.Delegate, delegateType.TypeKind);
        Assert.Same(delegateType.GetDelegateInvokeMethod(), boundInvocation.Method);
    }

    [Fact]
    public void Lambda_ReturningLambda_WithExplicitReturnType_ComposesSuccessfully()
    {
        const string code = """
import System.*
import System.Console.*

let makeAdder = (x: int) -> Func<int, int> => (a: int) => x + a
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void MetadataDelegate_PreservesDelegateTypeKind_WhenConstructed()
    {
        const string code = "class Container { }";

        var (compilation, _) = CreateCompilation(code);

        var definition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Func`2"));
        Assert.Equal(TypeKind.Delegate, definition.TypeKind);

        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(
            compilation.GetSpecialType(SpecialType.System_Int32),
            compilation.GetSpecialType(SpecialType.System_Int32)));
        Assert.Equal(TypeKind.Delegate, constructed.TypeKind);
    }
}

public class LambdaInferenceDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void Lambda_WithoutDelegateContext_ReportsParameterInferenceError()
    {
        const string code = """
class Container {
    Provide() -> unit {
        let lambda = (value) => value
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2200").WithSpan(3, 28, 3, 33).WithArguments("value"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Lambda_WithErrorTypeArgument_DoesNotReportConversionError()
    {
        const string code = """
func apply(value: int, transform: int -> int) -> int {
    transform(value)
}

let doubled = x => x * 2

let result = apply(5, doubled)
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2200").WithSpan(5, 15, 5, 16).WithArguments("x"),
            ]);

        verifier.Verify();
    }
}
