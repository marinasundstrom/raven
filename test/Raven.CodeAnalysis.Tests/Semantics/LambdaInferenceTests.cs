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
        return Transform(5, func (delta) => delta + 1)
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

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var parameter = Assert.Single(boundLambda.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameter.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, boundLambda.ReturnType));

        var transformSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .First(f => f.Identifier.Text == "Transform");
        var transformSymbol = Assert.IsType<IMethodSymbol>(model.GetDeclaredSymbol(transformSyntax));
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
}

public class LambdaInferenceDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void Lambda_WithoutDelegateContext_ReportsParameterInferenceError()
    {
        const string code = """
class Container {
    Provide() -> unit {
        let lambda = func (value) => value
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
}
