using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class OptionalParameterSemanticTests : CompilationTestBase
{
    [Fact]
    public void Invocation_OmitsOptionalArgument_UsesDefaultValue()
    {
        const string source = """
class Calculator {
    static Add(value: int = 42) -> int {
        return value
    }
}

let result = Calculator.Add()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Add");

        var boundInvocation = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var arguments = boundInvocation.Arguments.ToArray();

        Assert.Single(arguments);
        var literal = Assert.IsType<BoundLiteralExpression>(arguments[0]);
        Assert.Equal(42, literal.Value);

        var parameter = boundInvocation.Method.Parameters.Single();
        Assert.True(parameter.HasExplicitDefaultValue);
        Assert.Equal(42, parameter.ExplicitDefaultValue);
    }
}
