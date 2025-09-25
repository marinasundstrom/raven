using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionMethodSemanticTests : CompilationTestBase
{
    [Fact]
    public void MemberAccess_WithNamespaceImport_BindsExtensionInvocation()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

class Query {
    Run() -> int {
        let items = List<int>()
        items.Add(1)
        items.Add(2)
        return items.Where(func (value) => value > 0).Count()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var whereInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Where");

        var boundWhere = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(whereInvocation));
        Assert.True(boundWhere.Method.IsExtensionMethod);
        Assert.Equal(
            "global::System.Linq.Enumerable",
            boundWhere.Method.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var whereArguments = boundWhere.Arguments.ToArray();
        Assert.Equal(2, whereArguments.Length);
        Assert.Same(boundWhere.Receiver, whereArguments[0]);
        Assert.IsType<BoundLambdaExpression>(whereArguments[1]);

        var countInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Count");

        var boundCount = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(countInvocation));
        Assert.True(boundCount.Method.IsExtensionMethod);
        Assert.Equal(
            "global::System.Linq.Enumerable",
            boundCount.Method.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void MemberAccess_WithTypeImport_BindsExtensionInvocation()
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.Enumerable

let items = List<int>()
items.Add(1)
let hasItems = items.Any()
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var anyInvocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(node => node.Expression is MemberAccessExpressionSyntax member && member.Name.Identifier.Text == "Any");

        var boundAny = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(anyInvocation));
        Assert.True(boundAny.Method.IsExtensionMethod);
        Assert.Equal(
            "global::System.Linq.Enumerable",
            boundAny.Method.ContainingType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }
}
