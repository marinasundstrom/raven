using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExpressionTreeLoweringTests : CompilationTestBase
{
    [Fact]
    public void Lowerer_LambdaToExpressionTree_RewritesToExpressionFactoryCalls()
    {
        const string source = """
import System.*
import System.Linq.Expressions.*

class C {
    Build() -> Expression<System.Func<int, bool>> {
        val tree: Expression<System.Func<int, bool>> = (x: int) => x > 0
        return tree
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Build");
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));

        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));
        var localDeclaration = boundBody.Statements
            .OfType<BoundLocalDeclarationStatement>()
            .Single();
        var initializer = localDeclaration.Declarators.Single().Initializer;
        var conversion = Assert.IsType<BoundConversionExpression>(initializer);
        Assert.IsType<BoundLambdaExpression>(conversion.Expression);

        var lowered = Lowerer.LowerExpression(methodSymbol, conversion);
        var loweredBlock = Assert.IsType<BoundBlockExpression>(lowered);

        var collector = new InvocationCollector();
        collector.VisitBlockExpression(loweredBlock);

        Assert.Contains("Parameter", collector.MethodNames);
        Assert.Contains("Constant", collector.MethodNames);
        Assert.Contains("GreaterThan", collector.MethodNames);
        Assert.Contains("Lambda", collector.MethodNames);
    }

    private sealed class InvocationCollector : BoundTreeWalker
    {
        public HashSet<string> MethodNames { get; } = new(StringComparer.Ordinal);

        public override void VisitInvocationExpression(BoundInvocationExpression node)
        {
            MethodNames.Add(node.Method.Name);
            base.VisitInvocationExpression(node);
        }
    }
}
