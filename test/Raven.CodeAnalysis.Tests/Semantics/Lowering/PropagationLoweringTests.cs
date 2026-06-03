using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Lowering.Tests;

public sealed class PropagationLoweringTests : CompilationTestBase
{
    [Fact]
    public void Lowerer_PropagateLocalDeclaration_RewritesIntoCarrierCheckAndSuccessLocal()
    {
        const string source = """
import System.*

class C {
    func Test() -> Result<int, string> {
        val value = Parse()?
        return .Ok(value)
    }

    func Parse() -> Result<int, string> {
        return .Ok(1)
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");

        var lowered = Lowerer.LowerBlock(methodSymbol, boundBody);
        var statements = lowered.Statements.ToArray();

        Assert.Empty(CollectPropagateExpressions(lowered));

        Assert.True(statements.Length >= 6);

        var operandDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        Assert.Contains("propagateOperand", operandDeclaration.Declarators.Single().Local.Name, StringComparison.Ordinal);

        Assert.IsType<BoundAssignmentStatement>(statements[1]);

        var okDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[2]);
        Assert.Contains("propagateOk", okDeclaration.Declarators.Single().Local.Name, StringComparison.Ordinal);

        var carrierCheck = Assert.IsType<BoundIfStatement>(statements[3]);
        var tryGetValue = Assert.IsType<BoundInvocationExpression>(carrierCheck.Condition);
        Assert.Equal("TryGetValue", tryGetValue.Method.Name);
        Assert.Empty(Assert.IsType<BoundBlockStatement>(carrierCheck.ThenNode).Statements);
        Assert.Contains(
            CollectReturnStatements(Assert.IsType<BoundBlockStatement>(carrierCheck.ElseNode!)),
            statement => statement.Expression?.Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat) == "Result<int, string>");

        var valueDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[4]);
        var valueDeclarator = Assert.Single(valueDeclaration.Declarators);
        Assert.Equal("value", valueDeclarator.Local.Name);
        Assert.IsNotType<BoundPropagateExpression>(valueDeclarator.Initializer);
        Assert.Equal("int", valueDeclarator.Local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        Assert.IsType<BoundReturnStatement>(statements.Last());
    }

    private (IMethodSymbol Method, BoundBlockStatement Body) BindMethodBody(string source, string methodName)
    {
        var (compilation, tree) = CreateCompilation(source, references: TestMetadataReferences.DefaultWithRavenCore);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(method => method.Identifier.Text == methodName);

        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        return (methodSymbol, boundBody);
    }

    private static BoundPropagateExpression[] CollectPropagateExpressions(BoundBlockStatement block)
    {
        var collector = new PropagateExpressionCollector();
        collector.VisitBlockStatement(block);
        return collector.Expressions.ToArray();
    }

    private static BoundReturnStatement[] CollectReturnStatements(BoundBlockStatement block)
    {
        var collector = new ReturnStatementCollector();
        collector.VisitBlockStatement(block);
        return collector.Statements.ToArray();
    }

    private sealed class PropagateExpressionCollector : BoundTreeWalker
    {
        public List<BoundPropagateExpression> Expressions { get; } = [];

        public override void VisitPropagateExpression(BoundPropagateExpression node)
        {
            Expressions.Add(node);
            base.VisitPropagateExpression(node);
        }
    }

    private sealed class ReturnStatementCollector : BoundTreeWalker
    {
        public List<BoundReturnStatement> Statements { get; } = [];

        public override void VisitReturnStatement(BoundReturnStatement node)
        {
            Statements.Add(node);
            base.VisitReturnStatement(node);
        }
    }
}
