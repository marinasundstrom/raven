using System;
using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Symbols;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BinderAndLowererTests : CompilationTestBase
{
    [Fact]
    public void IfExpression_WithElse_BindsToBoundIfExpression()
    {
        const string source = """
val flag = true
val value = if flag 1 else 2
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var ifExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfExpressionSyntax>()
            .Single();

        var boundIf = Assert.IsType<BoundIfExpression>(model.GetBoundNode(ifExpression));
        var thenBranch = Assert.IsType<BoundConversionExpression>(boundIf.ThenBranch);
        var elseBranch = Assert.IsType<BoundConversionExpression>(boundIf.ElseBranch);
        Assert.IsType<BoundLiteralExpression>(thenBranch.Expression);
        Assert.IsType<BoundLiteralExpression>(elseBranch.Expression);
    }

    [Fact]
    public void MatchExpression_WithWildcard_BindsToBoundMatchExpression()
    {
        const string source = """
val value = 0 match {
    0 => 1
    _ => 2
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var matchExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<MatchExpressionSyntax>()
            .Single();

        var boundMatch = Assert.IsType<BoundMatchExpression>(model.GetBoundNode(matchExpression));
        Assert.Equal(2, boundMatch.Arms.Length);
        Assert.IsType<BoundLiteralExpression>(boundMatch.Expression);
        Assert.All(boundMatch.Arms, arm =>
        {
            var required = Assert.IsType<BoundRequiredResultExpression>(arm.Expression);
            Assert.IsType<BoundLiteralExpression>(required.Operand);
        });
        Assert.IsType<BoundConstantPattern>(boundMatch.Arms[0].Pattern);
        Assert.IsType<BoundDiscardPattern>(boundMatch.Arms[1].Pattern);
    }

    [Fact]
    public void Lowerer_IfStatementWithoutElse_RewritesToConditionalGoto()
    {
        const string source = """
class C {
    Test(flag: bool) {
        if flag {
            ()
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Test");

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;
        var boundIf = boundBody.Statements.OfType<BoundIfStatement>().Single();

        var lowered = Assert.IsType<BoundBlockStatement>(Lowerer.LowerStatement(methodSymbol, boundIf));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(3, statements.Length);

        var conditionalGoto = Assert.IsType<BoundConditionalGotoStatement>(statements[0]);
        Assert.False(conditionalGoto.JumpIfTrue);

        var endLabel = Assert.IsType<BoundLabeledStatement>(statements[2]);
        Assert.Same(endLabel.Label, conditionalGoto.Target);
    }

    [Fact]
    public void Lowerer_WhileStatement_RewritesToLoopControlFlow()
    {
        const string source = """
class C {
    Test(flag: bool) {
        while flag {
            break
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Test");

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;
        var boundWhile = boundBody.Statements.OfType<BoundWhileStatement>().Single();

        var lowered = Assert.IsType<BoundBlockStatement>(Lowerer.LowerStatement(methodSymbol, boundWhile));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(4, statements.Length);

        var continueLabel = Assert.IsType<BoundLabeledStatement>(statements[0]);
        var continueBlock = Assert.IsType<BoundBlockStatement>(continueLabel.Statement);
        var continueStatements = continueBlock.Statements.ToArray();
        var guard = Assert.IsType<BoundConditionalGotoStatement>(continueStatements.Single());
        Assert.False(guard.JumpIfTrue);

        var bodyBlock = Assert.IsType<BoundBlockStatement>(statements[1]);
        var bodyGoto = Assert.IsType<BoundGotoStatement>(bodyBlock.Statements.Single());

        var loopGoto = Assert.IsType<BoundGotoStatement>(statements[2]);
        Assert.True(loopGoto.IsBackward);
        Assert.Same(continueLabel.Label, loopGoto.Target);

        var breakLabel = Assert.IsType<BoundLabeledStatement>(statements[3]);
        Assert.Same(breakLabel.Label, bodyGoto.Target);
    }

    [Fact]
    public void Lowerer_LambdaBody_RewritesNestedLoop()
    {
        const string source = """
class C {
    Test(flag: bool) {
        val lambda = () => {
            while flag {
                ()
            }
        }

        lambda()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Test");

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;
        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);

        var lambda = loweredBody.Statements
            .OfType<BoundLocalDeclarationStatement>()
            .SelectMany(s => s.Declarators)
            .Select(d => d.Initializer)
            .OfType<BoundLambdaExpression>()
            .Single();

        var lambdaBlock = Assert.IsType<BoundBlockExpression>(lambda.Body);
        var inspector = new LambdaLoweringInspector();
        inspector.VisitBlockExpression(lambdaBlock);

        Assert.False(inspector.SeenWhile);
        Assert.True(inspector.GotoCount > 0);
    }

    [Fact]
    public void Lowerer_FunctionStatement_RemainsFunctionStatement()
    {
        const string source = """
class C {
    Test() {
        func nested() {
            ()
        }

        nested()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Test");

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var boundBody = (BoundBlockStatement)model.GetBoundNode(methodSyntax.Body!)!;

        var originalFunction = boundBody.Statements.OfType<BoundFunctionStatement>().Single();

        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);
        var loweredFunction = loweredBody.Statements.OfType<BoundFunctionStatement>().Single();

        Assert.Same(originalFunction.Method, loweredFunction.Method);
    }

    private sealed class LambdaLoweringInspector : BoundTreeWalker
    {
        public bool SeenWhile { get; private set; }
        public int GotoCount { get; private set; }

        public override void VisitWhileStatement(BoundWhileStatement node)
        {
            SeenWhile = true;
        }

        public override void VisitGotoStatement(BoundGotoStatement node)
        {
            GotoCount++;
            base.VisitGotoStatement(node);
        }
    }
}
