using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Lowering.Tests;

public sealed class ControlFlowLoweringTests : CompilationTestBase
{
    [Fact]
    public void Lowerer_IfStatementWithoutElse_RewritesToConditionalGoto()
    {
        const string source = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        }
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");
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
    public void Lowerer_IfStatementWithElse_RewritesToConditionalGotoAndLabels()
    {
        const string source = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        } else {
            ()
        }
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");
        var boundIf = boundBody.Statements.OfType<BoundIfStatement>().Single();

        var lowered = Assert.IsType<BoundBlockStatement>(Lowerer.LowerStatement(methodSymbol, boundIf));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(5, statements.Length);

        var conditionalGoto = Assert.IsType<BoundConditionalGotoStatement>(statements[0]);
        Assert.False(conditionalGoto.JumpIfTrue);

        Assert.IsType<BoundBlockStatement>(statements[1]);

        var gotoEnd = Assert.IsType<BoundGotoStatement>(statements[2]);
        var elseLabel = Assert.IsType<BoundLabeledStatement>(statements[3]);
        var endLabel = Assert.IsType<BoundLabeledStatement>(statements[4]);

        Assert.Same(elseLabel.Label, conditionalGoto.Target);
        Assert.Same(endLabel.Label, gotoEnd.Target);
        Assert.IsType<BoundBlockStatement>(elseLabel.Statement);
    }

    [Fact]
    public void Lowerer_WhileStatement_RewritesToLoopControlFlow()
    {
        const string source = """
class C {
    func Test(flag: bool) {
        while flag {
            break
        }
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");
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
    public void Lowerer_WhileStatement_RewritesContinueToBackwardGoto()
    {
        const string source = """
class C {
    func Test(flag: bool) {
        while flag {
            continue
        }
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");
        var boundWhile = boundBody.Statements.OfType<BoundWhileStatement>().Single();

        var lowered = Assert.IsType<BoundBlockStatement>(Lowerer.LowerStatement(methodSymbol, boundWhile));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(4, statements.Length);

        var continueLabel = Assert.IsType<BoundLabeledStatement>(statements[0]);
        var bodyBlock = Assert.IsType<BoundBlockStatement>(statements[1]);
        var continueGoto = Assert.IsType<BoundGotoStatement>(Assert.Single(bodyBlock.Statements));
        Assert.True(continueGoto.IsBackward);
        Assert.Same(continueLabel.Label, continueGoto.Target);

        var loopGoto = Assert.IsType<BoundGotoStatement>(statements[2]);
        Assert.True(loopGoto.IsBackward);
        Assert.Same(continueLabel.Label, loopGoto.Target);

        Assert.IsType<BoundLabeledStatement>(statements[3]);
    }

    [Fact]
    public void Lowerer_LoopStatement_RewritesToUnconditionalLoopControlFlow()
    {
        const string source = """
class C {
    func Test() {
        loop {
            break
        }
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");
        var boundLoop = boundBody.Statements.OfType<BoundLoopStatement>().Single();

        var lowered = Assert.IsType<BoundBlockStatement>(Lowerer.LowerStatement(methodSymbol, boundLoop));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(4, statements.Length);

        var continueLabel = Assert.IsType<BoundLabeledStatement>(statements[0]);
        var bodyBlock = Assert.IsType<BoundBlockStatement>(statements[1]);
        var breakGoto = Assert.IsType<BoundGotoStatement>(bodyBlock.Statements.Single());

        var loopGoto = Assert.IsType<BoundGotoStatement>(statements[2]);
        Assert.True(loopGoto.IsBackward);
        Assert.Same(continueLabel.Label, loopGoto.Target);

        var breakLabel = Assert.IsType<BoundLabeledStatement>(statements[3]);
        Assert.Same(breakLabel.Label, breakGoto.Target);
    }

    [Fact]
    public void Lowerer_LoopStatement_RewritesContinueToBackwardGoto()
    {
        const string source = """
class C {
    func Test() {
        loop {
            continue
        }
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");
        var boundLoop = boundBody.Statements.OfType<BoundLoopStatement>().Single();

        var lowered = Assert.IsType<BoundBlockStatement>(Lowerer.LowerStatement(methodSymbol, boundLoop));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(4, statements.Length);

        var continueLabel = Assert.IsType<BoundLabeledStatement>(statements[0]);
        var bodyBlock = Assert.IsType<BoundBlockStatement>(statements[1]);
        var continueGoto = Assert.IsType<BoundGotoStatement>(Assert.Single(bodyBlock.Statements));
        Assert.True(continueGoto.IsBackward);
        Assert.Same(continueLabel.Label, continueGoto.Target);

        var loopGoto = Assert.IsType<BoundGotoStatement>(statements[2]);
        Assert.True(loopGoto.IsBackward);
        Assert.Same(continueLabel.Label, loopGoto.Target);

        Assert.IsType<BoundLabeledStatement>(statements[3]);
    }

    [Fact]
    public void Lowerer_LambdaBody_RewritesNestedLoop()
    {
        const string source = """
class C {
    func Test(flag: bool) {
        val lambda = () => {
            while flag {
                ()
            }
        }

        lambda()
    }
}
""";

        var (methodSymbol, boundBody) = BindMethodBody(source, "Test");

        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);
        var lambda = loweredBody.Statements
            .OfType<BoundLocalDeclarationStatement>()
            .SelectMany(s => s.Declarators)
            .Select(d => d.Initializer)
            .OfType<BoundFunctionExpression>()
            .Single();

        var lambdaBlock = Assert.IsType<BoundBlockExpression>(lambda.Body);
        var inspector = new LambdaLoweringInspector();
        inspector.VisitBlockExpression(lambdaBlock);

        Assert.False(inspector.SeenWhile);
        Assert.True(inspector.GotoCount > 0);
    }

    private (IMethodSymbol Method, BoundBlockStatement Body) BindMethodBody(string source, string methodName)
    {
        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == methodName);

        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        return (methodSymbol, boundBody);
    }

    private sealed class LambdaLoweringInspector : BoundTreeWalker
    {
        public bool SeenWhile { get; private set; }
        public int GotoCount { get; private set; }

        public override void VisitWhileStatement(BoundWhileStatement node)
        {
            SeenWhile = true;
        }

        public override void VisitLoopStatement(BoundLoopStatement node)
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
