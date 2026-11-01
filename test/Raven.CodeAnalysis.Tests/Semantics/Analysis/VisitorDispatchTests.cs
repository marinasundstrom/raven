using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using GreenSyntaxNode = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode;
using GreenSyntaxRewriter = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxRewriter;
using GreenSyntaxToken = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class VisitorDispatchTests : CompilationTestBase
{
    [Fact]
    public void BoundVisitor_VisitExpression_DispatchesToLiteralHandler()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var expression = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 42, intType);
        var visitor = new RecordingBoundVisitor();

        visitor.VisitExpression(expression);

        Assert.True(visitor.SawLiteralExpression);
    }

    [Fact]
    public void BoundVisitor_VisitStatement_DispatchesToReturnHandler()
    {
        var statement = new BoundReturnStatement(expression: null);
        var visitor = new RecordingBoundVisitor();

        visitor.VisitStatement(statement);

        Assert.True(visitor.SawReturnStatement);
    }

    [Fact]
    public void BoundRewriter_VisitExpression_DispatchesPatternFromExpressionPath()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var pattern = new BoundDeclarationPattern(intType, new BoundDiscardDesignator(intType));
        var rewriter = new RecordingBoundRewriter();

        var rewritten = rewriter.VisitExpression(pattern);

        Assert.True(rewriter.SawDeclarationPattern);
        Assert.NotNull(rewritten);
    }

    [Fact]
    public void BoundRewriter_BaseVisitorReference_PreservesSpecialDispatch()
    {
        var compilation = CreateCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var pattern = new BoundDeclarationPattern(intType, new BoundDiscardDesignator(intType));
        var breakStatement = new BoundBreakStatement();
        var continueStatement = new BoundContinueStatement();
        var rewriter = new RecordingBoundRewriter();
        BoundTreeVisitor<BoundNode?> visitor = rewriter;

        var rewrittenPattern = visitor.VisitExpression(pattern);
        var rewrittenBreak = visitor.VisitStatement(breakStatement);
        var rewrittenContinue = visitor.VisitStatement(continueStatement);

        Assert.True(rewriter.SawDeclarationPattern);
        Assert.Same(pattern, rewrittenPattern);
        Assert.Same(breakStatement, rewrittenBreak);
        Assert.Same(continueStatement, rewrittenContinue);
    }

    [Fact]
    public void BoundTreeWalker_VisitHooksOverrideVisitorContract()
    {
        var visitorMethods = typeof(BoundTreeVisitor)
            .GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.DeclaredOnly)
            .Where(static method => method.Name.StartsWith("Visit", StringComparison.Ordinal))
            .ToDictionary(
                static method => (method.Name, ParameterType: method.GetParameters().Single().ParameterType));
        var walkerMethods = typeof(BoundTreeWalker)
            .GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.DeclaredOnly)
            .Where(static method => method.Name.StartsWith("Visit", StringComparison.Ordinal));

        foreach (var walkerMethod in walkerMethods)
        {
            var parameterType = walkerMethod.GetParameters().Single().ParameterType;
            if (!visitorMethods.TryGetValue((walkerMethod.Name, parameterType), out var visitorMethod))
                continue;

            Assert.Equal(visitorMethod, walkerMethod.GetBaseDefinition());
        }
    }

    [Fact]
    public void BoundTreeWalker_Visit_DispatchesAllBoundNodeFamiliesThroughGeneratedVisitor()
    {
        var compilation = CreateCompilation();
        var expression = new BoundDefaultValueExpression(
            compilation.GetSpecialType(SpecialType.System_Int32));
        var statement = new BoundReturnStatement(expression);
        var walker = new RecordingBoundTreeWalker();

        walker.Visit(expression);
        walker.Visit(statement);

        Assert.True(walker.SawDefaultValueExpression);
        Assert.True(walker.SawReturnStatement);
    }

    [Fact]
    public void SyntaxVisitor_VisitExpression_DispatchesToBinaryHandler()
    {
        var tree = SyntaxTree.ParseText("let x = 1 + 2;");
        var expression = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var visitor = new RecordingSyntaxVisitor();

        visitor.VisitExpression(expression);

        Assert.True(visitor.SawBinaryExpression);
    }

    [Fact]
    public void SyntaxRewriter_VisitStatement_DispatchesToIfHandler()
    {
        var tree = SyntaxTree.ParseText("if true { }");
        var statement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var rewriter = new RecordingSyntaxRewriter();

        var rewritten = rewriter.VisitStatement(statement);

        Assert.True(rewriter.SawIfStatement);
        Assert.NotNull(rewritten);
    }

    [Fact]
    public void SyntaxRewriter_NonNullNodeMayBeRemoved()
    {
        var tree = SyntaxTree.ParseText("if true { }");
        var statement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var rewriter = new RemovingSyntaxRewriter();

        var rewritten = rewriter.Visit(statement);

        Assert.Null(rewritten);
    }

    [Fact]
    public void GreenSyntaxRewriter_NodeVisit_UsesConcreteTokenRewriteHook()
    {
        var tree = SyntaxTree.ParseText("let value = 42");
        var root = Assert.IsAssignableFrom<GreenSyntaxNode>(tree.GetRoot().Green);
        var rewriter = new RecordingGreenSyntaxRewriter();

        var rewritten = rewriter.Visit(root);

        Assert.NotNull(rewritten);
        Assert.True(rewriter.TokenCount > 0);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void GreenSyntaxRewriter_UnchangedTreeRetainsIdentity()
    {
        var tree = SyntaxTree.ParseText("let value = 42");
        var root = Assert.IsAssignableFrom<GreenSyntaxNode>(tree.GetRoot().Green);
        Assert.Same(root, new RecordingGreenSyntaxRewriter().Visit(root));
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void GreenSyntaxRewriter_ListUsesConcreteTokenHookAndRetainsIdentity()
    {
        var token = new GreenSyntaxToken(SyntaxKind.IdentifierToken, "value");
        var list = new Syntax.InternalSyntax.SyntaxList([token]);
        var rewriter = new RecordingGreenSyntaxRewriter();
        var result = rewriter.VisitList(list);
        Assert.Equal(1, rewriter.TokenCount);
        Assert.Same(list, result);
        Assert.Same(token, result![0]);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void GreenSyntaxRewriter_ListReplacementAndRemovalRetainAnnotationsAndOrder()
    {
        var tree = SyntaxTree.ParseText("let value = 42");
        var literal = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single().Green;
        var token = new GreenSyntaxToken(SyntaxKind.IdentifierToken, "value");
        var annotation = new SyntaxAnnotation("rewrite");
        var diagnostic = Syntax.InternalSyntax.DiagnosticInfo.Create(CompilerDiagnostics.IdentifierExpected, new TextSpan(0, 0));
        var list = new Syntax.InternalSyntax.SyntaxList([token, literal, token], diagnostics: [diagnostic], annotations: [annotation]);
        var replacement = new GreenSyntaxToken(SyntaxKind.IdentifierToken, "updated");
        var rewritten = new ReplacingGreenSyntaxRewriter(replacement).VisitList(list)!;
        Assert.NotSame(list, rewritten);
        Assert.Equal(2, rewritten.SlotCount);
        Assert.Same(replacement, rewritten[0]);
        Assert.Same(replacement, rewritten[1]);
        Assert.Same(annotation, Assert.Single(rewritten.GetAnnotations(["rewrite"])));
        Assert.Same(diagnostic, Assert.Single(rewritten.GetDiagnostics()));
        Assert.Equal(3, list.SlotCount);
        Assert.Same(token, list[0]);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void GreenSyntaxRewriter_UnchangedTriviaAndNestedListRetainIdentity()
    {
        var trivia = new Syntax.InternalSyntax.SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " ");
        var inner = new Syntax.InternalSyntax.SyntaxList([trivia]);
        var outer = new Syntax.InternalSyntax.SyntaxList([inner]);
        Assert.Same(outer, new RecordingGreenSyntaxRewriter().VisitList(outer));
    }

    private sealed class ReplacingGreenSyntaxRewriter(GreenSyntaxToken replacement) : GreenSyntaxRewriter
    {
        public override GreenSyntaxToken VisitToken(GreenSyntaxToken token) => replacement;
        public override GreenSyntaxNode? VisitLiteralExpression(Syntax.InternalSyntax.LiteralExpressionSyntax node) => null;
    }

    private sealed class RecordingBoundVisitor : BoundTreeVisitor
    {
        public bool SawLiteralExpression { get; private set; }

        public bool SawReturnStatement { get; private set; }

        public override void VisitLiteralExpression(BoundLiteralExpression node)
        {
            SawLiteralExpression = true;
        }

        public override void VisitReturnStatement(BoundReturnStatement node)
        {
            SawReturnStatement = true;
        }
    }

    private sealed class RecordingBoundRewriter : BoundTreeRewriter
    {
        public bool SawDeclarationPattern { get; private set; }

        public override BoundNode? VisitDeclarationPattern(BoundDeclarationPattern node)
        {
            SawDeclarationPattern = true;
            return node;
        }
    }

    private sealed class RecordingBoundTreeWalker : BoundTreeWalker
    {
        public bool SawDefaultValueExpression { get; private set; }

        public bool SawReturnStatement { get; private set; }

        public override void VisitDefaultValueExpression(BoundDefaultValueExpression node)
        {
            SawDefaultValueExpression = true;
        }

        public override void VisitReturnStatement(BoundReturnStatement node)
        {
            SawReturnStatement = true;
            base.VisitReturnStatement(node);
        }
    }

    private sealed class RecordingSyntaxVisitor : SyntaxVisitor
    {
        public bool SawBinaryExpression { get; private set; }

        public override void VisitInfixOperatorExpression(InfixOperatorExpressionSyntax node)
        {
            SawBinaryExpression = true;
        }
    }

    private sealed class RecordingSyntaxRewriter : SyntaxRewriter
    {
        public bool SawIfStatement { get; private set; }

        public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
        {
            SawIfStatement = true;
            return node;
        }
    }

    private sealed class RecordingGreenSyntaxRewriter : GreenSyntaxRewriter
    {
        public int TokenCount { get; private set; }

        public override GreenSyntaxToken VisitToken(GreenSyntaxToken token)
        {
            TokenCount++;
            return token;
        }
    }

    private sealed class RemovingSyntaxRewriter : SyntaxRewriter
    {
        public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
        {
            return null;
        }
    }
}
