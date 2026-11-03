using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class ParseContextStateTests
{
    [Fact]
    public void ParenthesisDepth_IsLocalToEachContext()
    {
        var root = new RootContext();
        var child = new ChildContext(root);
        Assert.False(root.IsInsideParens);
        child.EnterParens();
        child.EnterParens();
        child.ExitParens();
        Assert.True(child.IsInsideParens);
        Assert.False(root.IsInsideParens);
        child.ExitParens();
        Assert.False(child.IsInsideParens);
        Assert.Equal(typeof(void), typeof(ParseContext).GetMethod(nameof(ParseContext.EnterParens))!.ReturnType);
    }

    [Fact]
    public void Diagnostics_AreSharedAtRootAndRestoreTruncatesOnlyTheSuffix()
    {
        var root = new RootContext();
        var child = new ChildContext(new ChildContext(root));
        var first = DiagnosticInfo.Create(CompilerDiagnostics.InvalidExpressionTerm, new TextSpan(0, 1), "first");
        var second = DiagnosticInfo.Create(CompilerDiagnostics.InvalidExpressionTerm, new TextSpan(2, 1), "second");
        Assert.Empty(child.Diagnostics);
        child.RestoreDiagnostics(0);
        child.AddDiagnostic(first);
        root.AddDiagnostic(second);
        Assert.Equal(2, child.GetDiagnosticCount());
        Assert.Same(first, root.Diagnostics[0]);
        Assert.Same(second, child.Diagnostics[1]);
        child.RestoreDiagnostics(1);
        Assert.Same(first, Assert.Single(root.Diagnostics));
        child.RestoreDiagnostics(10);
        Assert.Single(root.Diagnostics);
        child.RestoreDiagnostics(-1);
        Assert.Empty(root.Diagnostics);
    }

    [Fact]
    public void Children_DelegateContextPropertiesToRoot()
    {
        var child = new ChildContext(new ChildContext(new RootContext()));
        Assert.Equal(42, child.Position);
        Assert.Equal(2, child.BlockDepth);
        Assert.True(child.IsInBlock);
        Assert.False(child.IsInMacro);
        Assert.Throws<InvalidOperationException>(() => new ChildContext(null).Position);
    }

    private sealed class RootContext : ParseContext
    {
        public override int Position => 42;
        public override int BlockDepth => 2;
    }

    private sealed class ChildContext(ParseContext? parent) : ParseContext(parent);
}
