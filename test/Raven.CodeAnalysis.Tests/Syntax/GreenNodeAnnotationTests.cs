#if RAVEN_PORT
extern alias Port;
using NodeBase = Port::Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode;
#else
using NodeBase = Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode;
#endif

using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class GreenNodeAnnotationTests
{
    [Fact]
    public void UpdatesAppendAnnotationsPreserveDiagnosticsAndLeaveOriginalUnchanged()
    {
        var diagnostic = DiagnosticInfo.Create(CompilerDiagnostics.CharacterExpected, new TextSpan(0, 0), ')');
        var first = new SyntaxAnnotation("first");
        var second = new SyntaxAnnotation("second");
        GreenNode original = new TestNode([], [diagnostic], [first]);
        var updated = original.WithAdditionalAnnotations(second);

        Assert.Equal(new[] { first, second }, updated.GetAnnotations(new[] { "first", "second" }));
        Assert.Same(diagnostic, Assert.Single(updated.GetDiagnostics()));
        Assert.Same(diagnostic, Assert.Single(updated.GetDiagnosticsRecursive()));
        Assert.Equal(new[] { first }, original.GetAnnotations(new[] { "first", "second" }));
        Assert.Same(updated, updated.WithAdditionalAnnotations());
    }

    [Fact]
    public void FirstAnnotationRetainsChildrenAndDiagnostics()
    {
        var child = SyntaxFactory.Identifier("x").Green;
        var diagnostic = DiagnosticInfo.Create(CompilerDiagnostics.CharacterExpected, new TextSpan(0, 0), ')');
        GreenNode original = new TestNode([child], [diagnostic]);
        var annotation = new SyntaxAnnotation("first");
        var updated = original.WithAdditionalAnnotations(annotation);

        Assert.Same(child, updated.GetSlot(0));
        Assert.Same(diagnostic, Assert.Single(updated.GetDiagnostics()));
        Assert.Same(diagnostic, Assert.Single(updated.GetDiagnosticsRecursive()));
        Assert.Same(annotation, updated.GetAnnotation("first"));
        Assert.Equal(original.FullWidth, updated.FullWidth);
    }

    private sealed class TestNode : NodeBase
    {
        public TestNode(GreenNode?[] children, DiagnosticInfo[]? diagnostics = null, SyntaxAnnotation[]? annotations = null)
            : base(SyntaxKind.IdentifierName, children, diagnostics, annotations) { }

        internal override GreenNode With(GreenNode?[] children, DiagnosticInfo[]? diagnostics = null, SyntaxAnnotation[]? annotations = null)
            => new TestNode(children, diagnostics, annotations);

        internal override void Accept(InternalSyntax.SyntaxVisitor visitor) { }

        internal override TResult Accept<TResult>(InternalSyntax.SyntaxVisitor<TResult> visitor) => default!;
    }
}
