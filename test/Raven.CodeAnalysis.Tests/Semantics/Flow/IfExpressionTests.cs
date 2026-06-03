using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IfExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void IfExpression_WithElse_BindsToBoundIfExpression()
    {
        const string source = """
val flag = true
val value = if flag 1 else 2
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "if_expression_bound_shape",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var ifExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfExpressionSyntax>()
            .Single();

        var boundIf = Assert.IsType<BoundIfExpression>(model.GetBoundNode(ifExpression));
        Assert.IsType<BoundLiteralExpression>(boundIf.ThenBranch);
        Assert.IsType<BoundLiteralExpression>(boundIf.ElseBranch);
    }

    [Fact]
    public void IfExpressionWithoutElse_ReportsDiagnostic()
    {
        const string code = """
val value = if true {
    42
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV1901").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void IfExpressionWithElse_AllowsAssignment()
    {
        const string code = """
val value = if true {
    42
} else {
    0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void IfExpressionWithoutBraces_AllowsAssignment()
    {
        const string code = """
val value = if true
    42
else
    0
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void IfExpressionWithoutTargetType_IncompatibleBranches_ReportsDiagnostic()
    {
        const string code = """
val input = 2
val result = if input == 2 { 2 } else { false }
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "bool")]);

        verifier.Verify();
    }
}
