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
let flag = true
let value = if flag 1 else 2
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
let value = if true {
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
let value = if true {
    42
} else {
    0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void IfPatternExpression_WithElse_BindsPatternAndResult()
    {
        const string source = """
union Maybe {
    case Some(value: int)
    case None
}

let option: Maybe = .Some(42)
let value = if let .Some(x) = option {
    x
} else {
    0
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "if_pattern_expression_bound_shape",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var ifExpression = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfPatternExpressionSyntax>()
            .Single();

        var boundIf = Assert.IsType<BoundIfExpression>(model.GetBoundNode(ifExpression));
        Assert.IsType<BoundIsPatternExpression>(boundIf.Condition);
        boundIf.Type.SpecialType.ShouldBe(SpecialType.System_Int32);

        var designation = ifExpression.Pattern
            .DescendantNodesAndSelf()
            .OfType<SingleVariableDesignationSyntax>()
            .Single();
        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation));
        symbol.Type.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void IfPatternExpression_PatternLocalIsNotVisibleInElse()
    {
        const string code = """
union Maybe {
    case Some(value: int)
    case None
}

let option: Maybe = .Some(42)
let value = if let .Some(x) = option {
    x
} else {
    x
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "if_pattern_expression_else_scope",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext);

        var model = compilation.GetSemanticModel(tree);
        var elseIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<ElseExpressionClauseSyntax>()
            .Single()
            .Expression
            .DescendantNodesAndSelf()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "x");

        model.GetSymbolInfo(elseIdentifier).Symbol.ShouldBeNull();
    }

    [Fact]
    public void IfPatternExpressionWithoutElse_ReportsDiagnostic()
    {
        const string code = """
union Maybe {
    case Some(value: int)
    case None
}

let option: Maybe = .Some(42)
let value = if let .Some(x) = option {
    x
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV1901").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void IfExpressionWithoutBraces_AllowsAssignment()
    {
        const string code = """
let value = if true
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
let input = 2
let result = if input == 2 { 2 } else { false }
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "bool")]);

        verifier.Verify();
    }
}
