using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests.Patterns;

public sealed class PatternBindingRecoveryTests : CompilationTestBase
{
    [Theory]
    [InlineData("comparison")]
    [InlineData("unary")]
    [InlineData("binary")]
    public void UnsupportedSyntheticPatternKind_ProducesDiagnosticInsteadOfThrowing(string patternForm)
    {
        var source = patternForm switch
        {
            "comparison" => "func Test(value: int) { if value is == 1 { } }",
            "unary" => "func Test(value: int) { if value is not 1 { } }",
            "binary" => "func Test(value: int) { if value is 1 and 1 { } }",
            _ => throw new ArgumentOutOfRangeException(nameof(patternForm)),
        };
        var originalTree = SyntaxTree.ParseText(source);
        var originalRoot = originalTree.GetRoot();
        var originalPattern = originalRoot.DescendantNodes()
            .OfType<PatternSyntax>()
            .First(pattern => pattern is ComparisonPatternSyntax or UnaryPatternSyntax or BinaryPatternSyntax);
        PatternSyntax unsupportedPattern = originalPattern switch
        {
            ComparisonPatternSyntax pattern => SyntaxFactory.ComparisonPattern(
                SyntaxKind.AddExpression,
                pattern.OperatorToken,
                pattern.Expression),
            UnaryPatternSyntax pattern => SyntaxFactory.UnaryPattern(
                SyntaxKind.AddExpression,
                pattern.OperatorToken,
                pattern.Pattern),
            BinaryPatternSyntax pattern => SyntaxFactory.BinaryPattern(
                SyntaxKind.AddExpression,
                pattern.Left,
                pattern.OperatorToken,
                pattern.Right),
            _ => throw new InvalidOperationException(),
        };
        var tree = SyntaxTree.Create(
            (CompilationUnitSyntax)originalRoot.ReplaceNode(originalPattern, unsupportedPattern));
        var compilation = Compilation.Create(
                "pattern_recovery",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.InvalidExpressionTerm);
    }
}
