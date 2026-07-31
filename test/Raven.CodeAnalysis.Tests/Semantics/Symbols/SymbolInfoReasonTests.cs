using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SymbolInfoReasonTests : CompilationTestBase
{
    [Theory]
    [InlineData((int)BoundExpressionReason.None, CandidateReason.None)]
    [InlineData((int)BoundExpressionReason.NotFound, CandidateReason.NotFound)]
    [InlineData((int)BoundExpressionReason.OverloadResolutionFailed, CandidateReason.OverloadResolutionFailure)]
    [InlineData((int)BoundExpressionReason.Ambiguous, CandidateReason.Ambiguous)]
    [InlineData((int)BoundExpressionReason.Inaccessible, CandidateReason.Inaccessible)]
    [InlineData((int)BoundExpressionReason.WrongArity, CandidateReason.WrongArity)]
    [InlineData((int)BoundExpressionReason.TypeMismatch, CandidateReason.None)]
    [InlineData((int)BoundExpressionReason.MissingType, CandidateReason.NotFound)]
    [InlineData((int)BoundExpressionReason.ConstantExpected, CandidateReason.None)]
    [InlineData((int)BoundExpressionReason.UnsupportedOperation, CandidateReason.None)]
    [InlineData((int)BoundExpressionReason.OtherError, CandidateReason.None)]
    [InlineData((int)BoundExpressionReason.ArgumentBindingFailed, CandidateReason.OverloadResolutionFailure)]
    public void GetSymbolInfo_ConvertsReason(int boundReasonValue, CandidateReason candidateReason)
    {
        var boundReason = (BoundExpressionReason)boundReasonValue;
        var compilation = CreateCompilation();
        var error = new BoundErrorExpression(compilation.ErrorTypeSymbol, null, boundReason);

        var info = error.GetSymbolInfo();

        Assert.Equal(candidateReason, info.CandidateReason);
    }

    [Fact]
    public void GetSymbolInfo_ForStatementWithoutSymbol_ReturnsNone()
    {
        var tree = SyntaxTree.ParseText("func Main() { return }");
        var compilation = CreateCompilation(tree);
        var statement = tree.GetRoot().DescendantNodes().OfType<ReturnStatementSyntax>().Single();

        var info = compilation.GetSemanticModel(tree).GetSymbolInfo(statement);

        Assert.Null(info.Symbol);
        Assert.Empty(info.CandidateSymbols);
        Assert.Equal(CandidateReason.None, info.CandidateReason);
    }
}
