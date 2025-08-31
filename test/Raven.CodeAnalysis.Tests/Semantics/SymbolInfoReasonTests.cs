using Raven.CodeAnalysis;
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
}

