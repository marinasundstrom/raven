using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PropagationExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void OptionPropagation_InOptionReturningFunction_Binds()
    {
        var code = """
union Option<T> {
    Some(T)
    None
}

func test() -> Option<int> {
    val r = test2()?
    return .Some(r)
}

func test2() -> Option<int> {
    return .None
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        _ = result.Compilation.GetSemanticModel(tree);
        _ = tree.GetRoot().DescendantNodes().OfType<PropagateExpressionSyntax>().Single();

        verifier.Verify();
    }

    [Fact]
    public void OptionPropagation_RequiresOptionReturnType()
    {
        var code = """
union Result<T, E> {
    Ok(T)
    Error(E)
}

union Option<T> {
    Some(T)
    None
}

func test() -> Result<int, string> {
    val r = test2()?
    return .Ok(r)
}

func test2() -> Option<int> {
    return .None
}
""";

        var result = CreateVerifier(code).GetResult();
        var diagnostics = result.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic =>
                diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandOfType ||
                diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void ResultPropagation_ErrorOperand_DoesNotReportOperatorCannotBeApplied()
    {
        var code = """
async func test() {
    val greeting = await BuildGreeting()
    val value = greeting?
}

func BuildGreeting() -> Task<Result<int, string>> {
    return .Ok(1)
}
""";

        var result = CreateVerifier(code).GetResult();
        var diagnostics = result.Compilation.GetDiagnostics();
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandOfType);
    }
}
