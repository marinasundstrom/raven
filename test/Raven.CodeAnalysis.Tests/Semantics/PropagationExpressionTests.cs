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
func test() -> Option<int> {
    val r = test2()?
    return r
}

func test2() -> Option<int> {
    return .None
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var propagate = tree.GetRoot().DescendantNodes().OfType<PropagateExpressionSyntax>().Single();
        var type = model.GetTypeInfo(propagate).Type;

        Assert.Equal(SpecialType.System_Int32, type?.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void OptionPropagation_RequiresOptionReturnType()
    {
        var code = """
func test() -> Result<int, string> {
    val r = test2()?
    return .Ok(r)
}

func test2() -> Option<int> {
    return .None
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.OperatorCannotBeAppliedToOperandOfType.Id)
                    .WithAnySpan()
                    .WithArguments("?", "Option<int>")
            ]);

        verifier.Verify();
    }
}
