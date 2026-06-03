using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
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
    case Some(T)
    case None
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
    case Ok(T)
    case Error(E)
}

union Option<T> {
    case Some(T)
    case None
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

    [Fact]
    public void ResultPropagation_InAsyncResultChain_InfersPayloadTypes()
    {
        var code = """
import System.Threading.Tasks.*

union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

record Input(Text: string)
record Output(Length: int)

async func Run() -> Task<Result<Output, string>> {
    val firstResult = await LoadInput()
    val input = firstResult?
    val output = Transform(input)?
    return .Ok(output)
}

async func LoadInput() -> Task<Result<Input, string>> {
    await Task.Yield()
    return .Ok(Input("value"))
}

func Transform(input: Input) -> Result<Output, string> {
    return .Ok(Output(input.Text.Length))
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var locals = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Where(static declarator => declarator.Identifier.ValueText is "input" or "output")
            .ToDictionary(static declarator => declarator.Identifier.ValueText);
        var propagations = root.DescendantNodes().OfType<PropagateExpressionSyntax>().ToArray();

        Assert.Equal(2, propagations.Length);
        Assert.Equal("Input", model.GetTypeInfo(propagations[0]).Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Output", model.GetTypeInfo(propagations[1]).Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var inputLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["input"]));
        var outputLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["output"]));
        Assert.Equal("Input", inputLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Output", outputLocal.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        verifier.Verify();
    }
}
