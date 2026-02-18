using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ReturnExpressionSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void NullCoalesce_WithReturnExpressionInResultMethod_BindsWithoutDiagnostics()
    {
        var code = """
union MyResult {
    Ok(value: int)
    Error(message: string)
}

func Foo(name: string?) -> MyResult {
    val required = name ?? return .Error("Missing name")
    return .Ok(required.Length)
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(v => v.Identifier.Text == "required");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(variable));
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void NullCoalesce_WithBlockEarlyReturnInResultMethod_PreservesLeftType()
    {
        var code = """
union MyResult {
    Ok(value: string)
    Error(message: string)
}

func Foo(name: string?) -> MyResult {
    val required = name ?? {
        return .Error("Missing name")
    }

    .Ok(required)
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(v => v.Identifier.Text == "required");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(variable));
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);

        verifier.Verify();
    }
}
