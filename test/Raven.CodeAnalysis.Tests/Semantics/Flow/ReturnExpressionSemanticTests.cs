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
    case Ok(value: int)
    case Error(message: string)
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
    case Ok(value: string)
    case Error(message: string)
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

    [Fact]
    public void NullableReference_PostfixNullForgiving_NarrowsToUnderlyingType()
    {
        var code = """
func Foo(name: string?) -> int {
    val required = name!
    return required.Length
}
""";

        var verifier = CreateVerifier(code, disabledDiagnostics: ["RAV9012", "RAV0403"]);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(v => v.Identifier.Text == "required");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(variable));
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);
        Assert.False(local.Type.IsNullable);

        verifier.Verify();
    }

    [Fact]
    public void NullableValue_PostfixNullForgiving_NarrowsToUnderlyingType()
    {
        var code = """
func Foo(value: int?) -> int {
    val required = value!
    return required + 1
}
""";

        var verifier = CreateVerifier(code, disabledDiagnostics: ["RAV9012", "RAV0403"]);
        var result = verifier.GetResult();

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(v => v.Identifier.Text == "required");

        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(variable));
        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
        Assert.False(local.Type.IsNullable);

        verifier.Verify();
    }
}
