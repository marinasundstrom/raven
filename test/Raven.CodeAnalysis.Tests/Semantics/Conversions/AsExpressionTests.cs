using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AsExpressionTests : CompilationTestBase
{
    [Fact]
    public void AsCast_ReferenceType_ProducesNullableType()
    {
        var code = """
        val obj: object = ""
        val s = obj as string
        """;

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Last();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type;
        var nullable = Assert.IsType<NullableTypeSymbol>(type);
        Assert.Equal(SpecialType.System_String, nullable.UnderlyingType.SpecialType);
    }
}

public class AsExpressionDiagnosticTests : DiagnosticTestBase
{
    [Fact]
    public void AsCast_Invalid_ProducesDiagnostic()
    {
        string code = """
        val s = 1 as string
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id).WithAnySpan().WithArguments("int", "string")
        ]);
        verifier.Verify();
    }
}
