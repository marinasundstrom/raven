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
    [Trait("Target", "DotNet")]
    public void DotNet_NullableReferenceAsCastPreservesNullableResult()
    {
        var (compilation, tree) = CreateCompilation("""
            public class Probe {
                static func Cast(value: object?) -> string? => value as string
            }
            """, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<AsExpressionSyntax>().Single();
        var type = Assert.IsType<NullableTypeSymbol>(compilation.GetSemanticModel(tree).GetTypeInfo(syntax).Type);
        Assert.Equal(SpecialType.System_String, type.UnderlyingType.SpecialType);
    }

    [Fact]
    public void AsCast_ReferenceType_ProducesNullableType()
    {
        var code = """
        let obj: object = ""
        let s = obj as string
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
        let s = 1 as string
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id).WithAnySpan().WithArguments("int", "string")
        ]);
        verifier.Verify();
    }
}
