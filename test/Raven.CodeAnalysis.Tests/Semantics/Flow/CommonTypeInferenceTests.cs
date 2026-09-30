using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class CommonTypeInferenceTests : DiagnosticTestBase
{
    [Theory]
    [InlineData("Left", "Right", "Base")]
    [InlineData("Right", "Left", "Base")]
    [InlineData("Left", "Other", "Shared")]
    [InlineData("Other", "Left", "Shared")]
    [InlineData("Left", "Unrelated", "System.Object")]
    [InlineData("Unrelated", "Left", "System.Object")]
    [InlineData("System.ArgumentException", "System.InvalidOperationException", "System.SystemException")]
    [InlineData("System.InvalidOperationException", "System.ArgumentException", "System.SystemException")]
    public void NominalInferenceUsesSemanticBaseAndInterfaceContracts(string leftName, string rightName, string expectedName)
    {
        var tree = SyntaxTree.ParseText("""
            interface Shared { }
            open class Base { }
            class Left : Base, Shared { }
            class Right : Base, Shared { }
            class Other : Shared { }
            class Unrelated { }
            """);
        var compilation = Compilation.Create("NominalInference", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var left = compilation.GetTypeByMetadataName(leftName)!;
        var right = compilation.GetTypeByMetadataName(rightName)!;
        var expected = compilation.GetTypeByMetadataName(expectedName)!;
        Assert.NotNull(left);
        Assert.NotNull(right);
        Assert.NotNull(expected);

        var inferred = TypeSymbolNormalization.GetBestCommonType([left, right]);

        Assert.True(SymbolEqualityComparer.Default.Equals(expected, inferred),
            $"Expected {expectedName}, got {inferred.ToDisplayString()}.");
    }

    [Fact]
    public void MatchExpression_WithAbruptArms_InfersNonAbruptValueType()
    {
        const string code = """
import System.*

func Test(y: int) -> int {
    let r = match y {
        0 => return 0
        1 => 42
        _ => throw Exception("x")
    }

    return r + 1
}
""";

        var verifier = CreateVerifier(code, disabledDiagnostics: ["RAV9013"]);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "r");
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(local)!;

        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
        verifier.Verify();
    }

    [Fact]
    public void IfExpression_WithDistinctReferenceBranches_InfersCommonBaseType()
    {
        const string code = """
class A {}
class B {}

let value = if true { A() } else { B() }
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "value");
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(local)!;

        Assert.Equal(SpecialType.System_Object, symbol.Type.SpecialType);
        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithHomogeneousValueArms_InfersConcreteType()
    {
        const string code = """
let seed = 0
let value = match 1 {
    1 => seed
    _ => 42
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(locals[1])!;

        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
        verifier.Verify();
    }
}
