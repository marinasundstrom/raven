using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class UnderlyingUnionTypeComputationTests : DiagnosticTestBase
{
    [Fact]
    public void InferredUnion_SiblingTypes_ComputesCommonNominalBase()
    {
        const string code = """
open class Animal {}
class Dog: Animal {}
class Cat: Animal {}

val pet = if true { Dog() } else { Cat() }
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "pet");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);

        var baseType = Assert.IsAssignableFrom<INamedTypeSymbol>(union.BaseType);
        Assert.Equal("Animal", baseType.Name);

        verifier.Verify();
    }

    [Fact]
    public void InferredUnion_UnrelatedBranches_ComputesObjectBase()
    {
        const string code = """
val value = if true { 1 } else { "x" }
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "value");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);

        var baseType = Assert.IsAssignableFrom<INamedTypeSymbol>(union.BaseType);
        Assert.Equal(SpecialType.System_Object, baseType.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void LiteralUnion_SameUnderlyingType_ComputesStringBase()
    {
        const string code = """
val mode: "on" | "off" = "on"
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "mode");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);

        var baseType = Assert.IsAssignableFrom<INamedTypeSymbol>(union.BaseType);
        Assert.Equal(SpecialType.System_String, baseType.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void AliasLiteralUnion_ComputesStringBase()
    {
        const string code = """
alias Switch = "on" | "off"
val mode: Switch = "on"
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "mode");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        var type = local.Type;
        if (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol aliasTarget)
            type = aliasTarget;

        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(type);
        var baseType = Assert.IsAssignableFrom<INamedTypeSymbol>(union.BaseType);
        Assert.Equal(SpecialType.System_String, baseType.SpecialType);

        verifier.Verify();
    }
}
