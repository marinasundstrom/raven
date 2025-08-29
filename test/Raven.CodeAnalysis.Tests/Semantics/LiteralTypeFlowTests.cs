using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class LiteralTypeFlowTests : DiagnosticTestBase
{
    [Fact]
    public void VariableDeclaration_WithLiteral_InferredUnderlyingType()
    {
        var code = "var i = 0";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    [Fact]
    public void LetDeclaration_WithLiteral_InferredUnderlyingType()
    {
        var code = "let i = 0";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    [Fact]
    public void VariableDeclaration_WithFloatSuffix_InferredFloat()
    {
        var code = "var f = 3.14f"; // explicit float literal
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Single, local.Type.SpecialType);
    }

    [Fact]
    public void VariableDeclaration_WithDoubleLiteral_InferredDouble()
    {
        var code = "var d = 3.14"; // default double
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;

        Assert.Equal(SpecialType.System_Double, local.Type.SpecialType);
    }

    [Fact]
    public void Literal_ImplicitlyConvertsToUnderlyingType()
    {
        var code = "let x: bool = true";
        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void IfExpression_InferredLiteralUnion()
    {
        var code = """
let x = if true { "true" } else { 1 }
""";
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        var union = Assert.IsAssignableFrom<IUnionTypeSymbol>(local.Type);

        Assert.Contains(union.Types, t => t is LiteralTypeSymbol lt && Equals(lt.ConstantValue, "true"));
        Assert.Contains(union.Types, t => t is LiteralTypeSymbol lt && Equals(lt.ConstantValue, 1));
    }
}
