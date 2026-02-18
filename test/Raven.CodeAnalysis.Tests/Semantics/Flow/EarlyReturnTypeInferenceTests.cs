using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class EarlyReturnTypeInferenceTests
{
    [Fact]
    public void ReturnTypeCollector_InfersUnionFromEarlyReturns()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockStatement)model.GetBoundNode(method.Body!)!;

        var inferred = ReturnTypeCollector.Infer(boundBody);

        Assert.NotNull(inferred);
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(inferred);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);
    }

    [Fact]
    public void ReturnTypeCollector_InfersUnionFromImplicitFinalExpression()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        if flag {
            return 42
        }
        ()
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockStatement)model.GetBoundNode(method.Body!)!;

        var inferred = ReturnTypeCollector.Infer(boundBody);

        Assert.NotNull(inferred);
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(inferred);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);
    }

    [Fact]
    public void ReturnTypeCollector_IgnoresNestedStatementBlockTailExpressions()
    {
        var code = """
class Foo {
    Test(flag: bool) -> {
        if flag {
            0
        }
        return "ok"
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockStatement)model.GetBoundNode(method.Body!)!;

        var inferred = ReturnTypeCollector.Infer(boundBody);

        Assert.NotNull(inferred);
        Assert.Equal(SpecialType.System_String, inferred.SpecialType);
    }

    [Fact]
    public void ReturnTypeCollector_InfersUnionFromIfExpressionReturnedValue()
    {
        var code = """
class Foo {
    Test(flag: bool) {
        return if flag {
            1
        } else {
            "no"
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockStatement)model.GetBoundNode(method.Body!)!;

        var inferred = ReturnTypeCollector.Infer(boundBody);

        Assert.NotNull(inferred);
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(inferred);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_String);
    }

    [Fact]
    public void ReturnTypeCollector_InfersFromReturnedBlockExpressionValue()
    {
        var code = """
class Foo {
    Test(flag: bool) {
        return {
            val x = if flag {
                1
            } else {
                2
            }
            x
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var boundBody = (BoundBlockStatement)model.GetBoundNode(method.Body!)!;

        var inferred = ReturnTypeCollector.Infer(boundBody);

        Assert.NotNull(inferred);
        Assert.Equal(SpecialType.System_Int32, inferred.SpecialType);
    }
}
