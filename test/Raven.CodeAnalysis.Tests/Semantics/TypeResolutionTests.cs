using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeResolutionTests : CompilationTestBase
{
    [Fact]
    public void DefaultExpression_WithExplicitType_UsesType()
    {
        const string code = """
        val a = Foo<int>.Bar()

        class Foo<T> {
            public class Bar {
                val value: T
            }
        }
        """;

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().FirstOrDefault();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        Assert.Equal(SpecialType.System_Int32, type.SpecialType);
    }

    [Fact]
    public void DefaultLiteral_TargetTyped_UsesContextualType()
    {
        const string code = """
        val b = Outer<int>.Inner<string>()

        class Outer<A> {
            public class Inner<B> {
                val value: A
                val b: B
            }
        }
        """;

        var (compilation, tree) = CreateCompilation(code);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().FirstOrDefault();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        Assert.Equal(SpecialType.System_String, type.SpecialType);
    }
}
