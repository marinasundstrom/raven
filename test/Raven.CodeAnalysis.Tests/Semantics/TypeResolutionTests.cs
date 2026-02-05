using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeResolutionTests : CompilationTestBase
{
    [Fact]
    public void NestedTypeConstruction_SubstitutesOuterTypeParameters()
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
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        var int32 = compilation.GetSpecialType(SpecialType.System_Int32);

        // Definitions
        var fooDef = compilation.GetTypeByMetadataName("Foo`1")!;
        var fooDefBar = fooDef.GetMembers("Bar").OfType<INamedTypeSymbol>().First();
        var barValueDef = fooDefBar.GetMembers("value").OfType<IFieldSymbol>().First();

        // Constructed outer + nested type view under it
        var fooInt = fooDef.Construct(int32);
        var fooIntBar = fooInt.GetMembers("Bar").OfType<INamedTypeSymbol>().First();
        var barValueConstructed = fooIntBar.GetMembers("value").OfType<IFieldSymbol>().First();

        // The initializer type should be the constructed nested type (Bar inside Foo<int>)
        Assert.Equal(fooDefBar, type.OriginalDefinition, SymbolEqualityComparer.Default);
        Assert.Equal(fooInt, type.ContainingType, SymbolEqualityComparer.Default);

        // And the field substitution should have happened: T -> int
        Assert.Equal(fooDef, barValueDef.Type.ContainingType, SymbolEqualityComparer.Default);
        Assert.Equal(int32, barValueConstructed.Type, SymbolEqualityComparer.Default);

        // Sanity: the constructed nested type we computed matches the bound type
        Assert.Equal(fooIntBar, type, SymbolEqualityComparer.Default);
    }

    [Fact]
    public void NestedGenericTypeConstruction_SubstitutesBothOuterAndInnerTypeParameters()
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
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().First();
        var type = model.GetTypeInfo(declarator.Initializer!.Value).Type!;

        var int32 = compilation.GetSpecialType(SpecialType.System_Int32);
        var @string = compilation.GetSpecialType(SpecialType.System_String);

        // Outer definition
        var outerDef = compilation.GetTypeByMetadataName("Outer`1")!;

        // Inner definition (nested metadata name uses '+')
        var innerDef = compilation.GetTypeByMetadataName("Outer`1+Inner`1")!;

        // Construct Outer<int>
        var outerInt = outerDef.Construct(int32);

        // Get Inner<B> as a member of Outer<int>, then construct with string
        var innerInOuterIntDef = outerInt.GetMembers("Inner").OfType<INamedTypeSymbol>().First();
        var innerIntString = innerInOuterIntDef.Construct(@string);

        // Fields on the fully constructed nested type should be substituted
        var valueField = innerIntString.GetMembers("value").OfType<IFieldSymbol>().First();
        var bField = innerIntString.GetMembers("b").OfType<IFieldSymbol>().First();

        Assert.Equal(innerDef, type.OriginalDefinition, SymbolEqualityComparer.Default);
        Assert.Equal(outerInt, type.ContainingType, SymbolEqualityComparer.Default);
        Assert.Equal(innerIntString, type, SymbolEqualityComparer.Default);

        Assert.Equal(int32, valueField.Type, SymbolEqualityComparer.Default);
        Assert.Equal(@string, bField.Type, SymbolEqualityComparer.Default);
    }
}
