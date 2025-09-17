using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SymbolEqualityComparerTests
{
    [Fact]
    public void DefaultComparer_DistinguishesNullability()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var comparer = SymbolEqualityComparer.Default;
        Assert.False(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<ISymbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.False(dictionary.ContainsKey(nullableInt));
    }

    [Fact]
    public void IgnoringNullabilityComparer_TreatsNullableAndUnderlyingAsEqual()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullableInt = new NullableTypeSymbol(intType, null, null, null, []);

        var comparer = SymbolEqualityComparer.IgnoringNullability;
        Assert.True(comparer.Equals(intType, nullableInt));

        var dictionary = new Dictionary<ISymbol, int>(comparer)
        {
            [intType] = 1,
        };

        Assert.True(dictionary.ContainsKey(nullableInt));
    }

    [Fact]
    public void Comparer_UnwrapsAliasSymbols()
    {
        const string source = """
            alias Text = System.String

            let value: Text = ""
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var identifier = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Last(id => id.Identifier.Text == "Text");

        var symbol = model.GetSymbolInfo(identifier).Symbol;
        var alias = Assert.IsAssignableFrom<IAliasSymbol>(symbol);

        var comparer = SymbolEqualityComparer.Default;
        Assert.True(comparer.Equals(alias, alias.UnderlyingSymbol));
        Assert.Equal(comparer.GetHashCode(alias.UnderlyingSymbol), comparer.GetHashCode(alias));

        var set = new HashSet<ISymbol>(comparer) { alias };
        Assert.Contains(alias.UnderlyingSymbol, set);
    }
}
