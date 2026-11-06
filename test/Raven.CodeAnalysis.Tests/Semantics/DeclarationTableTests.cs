using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class DeclarationTableTests
{
    [Fact]
    public void DeclarationKeysUseRecordEqualityForDictionaryLookup()
    {
        var tree = SyntaxTree.ParseText("class Box<T> {}");
        var locations = System.Collections.Immutable.ImmutableArray.Create(tree.GetRoot().GetLocation());
        var key = new DeclKey(SymbolKind.Type, "Box", 1, null, locations);
        var equivalent = new DeclKey(SymbolKind.Type, "Box", 1, null, locations);
        var distinct = new DeclKey(SymbolKind.Type, "Box", 0, null, locations);
        var entries = new Dictionary<DeclKey, int> { [key] = 42 };
        Assert.True(key == equivalent);
        Assert.True(key != distinct);
        Assert.Equal(42, entries[equivalent]);
        Assert.False(entries.ContainsKey(distinct));
    }

    [Fact]
    public void GenericNamesCarryTheirDeclaredArityAndContainingKeys()
    {
        var tree = SyntaxTree.ParseText("class Outer<T> { class Inner<U,V> { func Run<W>() { let value = 42 } } }");
        var table = new DeclarationTable([tree]);
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        Assert.True(table.TryGetDeclKey(local, out var key));
        Assert.Equal(("value", SymbolKind.Local, 0), (key!.Name, key.Kind, key.Arity));
        Assert.Same(tree, Assert.Single(key.Locations).SourceTree);
        Assert.Equal(("Run", 1), (key.Containing!.Name, key.Containing.Arity));
        Assert.Equal(("Inner", 2), (key.Containing.Containing!.Name, key.Containing.Containing.Arity));
        var outer = key.Containing.Containing.Containing!;
        Assert.Equal(("Outer", 1), (outer.Name, outer.Arity));
        Assert.Null(outer.Containing);
    }

    [Theory]
    [InlineData("record Box<T>(Value: T)", "Box", 1)]
    [InlineData("struct Pair<A,B> {}", "Pair", 2)]
    [InlineData("union Choice<T>(T | int)", "Choice", 1)]
    [InlineData("extension Items<T> for T[] {}", "Items", 1)]
    public void TypeKeysRetainGenericArity(string source, string name, int arity)
    {
        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.Single(((CompilationUnitSyntax)tree.GetRoot()).Members);
        var table = new DeclarationTable([tree]);
        Assert.True(table.TryGetDeclKey(declaration, out var key));
        Assert.Equal(name, key!.Name);
        Assert.Equal(arity, key.Arity);
        Assert.Null(key.Containing);
    }

    [Fact]
    public void LocalKeysExcludeDiscardsAndRetainTheEnclosingMethod()
    {
        var tree = SyntaxTree.ParseText("class Probe { func Run() { let _ = 1; { let value = 42 } } }");
        var table = new DeclarationTable([tree]);
        var declarations = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var discard = Assert.Single(declarations, node => node.Identifier.Text == "_");
        Assert.False(table.TryGetDeclKey(discard, out var noKey));
        Assert.Null(noKey);
        var local = Assert.Single(declarations, node => node.Identifier.ValueText == "value");
        Assert.True(table.TryGetDeclKey(local, out var key));
        Assert.Equal("Run", key!.Containing!.Name);
    }

    [Fact]
    public void ConstructorConsumesInputTreesOnlyOnce()
    {
        var tree = SyntaxTree.ParseText("class Item {}");
        var enumerations = 0;
        IEnumerable<SyntaxTree> Trees()
        {
            Assert.Equal(1, ++enumerations);
            yield return tree;
            yield return tree;
        }
        var table = new DeclarationTable(Trees());
        var item = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        Assert.True(table.TryGetDeclKey(item, out var key));
        Assert.Equal("Item", key!.Name);
    }

    [Fact]
    public void OnlyUnchangedTreesShareDeclarationKeys()
    {
        var stable = SyntaxTree.ParseText("struct Stable {}");
        var before = SyntaxTree.ParseText("class Edited {}");
        var after = SyntaxTree.ParseText("class Edited<T> {}");
        var previous = new DeclarationTable([stable, before]);
        var current = new DeclarationTable([stable, after], previous);
        var stableNode = stable.GetRoot().DescendantNodes().OfType<StructDeclarationSyntax>().Single();
        var oldNode = before.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var newNode = after.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        Assert.True(previous.TryGetDeclKey(stableNode, out var oldStable));
        Assert.True(current.TryGetDeclKey(stableNode, out var newStable));
        Assert.Same(oldStable, newStable);
        Assert.False(current.TryGetDeclKey(oldNode, out _));
        Assert.True(current.TryGetDeclKey(newNode, out var newKey));
        Assert.Equal(1, newKey!.Arity);
        Assert.False(current.TryGetDeclKey(SyntaxFactory.IdentifierName("detached"), out _));
    }
}
