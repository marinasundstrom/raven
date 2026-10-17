namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SourceMetadataNameTests : CompilationTestBase
{
    [Theory]
    [InlineData("class Box<T> {}\nclass Box {}", "Box")]
    [InlineData("class Owner { class Box<T> {}\nclass Box {} }", "Owner+Box")]
    public void NongenericLookupDoesNotSelectGenericDeclaration(string source, string name)
    {
        var (compilation, _) = CreateCompilation(source);
        var plain = compilation.GetTypeByMetadataName(name);
        var generic = compilation.GetTypeByMetadataName(name + "`1");
        Assert.NotNull(plain);
        Assert.NotNull(generic);
        Assert.Equal(0, plain!.Arity);
        Assert.Equal(1, generic!.Arity);
        Assert.NotSame(plain, generic);
    }

    [Theory]
    [InlineData("class Box<T> {}", "Box")]
    [InlineData("class Owner { class Box<T> {} }", "Owner+Box")]
    public void NongenericLookupDoesNotFallBackToGenericDeclaration(string source, string name)
    {
        var (compilation, _) = CreateCompilation(source);
        Assert.Null(compilation.GetTypeByMetadataName(name));
        Assert.NotNull(compilation.GetTypeByMetadataName(name + "`1"));
    }
}
