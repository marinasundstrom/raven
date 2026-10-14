using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class StructuralConstraintTests
{
    private const string Types = """
        interface A {}
        interface B {}
        class OnlyA : A {}
        class OnlyB : B {}
        class Both : A, B {}
        class Neither {}
        class Response<T> {}
        """;

    private static (Compilation Compilation, SemanticModel Model, SyntaxTree Tree) Create(string source)
    {
        var tree = SyntaxTree.ParseText(Types + "\n" + source);
        var compilation = Compilation.Create("constraints", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR)
                .WithMetadataImportOptions(new MetadataImportOptions().WithStructuralCompositions()));
        return (compilation, compilation.GetSemanticModel(tree), tree);
    }

    [Theory]
    [InlineData("OnlyA", true)]
    [InlineData("OnlyB", true)]
    [InlineData("Both", true)]
    [InlineData("Neither", false)]
    [InlineData("OnlyA | OnlyB", true)]
    [InlineData("OnlyA | Neither", false)]
    public void UnionConstraint_RequiresCompleteArgumentCoverage(string argument, bool valid)
    {
        var (_, model, tree) = Create($"class Restricted<T: A | B> {{}}\nfunc Use(value: Restricted<{argument}>) -> int {{ return 42 }}");
        var constraint = tree.GetRoot().DescendantNodes().OfType<TypeConstraintSyntax>().Single();
        Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(model.GetTypeInfo(constraint.Type).Type);
        var diagnostics = model.GetDiagnostics();
        Assert.Equal(!valid, diagnostics.Any(d => d.Id == "RAV0320"));
    }

    [Theory]
    [InlineData("Both", true)]
    [InlineData("OnlyA", false)]
    [InlineData("OnlyA | OnlyB", false)]
    public void IntersectionConstraint_AllBoundsMustHold(string argument, bool valid)
    {
        var (_, model, _) = Create($"class Restricted<T: A & B> {{}}\nfunc Use(value: Restricted<{argument}>) -> int {{ return 42 }}");
        Assert.Equal(!valid, model.GetDiagnostics().Any(d => d.Id == "RAV0320"));
    }

    [Theory]
    [InlineData("OnlyA", true)]
    [InlineData("Neither", false)]
    public void GenericMethod_EnforcesUnionConstraint(string argument, bool valid)
    {
        var (_, model, _) = Create($"func Check<T>() -> int where T: A | B {{ return 42 }}\nfunc Use() -> int {{ return Check<{argument}>() }}");
        Assert.Equal(!valid, model.GetDiagnostics().Any(d => d.Id == "RAV0320"));
    }

    [Theory]
    [InlineData("Response<OnlyA>", true)]
    [InlineData("Response<OnlyB>", false)]
    [InlineData("OnlyB", true)]
    public void StructuralBounds_SubstituteOwnerParameters(string argument, bool valid)
    {
        var (_, model, _) = Create($"class Restricted<U, T: Response<U> | B> {{}}\nfunc Use(value: Restricted<OnlyA, {argument}>) -> int {{ return 42 }}");
        Assert.Equal(!valid, model.GetDiagnostics().Any(d => d.Id == "RAV0320"));
    }

    [Theory]
    [InlineData("Response<OnlyA>", true)]
    [InlineData("Response<OnlyB>", false)]
    [InlineData("OnlyB", true)]
    public void StructuralBounds_SubstituteMethodParameters(string argument, bool valid)
    {
        var (_, model, _) = Create($"func Check<U, T>() -> int where T: Response<U> | B {{ return 42 }}\nfunc Use() -> int {{ return Check<OnlyA, {argument}>() }}");
        Assert.Equal(!valid, model.GetDiagnostics().Any(d => d.Id == "RAV0320"));
    }

    [Fact]
    public void ParameterForwarding_PreservesUnionGuarantee()
    {
        var (compilation, model, tree) = Create("class Restricted<T: A | B> {}\nclass Broad<T: A | Neither> {}");
        _ = model.GetDiagnostics();
        var required = compilation.GetTypeByMetadataName("Restricted`1")!.TypeParameters[0];
        var broad = compilation.GetTypeByMetadataName("Broad`1")!.TypeParameters[0];
        var bound = Assert.Single(required.ConstraintTypes);
        Assert.True(SemanticFacts.SatisfiesTypeConstraint(required, bound));
        Assert.False(SemanticFacts.SatisfiesTypeConstraint(broad, bound));
    }
}
