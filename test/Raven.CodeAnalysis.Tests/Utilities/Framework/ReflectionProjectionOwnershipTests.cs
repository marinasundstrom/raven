namespace Raven.CodeAnalysis.Tests;

public class ReflectionProjectionOwnershipTests
{
    [Fact]
    public void ProjectorCanBeRequestedConcurrentlyBeforeTargetSetup()
    {
        var compilation = Compilation.Create("NoReferences", [], [], CompilationOptions.DotNet);
        var projectors = new ReflectionTypeLoader[8];
        Parallel.For(0, projectors.Length, index => projectors[index] = compilation.ReflectionTypeLoader);
        Assert.All(projectors, projector => Assert.Same(projectors[0], projector));
        // Projector allocation must not initialize a target or manufacture host references.
        Assert.Empty(compilation.References);
        Assert.Equal("RAVT004", Assert.Single(compilation.GetDiagnostics()).Id);
    }

    [Fact]
    public void ColdReflectionQueryAndImportedMembersUseTheSameCompilationSymbols()
    {
        var compilation = Compilation.Create("ColdProjection", [], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var list = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetType(typeof(List<int>)));
        Assert.Same(list, compilation.GetType(typeof(List<int>)));
        Assert.Same(compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"), list.OriginalDefinition);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        Assert.Same(intType, list.TypeArguments.Single());
        var indexer = Assert.Single(list.GetMembers("Item").OfType<IPropertySymbol>()
            .Where(property => property.DeclaredAccessibility == Accessibility.Public));
        Assert.Same(intType, indexer.Type);
        Assert.Same(intType, Assert.Single(indexer.Parameters).Type);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
