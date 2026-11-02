using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MetadataDefaultValueConstructorTests
{
    [Theory]
    [InlineData("System.String()")]
    [InlineData("System.HashCode(1)")]
    public void InvalidMetadataConstruction_StillReportsOverloadFailure(string expression)
    {
        var tree = SyntaxTree.ParseText($"let value = {expression}");
        var compilation = Compilation.Create("InvalidMetadataConstruction", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV1501");
    }

    [Theory]
    [InlineData("System.Collections.Generic.List`1", "Add")]
    [InlineData("System.Collections.Generic.List`1", "Count")]
    [InlineData("System.ValueTuple`1", "Item1")]
    public void StableMetadataMemberProjection_IsCanonicalAcrossLookupPaths(string typeName, string memberName)
    {
        var compilation = Compilation.Create("MetadataProjectionIdentity", [], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var definition = compilation.GetTypeByMetadataName(typeName)!;
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(definition.Construct(compilation.GetSpecialType(SpecialType.System_Int32)));
        var all = type.GetMembers();
        var named = type.GetMembers(memberName);
        Assert.NotEmpty(named);
        Assert.Equal(all.Count(m => m.Name == memberName), named.Length);
        foreach (var member in named)
            Assert.Contains(all, candidate => ReferenceEquals(member, candidate));
        var repeated = type.GetMembers(memberName);
        for (var i = 0; i < named.Length; i++)
            Assert.Same(named[i], repeated[i]);
    }

    [Theory]
    [InlineData("System.HashCode")]
    [InlineData("System.Guid")]
    [InlineData("System.Int32")]
    [InlineData("System.DateTime")]
    [InlineData("System.ValueTuple<int>")]
    public void MetadataValueType_ExposesCanonicalImplicitConstructor(string typeName)
    {
        var tree = SyntaxTree.ParseText($"func Create() -> {typeName} => {typeName}()");
        var compilation = Compilation.Create("MetadataValueDefault", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var constructor = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal(MethodKind.Constructor, constructor.MethodKind);
        Assert.True(constructor.IsImplicitlyDeclared);
        Assert.Empty(constructor.Parameters);
        Assert.Same(constructor, Assert.Single(constructor.ContainingType!.Constructors.Where(c => c.Parameters.IsEmpty)));
        Assert.Same(constructor, Assert.Single(constructor.ContainingType.GetMembers(".ctor").OfType<IMethodSymbol>().Where(c => c.Parameters.IsEmpty)));
        var operation = Assert.IsAssignableFrom<IObjectCreationOperation>(model.GetOperation(invocation));
        Assert.Same(constructor, operation.Constructor);
        Assert.False(operation.IsImplicit);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrMetadataDefaultValueConstructorTests
{
    [Theory]
    [InlineData("System.Int32")]
    [InlineData("System.ValueTuple<int>")]
    public void NeoClrSemanticBindingWithHostReferences_AdmitsImplicitValueConstruction(string typeName)
    {
        var tree = SyntaxTree.ParseText($"func Create() -> {typeName} => {typeName}()");
        var compilation = Compilation.Create("NeoClrMetadataValueDefault", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var constructor = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.True(constructor.IsImplicitlyDeclared);
        Assert.Equal(MethodKind.Constructor, constructor.MethodKind);
    }
}
