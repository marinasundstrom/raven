using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class StandardUnionTypeSemanticTests : CompilationTestBase
{
    [Fact]
    public void UnionTypeSyntax_BindsToRavenCoreUnion()
    {
        const string source = """
        import System.*

        func accept(value: int | string) -> () { }
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, CreateUnionReference()]);

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var parameter = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetTypeInfo(parameter.TypeAnnotation!.Type).Type);

        Assert.Equal("Union", type.Name);
        Assert.Equal("System.Union`2", type.OriginalDefinition.ToFullyQualifiedMetadataName());
        Assert.Collection(
            type.TypeArguments,
            arg => Assert.Equal(SpecialType.System_Int32, arg.SpecialType),
            arg => Assert.Equal(SpecialType.System_String, arg.SpecialType));
        Assert.Equal("int | string", type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void UnionTypeSyntax_BindsFiveAlternatives()
    {
        const string source = """
        import System.*

        func accept(value: int | string | bool | char | decimal) -> () { }
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, CreateUnionReference()]);

        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var parameter = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetTypeInfo(parameter.TypeAnnotation!.Type).Type);

        Assert.Equal("System.Union`5", type.OriginalDefinition.ToFullyQualifiedMetadataName());
        Assert.Equal("int | string | bool | char | decimal", type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void UnionTypeSyntax_ImplicitlyConvertsAlternativeToAnnotatedUnion()
    {
        const string source = """
        import System.*

        class C {
            static func M() -> () {
                val value: int | string = 42
            }
        }
        """;

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, CreateUnionReference()]);

        Assert.Empty(compilation.GetDiagnostics());
    }

    private static MetadataReference CreateUnionReference()
    {
        const string fixtureSource = """
        namespace System

        public union Union<T1, T2>(T1 | T2)

        public union Union<T1, T2, T3, T4, T5>(T1 | T2 | T3 | T4 | T5)
        """;

        return TestMetadataFactory.CreateFromSource(
            fixtureSource,
            assemblyName: $"raven-core-union-fixture-{Guid.NewGuid():N}");
    }
}
