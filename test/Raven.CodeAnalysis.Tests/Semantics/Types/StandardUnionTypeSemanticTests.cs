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
    public void UnionTypeSyntax_DoesNotFallBackToLegacyTypeUnionSemantics()
    {
        const string source = "func accept(value: int | string) -> () { }";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: TestMetadataReferences.Default);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, diagnostic.Descriptor);

        var model = compilation.GetSemanticModel(tree);
        var parameter = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        Assert.Null(model.GetTypeInfo(parameter.TypeAnnotation!.Type).Type);
    }

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
                let value: int | string = 42
            }
        }
        """;

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, CreateUnionReference()]);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void UnionTypeSyntax_ImplicitlyConvertsAlternativeInAllTargetTypedContexts()
    {
        const string source = """
        import System.*

        func consume(value: int | string) -> () { }

        func create() -> (int | string) {
            let assigned: int | string = 41
            consume(assigned)
            consume(42)
            return 43
        }
        """;

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithRavenCore);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GetTypeInfo_ReportsStandardUnionCarrierConversionRegardlessOfQueryOrder(bool diagnosticsFirst)
    {
        const string source = """
        import System.*

        func consume(value: int | string) -> () { }

        func create() -> (int | string) {
            let assigned: int | string = 41
            consume(42)
            return 43
        }
        """;

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithRavenCore);

        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var literals = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().ToArray();

        Assert.Collection(literals, AssertCarrierConversion, AssertCarrierConversion, AssertCarrierConversion);
        Assert.Empty(compilation.GetDiagnostics());

        void AssertCarrierConversion(LiteralExpressionSyntax literal)
        {
            var typeInfo = model.GetTypeInfo(literal);

            Assert.Equal(SpecialType.System_Int32, typeInfo.Type?.SpecialType);
            var convertedType = Assert.IsAssignableFrom<INamedTypeSymbol>(typeInfo.ConvertedType);
            Assert.Equal("System.Union`2", convertedType.OriginalDefinition.ToFullyQualifiedMetadataName());
            Assert.True(typeInfo.Conversion.Exists);
            Assert.True(typeInfo.Conversion.IsImplicit);
            Assert.True(typeInfo.Conversion.IsUnion);
            Assert.NotNull(typeInfo.Conversion.ConstructorSymbol);
        }
    }

    [Fact]
    public void UnionTypeSyntax_DoesNotConvertNullLiteralToNullableContentUnion()
    {
        const string source = """
        import System.*

        class C {
            static func M() -> () {
                let value: Union<int, string?> = null
            }
        }
        """;

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, CreateUnionReference()]);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
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
