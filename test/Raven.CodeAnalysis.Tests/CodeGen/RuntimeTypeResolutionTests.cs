using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RuntimeTypeResolutionTests
{
    [Fact]
    public void ResolvesConstructedGenericFromMetadata()
    {
        var compilation = Compilation.Create("GenericResolution", syntaxTrees: [],
            references: TestMetadataReferences.Default,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var definition = compilation.GetTypeByMetadataName("System.Action`1")!;
        var constructed = definition.Construct(compilation.GetSpecialType(SpecialType.System_String));
        var generator = new CodeGenerator(compilation);

        Assert.Equal(typeof(Action<string>), generator.RuntimeSymbolResolver.GetType(constructed));
    }

    [Theory]
    [InlineData(1, false)]
    [InlineData(2, false)]
    [InlineData(1, true)]
    [InlineData(2, true)]
    public void GenericArraysKeepTheirSelectedReflectionContext(int rank, bool forAttribute)
    {
        var compilation = Compilation.Create("GenericArrayResolution", syntaxTrees: [],
            references: TestMetadataReferences.Default,
            options: CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var definition = compilation.GetTypeByMetadataName("System.Action`1")!;
        var constructed = definition.Construct(compilation.GetSpecialType(SpecialType.System_String));
        var array = compilation.CreateArrayTypeSymbol(constructed, rank);
        var generator = new CodeGenerator(compilation, new EmitOptions(compilation.CoreAssembly.GetName()));

        var resolved = generator.RuntimeSymbolResolver.GetType(array,
            usage: forAttribute ? RuntimeTypeUsage.CustomAttribute : RuntimeTypeUsage.Signature);

        Assert.Equal(rank, resolved.GetArrayRank());
        var element = resolved.GetElementType()!;
        Assert.Equal("System.Action`1", element.GetGenericTypeDefinition().FullName);
        var argument = Assert.Single(element.GetGenericArguments());
        Assert.Equal("System.String", argument.FullName);
        var expectedAssembly = forAttribute ? typeof(object).Assembly : compilation.CoreAssembly;
        Assert.Equal(expectedAssembly, element.Assembly);
        Assert.Equal(expectedAssembly, argument.Assembly);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MethodBodyUnitResolutionHonorsVoidPolicy(bool treatUnitAsVoid)
    {
        var compilation = Compilation.Create("UnitResolution", [SyntaxTree.ParseText("class Example { }")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var generator = new CodeGenerator(compilation);
        using var output = new MemoryStream();
        generator.Emit(output, null);
        Assert.NotNull(generator.UnitType);

        var resolved = generator.RuntimeSymbolResolver.GetType(compilation.UnitTypeSymbol,
            treatUnitAsVoid, RuntimeTypeUsage.MethodBody);

        Assert.Equal(treatUnitAsVoid ? typeof(void) : generator.UnitType, resolved);
        var array = compilation.CreateArrayTypeSymbol(compilation.UnitTypeSymbol);
        var resolvedArray = generator.RuntimeSymbolResolver.GetType(array, treatUnitAsVoid, RuntimeTypeUsage.MethodBody);
        Assert.True(resolvedArray.IsArray);
        Assert.Equal(generator.UnitType, resolvedArray.GetElementType());
    }

    [Fact]
    public void AttributeUsageResolvesHostTypesWhileSignatureRetainsTargetMetadata()
    {
        var compilation = Compilation.Create("AttributeResolution", syntaxTrees: [], references: TestMetadataReferences.Default,
            options: CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var definition = compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var symbol = definition.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var generator = new CodeGenerator(compilation, new EmitOptions(compilation.CoreAssembly.GetName()));

        var signature = generator.RuntimeSymbolResolver.GetType(symbol, usage: RuntimeTypeUsage.Signature);
        var attribute = generator.RuntimeSymbolResolver.GetType(symbol, usage: RuntimeTypeUsage.CustomAttribute);

        Assert.NotEqual(typeof(List<int>).Assembly, signature.Assembly);
        Assert.Equal(typeof(List<int>), attribute);
        Assert.Equal(typeof(int), Assert.Single(attribute.GetGenericArguments()));
    }
}
