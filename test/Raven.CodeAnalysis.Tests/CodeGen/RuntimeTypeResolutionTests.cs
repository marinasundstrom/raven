using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RuntimeTypeResolutionTests
{
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
