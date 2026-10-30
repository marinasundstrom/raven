using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ParameterDefaultCapabilityTests
{
    [Theory]
    [InlineData("int = 42")]
    [InlineData("bool = false")]
    [InlineData("string = \"text\"")]
    public void ScalarDefaultsRequireAnExplicitTargetCapability(string parameter)
    {
        var compilation = Compilation.Create("Defaults", [SyntaxTree.ParseText(
            $"class Api {{ static func Read(value: {parameter}) {{ }} }}")], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var method = compilation.GetTypeByMetadataName("Api")!.GetMembers("Read").OfType<IMethodSymbol>().Single();
        EmissionCapabilities Capabilities(bool defaults) => new(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>(), Enum.GetValues<EmissionDeclarationKind>(), allowsParameterDefaults: defaults);
        Assert.False(CallableSignature.TryCreate(method, out _, Capabilities(false)));
        Assert.True(CallableSignature.TryCreate(method, out var signature, Capabilities(true)));
        Assert.Equal(1, signature.ParameterCount);
        Assert.True(method.Parameters[0].HasExplicitDefaultValue);
    }
}
