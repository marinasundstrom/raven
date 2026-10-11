using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableIndexValueTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void IndexAndRangeValuesUseAdmittedLibraryCalls(bool constructors)
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            public static class Slices {
                public static func Run() -> int {
                    let values: int[] = [10, 20, 30, 40]
                    let last: Index = ^1
                    let middle = values[1..^1]
                    let prefix = values[..2]
                    let suffix = values[2..]
                    let all = values[..]
                    return values[last] + values[^2] + middle.Length + prefix.Length + suffix.Length + all.Length
                }
            }
            """);
        var compilation = Compilation.Create("PortableIndexValues", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsArrays: true, allowsGenericMethods: true, allowsManagedReferences: true,
            allowsExternalValueSignatures: true, allowsExternalValueInstanceCalls: true, allowsExternalConstructors: constructors);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        var lowered = plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities);
        Assert.True(lowered == constructors, failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(80, loaded.Assembly.GetType("Slices")!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
    }
}
