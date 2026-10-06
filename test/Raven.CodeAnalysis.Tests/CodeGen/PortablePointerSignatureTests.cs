using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortablePointerSignatureTests
{
    [Fact]
    public void PointerArgumentLocalAndReturnLowerThroughSemanticOperands()
    {
        var tree = SyntaxTree.ParseText("""
            public static class Pointers {
                public static unsafe func Echo(pointer: *System.Void) -> *System.Void {
                    let copy = pointer
                    return copy
                }
            }
            """);
        var compilation = Compilation.Create("PointerLocals", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsUnmanagedPointers: true);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }

    [Fact]
    public void PointerSignaturesRequireExplicitCapabilityAndSupportedTargets()
    {
        var compilation = Compilation.Create("Pointers", [], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), [], allowsUnmanagedPointers: true);
        foreach (var kind in new[] { SpecialType.System_Void, SpecialType.System_Byte, SpecialType.System_Int32, SpecialType.System_UIntPtr })
        {
            var pointer = compilation.CreatePointerTypeSymbol(compilation.GetSpecialType(kind));
            Assert.False(CallableSignature.TryType(pointer, false, out _));
            Assert.True(CallableSignature.TryType(pointer, false, out var signature, capabilities));
            Assert.Same(pointer, signature.Pointer);
            Assert.True(capabilities.Allows(signature));
            Assert.False(CallableSignature.TryType(compilation.CreateArrayTypeSymbol(pointer), false, out _, capabilities));
        }
        var managed = compilation.CreatePointerTypeSymbol(compilation.GetSpecialType(SpecialType.System_String));
        Assert.False(CallableSignature.TryType(managed, false, out _, capabilities));
    }
}
