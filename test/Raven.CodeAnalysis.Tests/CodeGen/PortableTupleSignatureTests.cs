using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableTupleSignatureTests
{
    [Fact]
    [Trait("Target", "Shared")]
    public void NominalTupleElementsKeepIdentityAndRequireTheirCapabilities()
    {
        var tree = SyntaxTree.ParseText("""
            public class Cell {}
            public struct Number {}
            public static class Consumer {
                static func Echo(value: (cell: Cell, nested: (number: Number, alias: Cell))) -> (cell: Cell, nested: (number: Number, alias: Cell)) {
                    return value
                }
            }
            """);
        var compilation = Compilation.Create("NominalTupleSignature", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        EmissionCapabilities Capabilities(bool tuples, bool classes) => new(
            Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>().Where(i => tuples || i != LinearInstructionKind.NewTuple),
            Enum.GetValues<EmissionDeclarationKind>().Where(d => classes || d != EmissionDeclarationKind.RootClass),
            Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassSignatures: classes);
        var capabilities = Capabilities(true, true);
        Assert.True(CallableSignature.TryCreate(method, out var signature, capabilities));
        Assert.Same(method.ReturnType, signature.ReturnType.Nominal);
        Assert.Same(method.Parameters[0].Type, signature.ParameterTypes[0].Nominal);
        Assert.True(capabilities.Allows(signature.ReturnType));
        Assert.False(CallableSignature.TryCreate(method, out _, Capabilities(false, true)));
        Assert.False(CallableSignature.TryCreate(method, out _, Capabilities(true, false)));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void NominalTupleForwardingPreservesReferenceIdentityAndValueContents()
    {
        var tree = SyntaxTree.ParseText("""
            public class Cell {
                field Number: int
                init(number: int) { self.Number = number }
            }
            public struct Number {
                field Value: int
                init(value: int) { self.Value = value }
            }
            public static class Consumer {
                static func Echo(value: (Cell, Number)) -> (Cell, Number) { return value }
                static func Run() -> int {
                    let original = (Cell(42), Number(17))
                    let forwarded = Echo(original)
                    if forwarded.Item1 != original.Item1 { return 0 }
                    if forwarded.Item2.Value != 17 { return 0 }
                    return forwarded.Item1.Number
                }
            }
            """);
        var compilation = Compilation.Create("NominalTupleForwarding", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
    }
}
