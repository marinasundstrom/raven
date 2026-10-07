using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SourceUnitContractTests
{
    private static Compilation Create(string owner = "UnitLibrary", string declaration = "public struct Void { }")
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net10.0"));
        using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"));
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        using var image = new MemoryStream();
        core.Write(image);
        var references = paths.Select(MetadataReference.CreateFromFile).Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray();
        var source = SyntaxTree.ParseText("""
            namespace System
            DECLARATION
            public static class Pointers {
                static func Complete() -> System.Void { }
                static unsafe func Echo(pointer: *System.Void) -> *System.Void => pointer
            }
            """.Replace("DECLARATION", declaration));
        return Compilation.Create("UnitLibrary", [source], references,
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null)
                .WithRuntimeUnitContract(new(owner, "System.Void")));
    }

    [Fact]
    public void SourceUnitOwnsStorageButItsPointerRemainsCliVoid()
    {
        var compilation = Create();
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var unit = Assert.IsType<UnitTypeSymbol>(compilation.GetSpecialType(SpecialType.System_Unit));
        Assert.Same(compilation.Assembly.GetTypeByMetadataName("System.Void"), unit.RuntimeRepresentation);
        var method = compilation.Assembly.GetTypeByMetadataName("System.Pointers")!.GetMembers("Echo").OfType<IMethodSymbol>().Single();
        var pointer = Assert.IsAssignableFrom<IPointerTypeSymbol>(method.ReturnType);
        Assert.Equal(SpecialType.System_Void, pointer.PointedAtType.SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(pointer, method.Parameters[0].Type));
        Assert.True(CallableSignature.IsSupportedPointer(pointer));
        var complete = compilation.Assembly.GetTypeByMetadataName("System.Pointers")!.GetMembers("Complete").OfType<IMethodSymbol>().Single();
        Assert.Equal(SpecialType.System_Unit, complete.ReturnType.SpecialType);
        Assert.True(EmissionPrimitiveTypes.TryGetReturnType(complete.ReturnType, out var result));
        Assert.Equal(EmissionPrimitiveType.NoResult, result);
    }

    [Fact]
    public void SameNamedUnselectedSourceTypeIsNotAnUntypedPointer()
    {
        var compilation = Create("NeoCLR.CoreProbe");
        _ = compilation.GetDiagnostics();
        var method = compilation.Assembly.GetTypeByMetadataName("System.Pointers")!.GetMembers("Echo").OfType<IMethodSymbol>().Single();
        var pointer = Assert.IsAssignableFrom<IPointerTypeSymbol>(method.ReturnType);
        Assert.Same(compilation.Assembly.GetTypeByMetadataName("System.Void"), pointer.PointedAtType);
        Assert.False(CallableSignature.IsSupportedPointer(pointer));
    }

    [Theory]
    [InlineData("Missing", "public struct Void { }")]
    [InlineData("UnitLibrary", "internal struct Void { }")]
    [InlineData("UnitLibrary", "public struct Void { public field Value: int }")]
    public void InvalidSourceOwnerOrStorageIsADiagnostic(string owner, string declaration)
    {
        var compilation = Create(owner, declaration);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003");
    }
}
