using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ExternalSignatureCapabilityTests
{
    [Fact]
    public void ExternalValueSignaturesRequireTheirOwnOptIn()
    {
        var app = Compilation.Create("ValueAdmission", [SyntaxTree.ParseText("""
            import System.*
            public static class Consumer {
                public static func Accept(value: DateTime) -> int => 42
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        var references = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true);
        var values = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsExternalValueSignatures: true);
        Assert.False(CallableSignature.TryCreate(method, out _, references));
        Assert.False(CallableSignature.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
        Assert.True(CallableSignature.TryCreate(method, out var signature, values));
        Assert.True(values.Allows(signature));
        Assert.False(references.Allows(signature));
        using var image = new MemoryStream();
        var result = app.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var context = new AssemblyLoadContext("value-admission", isCollectible: true);
        try
        {
            image.Position = 0;
            var assembly = context.LoadFromStream(image);
            Assert.Equal(42, assembly.GetType("Consumer")!.GetMethod("Accept")!.Invoke(null, [new DateTime(2026, 10, 1)]));
        }
        finally { context.Unload(); }
    }

    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void ExternalSignaturesRequireOptInAndOrdinaryDotNetStillExecutes(OptimizationLevel optimization)
    {
        var library = Compilation.Create("ExternalContracts", [SyntaxTree.ParseText("public class Box<T> { }")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var libraryImage = new MemoryStream();
        var libraryResult = library.Emit(libraryImage);
        Assert.True(libraryResult.Success, string.Join("\n", libraryResult.Diagnostics));
        var reference = MetadataReference.CreateFromImage(libraryImage.ToArray());
        var app = Compilation.Create("ExternalConsumer", [SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Accept(value: Box<int>?) -> int => 42
            }
            """)], TestMetadataReferences.Default.Append(reference).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        var syntax = app.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)app.GetSemanticModel(syntax.SyntaxTree).GetDeclaredSymbol(syntax)!;
        Assert.False(CallableSignature.TryCreate(method, out _));
        Assert.False(CallableSignature.TryCreate(method, out _, ReflectionEmitCapabilities.Shared));
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRootClassSignatures: true, allowsGenericClassOwners: true, allowsExternalReferenceSignatures: true);
        Assert.True(CallableSignature.TryCreate(method, out var signature, capabilities));
        Assert.True(capabilities.Allows(signature));
        Assert.False(ReflectionEmitCapabilities.Shared.Allows(signature));
        using var appImage = new MemoryStream();
        var result = app.Emit(appImage);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var context = new AssemblyLoadContext("external-signature-" + optimization, isCollectible: true);
        try
        {
            libraryImage.Position = 0;
            context.LoadFromStream(libraryImage);
            appImage.Position = 0;
            var assembly = context.LoadFromStream(appImage);
            Assert.Equal(42, assembly.GetType("Consumer")!.GetMethod("Accept")!.Invoke(null, [null]));
        }
        finally { context.Unload(); }
    }
}
