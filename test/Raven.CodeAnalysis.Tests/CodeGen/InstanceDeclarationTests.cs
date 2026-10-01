using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class InstanceDeclarationTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void RootClassInstanceMethodsKeepReceiverSeparateFromDeclaredParameters(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            public class Calculator {
                public func Add(left: int, right: int) -> int => left + right
                public func Wide(value: long) -> long => value + 1L
                public func Flag(value: bool) -> bool => !value
                public func Text(value: string) -> string => value
            }
            """);
        var compilation = Compilation.Create("InstanceDeclarations", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var owner = (INamedTypeSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single())!;
        Assert.True(SourceTypePlan.TryCreate(owner, out var type, ReflectionEmitCapabilities.Shared));
        Assert.Equal(EmissionDeclarationKind.RootClass, type!.DeclarationKind);
        Assert.False(type.IsStatic);
        Assert.False(SourceTypePlan.TryCreate(owner, out _, new([], [], [EmissionDeclarationKind.StaticType], [Accessibility.Public])));
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            var symbol = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.Equal(EmissionDeclarationKind.InstanceMethod, plan!.DeclarationKind);
            Assert.Same(owner, plan.TypeOwner);
            Assert.Equal(symbol.Parameters.Length, plan.Signature.ParameterCount);
            Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            var denied = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                [EmissionDeclarationKind.StaticMethod], methodVisibilities: [Accessibility.Public]);
            Assert.False(SourceCallablePlan.TryCreate(symbol, out _, denied));
            Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out _, denied));
        }
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var runtimeType = Assembly.Load(image.ToArray()).GetType("Calculator")!;
        var instance = Activator.CreateInstance(runtimeType);
        Assert.Equal(42, runtimeType.GetMethod("Add")!.Invoke(instance, [40, 2]));
        Assert.Equal(5000000001L, runtimeType.GetMethod("Wide")!.Invoke(instance, [5000000000L]));
        Assert.Equal(false, runtimeType.GetMethod("Flag")!.Invoke(instance, [true]));
        Assert.Equal("Hej 🌍", runtimeType.GetMethod("Text")!.Invoke(instance, ["Hej 🌍"]));
    }
}
