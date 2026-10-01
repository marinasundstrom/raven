using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedTypeConstraintTests
{
    private const string Source = """
        class Payload { var Number: int = 42 }
        class Restricted<Element> where Element: Payload {
            private var stored: Element
            init(value: Element) { stored = value }
            val Value: Element => stored
        }
        func Main() -> int => Restricted<Payload>(Payload()).Value.Number
        """;

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void NominalOwnerBoundsSurviveSharedPlanning(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText(Source);
        var compilation = Compilation.Create("NominalTypeBounds", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.ValueText == "Restricted");
        var owner = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceTypePlan.TryCreate(owner, out _, ReflectionEmitCapabilities.Shared));
        var noBounds = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
            [Accessibility.Public], [Accessibility.Internal], allowsRootClassSignatures: true, allowsGenericClassOwners: true);
        Assert.False(SourceTypePlan.TryCreate(owner, out _, noBounds));
        using var image = new MemoryStream(); var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var loaded = Assembly.Load(image.ToArray());
        Assert.Equal(42, loaded.EntryPoint!.Invoke(null, null));
        var parameter = loaded.GetType("Restricted`1")!.GetGenericArguments().Single();
        Assert.Equal("Payload", parameter.GetGenericParameterConstraints().Single().Name);
    }

    [Fact]
    public void InvalidConcreteTypeArgumentIsRejectedByBinding()
    {
        var tree = SyntaxTree.ParseText(Source.Replace("Restricted<Payload>(Payload())", "Restricted<int>(42)"));
        var compilation = Compilation.Create("InvalidBound", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.Location.IsInSource);
        using var image = new MemoryStream();
        Assert.False(compilation.Emit(image).Success);
        Assert.Equal(0, image.Length);
    }
}
